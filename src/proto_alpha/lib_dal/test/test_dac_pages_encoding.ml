(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Dal_node Slot_frame_encoding
    Invocation: dune exec src/proto_alpha/lib_dal/test/main.exe \
      -- test "^\[Unit\] Dac_pages_encoding.ml$"
    Subject:    Tests for the SCORU storage module
*)

(** DAC/FIXME: https://gitlab.com/tezos/tezos/-/issues/4021
    Add tests to check actual (sequences of) bytes in serialized pages. *)

open Protocol
open Alpha_context

let lift f = Lwt.map Environment.wrap_tzresult f

(* Tests are run against a mock storage backend where a Hash-indexed/Bytes-valued Map
   is used to simulate adding and retrieving files to a directory.
*)
module Hashes_map = Sc_rollup.Reveal_hash.Map

type hashes_map = bytes Hashes_map.t

module Make_Merkle_tree_V0_backend () = struct
  open Environment.Error_monad

  type error +=
    | Page_already_saved of Sc_rollup.Reveal_hash.t
    | Page_is_missing of Sc_rollup.Reveal_hash.t

  let backend = ref Hashes_map.empty

  let save_page (hash, bytes) =
    let open Lwt_result_syntax in
    match Hashes_map.find hash !backend with
    | None ->
        let () = backend := Hashes_map.add hash bytes !backend in
        return_unit
    | Some old_bytes ->
        if Bytes.equal old_bytes bytes then return_unit
        else tzfail @@ Page_already_saved hash

  let load_page hash =
    let open Lwt_result_syntax in
    let bytes = Hashes_map.find hash !backend in
    match bytes with
    | None -> tzfail @@ Page_is_missing hash
    | Some bytes -> return bytes

  let number_of_pages () = List.length @@ Hashes_map.bindings !backend
end

let assert_equal_bytes ~loc msg =
  Assert.equal ~loc Bytes.equal msg String.pp_bytes_hex

let assert_fails_with ~loc k expected_err =
  let open Lwt_result_syntax in
  let*! res = k in
  let res = Environment.wrap_tzresult res in
  Assert.error ~loc res (( = ) (Environment.wrap_tzerror expected_err))

module Merkle_tree = struct
  module V0 = struct
    open Dac_pages_encoding.Merkle_tree.V0

    (* We use 50 bytes as the size of a page. Of these, 5 bytes are used for
       the preamble, which leaves 45 bytes of space for storing hashes in a
       page. The size of a hash is 32 bytes, therefore only floor(45/2) = 1
       hashes can be stored for each page. Because the serialization process
       requires a page size that can contain at least two hashes, the
       serialization of any content will fail in this case.
    *)

    let serialize_one_hash_per_page_fails () =
      let module Backend = Make_Merkle_tree_V0_backend () in
      let payload =
        List.repeat 195 (Bytes.of_string "a") |> Bytes.concat Bytes.empty
      in
      assert_fails_with
        ~loc:__LOC__
        (serialize_payload
           ~max_page_size:50
           payload
           ~for_each_page:Backend.save_page)
        Dac_pages_encoding.Merkle_tree_branching_factor_not_high_enough

    let serialize_empty_payload_fails () =
      let module Backend = Make_Merkle_tree_V0_backend () in
      (* Limit the number of hashes stored per page to 2. Because hashes
         have a fixed size of 32 bytes, and 5 bytes are used for the preamble,
         we need 32 * 2 + 5 = 69 bytes to store two hashes in a page. We round
         this value to 70. *)
      let max_page_size = 70 in
      let payload = Bytes.empty in

      assert_fails_with
        ~loc:__LOC__
        (serialize_payload
           ~max_page_size
           payload
           ~for_each_page:Backend.save_page)
        Dac_pages_encoding.Payload_cannot_be_empty

    let one_page_roundtrip () =
      let open Lwt_result_syntax in
      let module Backend = Make_Merkle_tree_V0_backend () in
      (* Limit the number of hashes stored per page to 2. Because hashes
         have a fixed size of 32 bytes, and 5 bytes are used for the preamble,
         we need 32 * 2 + 5 = 69 bytes to store two hashes in a page. We round
         this value to 70. *)
      let max_page_size = 70 in
      let payload = Bytes.of_string "Hello payload" in
      let* hash =
        lift
        @@ serialize_payload
             ~max_page_size
             payload
             ~for_each_page:Backend.save_page
      in
      let* retrieved_payload =
        lift
        @@ deserialize_payload hash ~retrieve_page_from_hash:Backend.load_page
      in
      let map_size = Backend.number_of_pages () in
      let* () = Assert.equal_int ~loc:__LOC__ map_size 1 in
      assert_equal_bytes
        ~loc:__LOC__
        "Deserialized payload do not match with original"
        payload
        retrieved_payload

    let multiple_pages_roundtrip_heterogeneous_payload () =
      (* Each page in tests contains at most 70 bytes, of which 5 are reserved
         for the page prefix. This leaves 65 bytes to store the payload to be
         serialized in a page. It also means that a `Hashes` page can contain
         at most 2 hashes of size 32 bytes each. If we try to serialize a
         payload between 131 and 195 bytes (included), then the serialized
         payload should be spread among a total of 6 pages. Of these,
         195/65 = 3 pages are used to store the payload, ceil(3/2) = 2 pages
         are used for storing the 3 hashes of the 3 payload pages, and
         ceil(2/2) = 1 page is used for storing the 2 hashes of the previous
         pages. *)
      let open Lwt_result_syntax in
      let module Backend = Make_Merkle_tree_V0_backend () in
      (* Limit the number of hashes stored per page to 2. Because hashes
         have a fixed size of 32 bytes, and 5 bytes are used for the preamble,
         we need 32 * 2 + 5 = 69 bytes to store two hashes in a page. We round
         this value to 70. *)
      let max_page_size = 70 in
      let payload =
        Bytes.of_string
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do \
           eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim \
           ad minim veniam, quis nostrud exercitation ullamco"
      in
      let* hash =
        lift
        @@ serialize_payload
             ~max_page_size
             payload
             ~for_each_page:Backend.save_page
      in
      let* retrieved_payload =
        lift
        @@ deserialize_payload hash ~retrieve_page_from_hash:Backend.load_page
      in
      let map_size = Backend.number_of_pages () in
      let* () = Assert.equal_int ~loc:__LOC__ map_size 6 in
      assert_equal_bytes
        ~loc:__LOC__
        "Deserialized payload do not match with original"
        payload
        retrieved_payload

    let multiple_pages_roundtrip_homogeneous_payload () =
      (* Each page in tests contains at most 70 bytes, of which 5 are reserved
         for the page prefix. This leaves 65 bytes to store the contents to be
         serialized in a page. It also means that a `Hashes` page can contain
         at most 2 hashes of size 32 bytes each. If we try to serialize a
         payload of 195 repetitions of the same character, then only one
         payload page will be produced. However, the hash of this page will be
         repeated three times across two pages represent nodes of the Merkle
         tree. Finally, another page will be used for storing the Merkle tree
         root page, which contains the two hashes of the Merkle tree nodes
         above. In total, the serialization should be spread among 4 pages. *)
      let module Backend = Make_Merkle_tree_V0_backend () in
      let open Lwt_result_syntax in
      (* Limit the number of hashes stored per page to 2. Because hashes
         have a fixed size of 32 bytes, and 5 bytes are used for the preamble,
         we need 32 * 2 + 5 = 69 bytes to store two hashes in a page. We round
         this value to 70. *)
      let max_page_size = 70 in
      let payload =
        List.repeat 195 (Bytes.of_string "a") |> Bytes.concat Bytes.empty
      in
      let* hash =
        lift
        @@ serialize_payload
             ~max_page_size
             payload
             ~for_each_page:Backend.save_page
      in
      let* retrieved_payload =
        lift
        @@ deserialize_payload hash ~retrieve_page_from_hash:Backend.load_page
      in
      let map_size = Backend.number_of_pages () in
      let* () = Assert.equal_int ~loc:__LOC__ map_size 4 in
      assert_equal_bytes
        ~loc:__LOC__
        "Deserialized payload do not match with original"
        payload
        retrieved_payload

    let multiple_pages_roundtrip_do_not_exceed_page_size () =
      (* Check that a bug related to the size of hashes has been fixed.
         Before the bug was fixed: the `Sc_rollup.Reveal_hash` module borrowed
         the size function from the underlying hash module, meaning that it
         would return `31` for the size, rather than the actual hash size
         which is `32`. For a page that is exactly `98` bytes long, this would
         mean that the serialization algorithm will compute the number of
         hashes per page to be `(98-5)/31 = 3`, but the actual hash pages will
         have size `32 * 3 + 5 = 101` bytes. This will cause the check on a page
         size to fail, when serializing a page.  With 98 bytes per page, 93
         bytes will be reserved for the payload in content pages.
         Before the patch was applied, trying to
         serialize a payload of `93 * 3 = 279`  bytes with a page size of
         98 bytes would have caused to try to serialize a page containing
         3 hashes of 32 bytes each, resulting in a page of `101 bytes` and
         causing the serialization to fail.
      *)
      let open Lwt_result_syntax in
      let module Backend = Make_Merkle_tree_V0_backend () in
      let max_page_size = 98 in
      (* 279 bytes of payload *)
      let payload =
        Bytes.of_string
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do \
           eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim \
           ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut \
           aliquip ex ea commodo consequat. Duis aute irure dolor in \
           reprehenderit in volup"
      in
      let* hash =
        lift
        @@ serialize_payload
             ~max_page_size
             payload
             ~for_each_page:Backend.save_page
      in
      let* retrieved_payload =
        lift
        @@ deserialize_payload hash ~retrieve_page_from_hash:Backend.load_page
      in
      assert_equal_bytes
        ~loc:__LOC__
        "Deserialized payload do not match with original"
        payload
        retrieved_payload

    let long_content_roundtrip () =
      (* To ensure that the serialization and deserialization process work as
         expected, we test a roundtrip for a reasonably long text. We also
         increase the page size to allow for more than two hashes in a page. *)
      let module Backend = Make_Merkle_tree_V0_backend () in
      let open Lwt_result_syntax in
      (* The page size is set to 150. Of these, 5 bytes are used for the page
         preamble, and the reset will contain hashes which are 32 bytes long
         each. The number of hashes that can fit into a page is
         floor((150 - 5)/32) = 4. *)
      let max_page_size = 150 in
      let payload =
        (* Inferno, Canto I (Dante Alighieri). Size in bytes: 5226. *)
        Bytes.of_string
          "Nel mezzo del cammin di nostra vita\n\
           mi ritrovai per una selva oscura\n\
           ché la diritta via era smarrita.\n\
           Ahi quanto a dir qual era è cosa dura\n\
           esta selva selvaggia e aspra e forte\n\
           che nel pensier rinova la paura!\n\
           Tant’è amara che poco è più morte;\n\
           ma per trattar del ben ch’i’ vi trovai,\n\
           dirò de l’altre cose ch’i’ v’ho scorte.\n\
           Io non so ben ridir com’i’ v’intrai,\n\
           tant’era pien di sonno a quel punto\n\
           che la verace via abbandonai.\n\
           Ma poi ch’i’ fui al piè d’un colle giunto,\n\
           là dove terminava quella valle\n\
           che m’avea di paura il cor compunto,\n\
           guardai in alto, e vidi le sue spalle\n\
           vestite già de’ raggi del pianeta\n\
           che mena dritto altrui per ogne calle.\n\
           Allor fu la paura un poco queta\n\
           che nel lago del cor m’era durata\n\
           la notte ch’i’ passai con tanta pieta.\n\
           E come quei che con lena affannata\n\
           uscito fuor del pelago a la riva\n\
           si volge a l’acqua perigliosa e guata,\n\
           così l’animo mio, ch’ancor fuggiva,\n\
           si volse a retro a rimirar lo passo\n\
           che non lasciò già mai persona viva.\n\
           Poi ch’èi posato un poco il corpo lasso,\n\
           ripresi via per la piaggia diserta,\n\
           sì che ’l piè fermo sempre era ’l più basso.\n\
           Ed ecco, quasi al cominciar de l’erta,\n\
           una lonza leggera e presta molto,\n\
           che di pel macolato era coverta;\n\
           e non mi si partia dinanzi al volto,\n\
           anzi ’mpediva tanto il mio cammino,\n\
           ch’i’ fui per ritornar più volte vòlto.\n\
           Temp’era dal principio del mattino,\n\
           e ’l sol montava ’n sù con quelle stelle\n\
           ch’eran con lui quando l’amor divino\n\
           mosse di prima quelle cose belle;\n\
           sì ch’a bene sperar m’era cagione\n\
           di quella fiera a la gaetta pelle\n\
           l’ora del tempo e la dolce stagione;\n\
           ma non sì che paura non mi desse\n\
           la vista che m’apparve d’un leone.\n\
           Questi parea che contra me venisse\n\
           con la test’alta e con rabbiosa fame,\n\
           sì che parea che l’aere ne tremesse.\n\
           Ed una lupa, che di tutte brame\n\
           sembiava carca ne la sua magrezza,\n\
           e molte genti fé già viver grame,\n\
           questa mi porse tanto di gravezza\n\
           con la paura ch’uscia di sua vista,\n\
           ch’io perdei la speranza de l’altezza.\n\
           E qual è quei che volontieri acquista,\n\
           e giugne ’l tempo che perder lo face,\n\
           che ’n tutt’i suoi pensier piange e s’attrista;\n\
           tal mi fece la bestia sanza pace,\n\
           che, venendomi ’ncontro, a poco a poco\n\
           mi ripigneva là dove ’l sol tace.\n\
           Mentre ch’i’ rovinava in basso loco,\n\
           dinanzi a li occhi mi si fu offerto\n\
           chi per lungo silenzio parea fioco.\n\
           Quando vidi costui nel gran diserto,\n\
           «Miserere di me», gridai a lui,\n\
           «qual che tu sii, od ombra od omo certo!».\n\
           Rispuosemi: «Non omo, omo già fui,\n\
           e li parenti miei furon lombardi,\n\
           mantoani per patria ambedui.\n\
           Nacqui sub Iulio, ancor che fosse tardi,\n\
           e vissi a Roma sotto ’l buono Augusto\n\
           nel tempo de li dèi falsi e bugiardi.\n\
           Poeta fui, e cantai di quel giusto\n\
           figliuol d’Anchise che venne di Troia,\n\
           poi che ’l superbo Ilión fu combusto.\n\
           Ma tu perché ritorni a tanta noia?\n\
           perché non sali il dilettoso monte\n\
           ch’è principio e cagion di tutta gioia?».\n\
           «Or se’ tu quel Virgilio e quella fonte\n\
           che spandi di parlar sì largo fiume?»,\n\
           rispuos’io lui con vergognosa fronte.\n\
           «O de li altri poeti onore e lume\n\
           vagliami ’l lungo studio e ’l grande amore\n\
           che m’ha fatto cercar lo tuo volume.\n\
           Tu se’ lo mio maestro e ’l mio autore;\n\
           tu se’ solo colui da cu’ io tolsi\n\
           lo bello stilo che m’ha fatto onore.\n\
           Vedi la bestia per cu’ io mi volsi:\n\
           aiutami da lei, famoso saggio,\n\
           ch’ella mi fa tremar le vene e i polsi».\n\
           «A te convien tenere altro viaggio»,\n\
           rispuose poi che lagrimar mi vide,\n\
           «se vuo’ campar d’esto loco selvaggio:\n\
           ché questa bestia, per la qual tu gride,\n\
           non lascia altrui passar per la sua via,\n\
           ma tanto lo ’mpedisce che l’uccide;\n\
           e ha natura sì malvagia e ria,\n\
           che mai non empie la bramosa voglia,\n\
           e dopo ’l pasto ha più fame che pria.\n\
           Molti son li animali a cui s’ammoglia,\n\
           e più saranno ancora, infin che ’l veltro\n\
           verrà, che la farà morir con doglia.\n\
           Questi non ciberà terra né peltro,\n\
           ma sapienza, amore e virtute,\n\
           e sua nazion sarà tra feltro e feltro.\n\
           Di quella umile Italia fia salute\n\
           per cui morì la vergine Cammilla,\n\
           Eurialo e Turno e Niso di ferute.\n\
           Questi la caccerà per ogne villa,\n\
           fin che l’avrà rimessa ne lo ’nferno,\n\
           là onde ’nvidia prima dipartilla.\n\
           Ond’io per lo tuo me’ penso e discerno\n\
           che tu mi segui, e io sarò tua guida,\n\
           e trarrotti di qui per loco etterno,\n\
           ove udirai le disperate strida,\n\
           vedrai li antichi spiriti dolenti,\n\
           ch’a la seconda morte ciascun grida;\n\
           e vederai color che son contenti\n\
           nel foco, perché speran di venire\n\
           quando che sia a le beate genti.\n\
           A le quai poi se tu vorrai salire,\n\
           anima fia a ciò più di me degna:\n\
           con lei ti lascerò nel mio partire;\n\
           ché quello imperador che là sù regna,\n\
           perch’i’ fu’ ribellante a la sua legge,\n\
           non vuol che ’n sua città per me si vegna.\n\
           In tutte parti impera e quivi regge;\n\
           quivi è la sua città e l’alto seggio:\n\
           oh felice colui cu’ ivi elegge!».\n\
           E io a lui: «Poeta, io ti richeggio\n\
           per quello Dio che tu non conoscesti,\n\
           acciò ch’io fugga questo male e peggio,\n\
           che tu mi meni là dov’or dicesti,\n\
           sì ch’io veggia la porta di san Pietro\n\
           e color cui tu fai cotanto mesti».\n\
           Allor si mosse, e io li tenni dietro."
      in
      let* hash =
        lift
        @@ serialize_payload
             ~max_page_size
             payload
             ~for_each_page:Backend.save_page
      in
      let* retrieved_payload =
        lift
        @@ deserialize_payload hash ~retrieve_page_from_hash:Backend.load_page
      in
      assert_equal_bytes
        ~loc:__LOC__
        "Deserialized payload do not match with original"
        payload
        retrieved_payload
  end
end

let tests =
  [
    Tztest.tztest
      "Storing only one hash per page causes serialization to fail (Merkle \
       tree, v0)"
      `Quick
      Merkle_tree.V0.serialize_one_hash_per_page_fails;
    Tztest.tztest
      "Serializing empty payload returns an error (Merkle tree, v0)"
      `Quick
      Merkle_tree.V0.serialize_empty_payload_fails;
    Tztest.tztest
      "Contents fitting in one page can be retrieved after being saved (Merkle \
       tree, v0)"
      `Quick
      Merkle_tree.V0.one_page_roundtrip;
    Tztest.tztest
      "Contents fitting in more pages can be retrieved after being saved - no \
       repeated pages (Merkle tree, V0)"
      `Quick
      Merkle_tree.V0.multiple_pages_roundtrip_heterogeneous_payload;
    Tztest.tztest
      "Contents fitting in more pages can be retrieved after being saved - \
       repeated pages (Merkle tree, V0)"
      `Quick
      Merkle_tree.V0.multiple_pages_roundtrip_homogeneous_payload;
    Tztest.tztest
      "Serialization and deserialization of very long contents is correct."
      `Quick
      Merkle_tree.V0.long_content_roundtrip;
    Tztest.tztest
      "Hashes pages are not larger than expected"
      `Quick
      Merkle_tree.V0.multiple_pages_roundtrip_do_not_exceed_page_size;
  ]
