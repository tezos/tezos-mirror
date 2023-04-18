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
    Invocation: dune exec src/proto_016_PtMumbai/lib_dal/test/main.exe
    Subject:    Tests for the SCORU storage module
*)

(** DAC/FIXME: https://gitlab.com/tezos/tezos/-/issues/4021
    Add tests to check actual (sequences of) bytes in serialized pages. *)

open Protocol

let lift f = Lwt.map Environment.wrap_tzresult f

(* Tests are run against a mock storage backend where a Hash-indexed/Bytes-valued Map
   is used to simulate adding and retrieving files to a directory.
*)
module Hashes_map = Sc_rollup_reveal_hash.Map

type hashes_map = bytes Hashes_map.t

let long_payload =
  (* Inferno, Canto I (Dante Alighieri). Size in bytes: 5226. *)
  {|Nel mezzo del cammin di nostra vita
mi ritrovai per una selva oscura
ché la diritta via era smarrita.
Ahi quanto a dir qual era è cosa dura
esta selva selvaggia e aspra e forte
che nel pensier rinova la paura!
Tant’è amara che poco è più morte;
ma per trattar del ben ch’i’ vi trovai,
dirò de l’altre cose ch’i’ v’ho scorte.
Io non so ben ridir com’i’ v’intrai,
tant’era pien di sonno a quel punto
che la verace via abbandonai.
Ma poi ch’i’ fui al piè d’un colle giunto,
là dove terminava quella valle
che m’avea di paura il cor compunto,
guardai in alto, e vidi le sue spalle
vestite già de’ raggi del pianeta
che mena dritto altrui per ogne calle.
Allor fu la paura un poco queta
che nel lago del cor m’era durata
la notte ch’i’ passai con tanta pieta.
E come quei che con lena affannata
uscito fuor del pelago a la riva
si volge a l’acqua perigliosa e guata,
così l’animo mio, ch’ancor fuggiva,
si volse a retro a rimirar lo passo
che non lasciò già mai persona viva.
Poi ch’èi posato un poco il corpo lasso,
ripresi via per la piaggia diserta,
sì che ’l piè fermo sempre era ’l più basso.
Ed ecco, quasi al cominciar de l’erta,
una lonza leggera e presta molto,
che di pel macolato era coverta;
e non mi si partia dinanzi al volto,
anzi ’mpediva tanto il mio cammino,
ch’i’ fui per ritornar più volte vòlto.
Temp’era dal principio del mattino,
e ’l sol montava ’n sù con quelle stelle
ch’eran con lui quando l’amor divino
mosse di prima quelle cose belle;
sì ch’a bene sperar m’era cagione
di quella fiera a la gaetta pelle
l’ora del tempo e la dolce stagione;
ma non sì che paura non mi desse
la vista che m’apparve d’un leone.
Questi parea che contra me venisse
con la test’alta e con rabbiosa fame,
sì che parea che l’aere ne tremesse.
Ed una lupa, che di tutte brame
sembiava carca ne la sua magrezza,
e molte genti fé già viver grame,
questa mi porse tanto di gravezza
con la paura ch’uscia di sua vista,
ch’io perdei la speranza de l’altezza.
E qual è quei che volontieri acquista,
e giugne ’l tempo che perder lo face,
che ’n tutt’i suoi pensier piange e s’attrista;
tal mi fece la bestia sanza pace,
che, venendomi ’ncontro, a poco a poco
mi ripigneva là dove ’l sol tace.
Mentre ch’i’ rovinava in basso loco,
dinanzi a li occhi mi si fu offerto
chi per lungo silenzio parea fioco.
Quando vidi costui nel gran diserto,
«Miserere di me», gridai a lui,
«qual che tu sii, od ombra od omo certo!».
Rispuosemi: «Non omo, omo già fui,
e li parenti miei furon lombardi,
mantoani per patria ambedui.
Nacqui sub Iulio, ancor che fosse tardi,
e vissi a Roma sotto ’l buono Augusto
nel tempo de li dèi falsi e bugiardi.
Poeta fui, e cantai di quel giusto
figliuol d’Anchise che venne di Troia,
poi che ’l superbo Ilión fu combusto.
Ma tu perché ritorni a tanta noia?
perché non sali il dilettoso monte
ch’è principio e cagion di tutta gioia?».
«Or se’ tu quel Virgilio e quella fonte
che spandi di parlar sì largo fiume?»,
rispuos’io lui con vergognosa fronte.
«O de li altri poeti onore e lume
vagliami ’l lungo studio e ’l grande amore
che m’ha fatto cercar lo tuo volume.
Tu se’ lo mio maestro e ’l mio autore;
tu se’ solo colui da cu’ io tolsi
lo bello stilo che m’ha fatto onore.
Vedi la bestia per cu’ io mi volsi:
aiutami da lei, famoso saggio,
ch’ella mi fa tremar le vene e i polsi».
«A te convien tenere altro viaggio»,
rispuose poi che lagrimar mi vide,
«se vuo’ campar d’esto loco selvaggio:
ché questa bestia, per la qual tu gride,
non lascia altrui passar per la sua via,
ma tanto lo ’mpedisce che l’uccide;
e ha natura sì malvagia e ria,
che mai non empie la bramosa voglia,
e dopo ’l pasto ha più fame che pria.
Molti son li animali a cui s’ammoglia,
e più saranno ancora, infin che ’l veltro
verrà, che la farà morir con doglia.
Questi non ciberà terra né peltro,
ma sapienza, amore e virtute,
e sua nazion sarà tra feltro e feltro.
Di quella umile Italia fia salute
per cui morì la vergine Cammilla,
Eurialo e Turno e Niso di ferute.
Questi la caccerà per ogne villa,
fin che l’avrà rimessa ne lo ’nferno,
là onde ’nvidia prima dipartilla.
Ond’io per lo tuo me’ penso e discerno
che tu mi segui, e io sarò tua guida,
e trarrotti di qui per loco etterno,
ove udirai le disperate strida,
vedrai li antichi spiriti dolenti,
ch’a la seconda morte ciascun grida;
e vederai color che son contenti
nel foco, perché speran di venire
quando che sia a le beate genti.
A le quai poi se tu vorrai salire,
anima fia a ciò più di me degna:
con lei ti lascerò nel mio partire;
ché quello imperador che là sù regna,
perch’i’ fu’ ribellante a la sua legge,
non vuol che ’n sua città per me si vegna.
In tutte parti impera e quivi regge;
quivi è la sua città e l’alto seggio:
oh felice colui cu’ ivi elegge!».
E io a lui: «Poeta, io ti richeggio
per quello Dio che tu non conoscesti,
acciò ch’io fugga questo male e peggio,
che tu mi meni là dov’or dicesti,
sì ch’io veggia la porta di san Pietro
e color cui tu fai cotanto mesti».
Allor si mosse, e io li tenni dietro.|}

module type BACKEND = sig
  type h

  val save_page : h * bytes -> (unit, error trace) result Lwt.t

  val load_page : h -> (bytes, error trace) result Lwt.t

  val number_of_pages : unit -> int
end

module type Hashes_Map_backend = functor () ->
  BACKEND with type h = Hashes_map.key

module Hashes_Map_backend () = struct
  open Environment.Error_monad

  type error +=
    | Page_already_saved of Sc_rollup_reveal_hash.t
    | Page_is_missing of Sc_rollup_reveal_hash.t

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
      let module Backend = Hashes_Map_backend () in
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
      let module Backend = Hashes_Map_backend () in
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
      let module Backend = Hashes_Map_backend () in
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
      (* Each page in tests contains at most 80 bytes, of which 5 are reserved
         for the page prefix. This leaves 75 bytes to store the payload to be
         serialized in a page. It also means that a `Hashes` page can contain
         at most 2 hashes of size 33 bytes each. If we try to serialize a
         payload between 151 and 225 bytes (included), then the serialized
         payload should be spread among a total of 6 pages. Of these,
         225/75 = 3 pages are used to store the payload, ceil(3/2) = 2 pages
         are used for storing the 3 hashes of the 3 payload pages, and
         ceil(2/2) = 1 page is used for storing the 2 hashes of the previous
         pages. *)
      let open Lwt_result_syntax in
      let module Backend = Hashes_Map_backend () in
      (* Limit the number of hashes stored per page to 2. Because hashes
         have a fixed size of 32 bytes, and 5 bytes are used for the preamble,
         we need 32 * 2 + 5 = 69 bytes to store two hashes in a page. We round
         this value to 70. *)
      let max_page_size = 80 in
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
      (* Each page in tests contains at most 80 bytes, of which 5 are reserved
         for the page prefix. This leaves 75 bytes to store the contents to be
         serialized in a page. It also means that a `Hashes` page can contain
         at most 2 hashes of size 32 bytes each. If we try to serialize a
         payload of 225 repetitions of the same character, then only one
         payload page will be produced. However, the hash of this page will be
         repeated three times across two pages represent nodes of the Merkle
         tree. Finally, another page will be used for storing the Merkle tree
         root page, which contains the two hashes of the Merkle tree nodes
         above. In total, the serialization should be spread among 4 pages. *)
      let module Backend = Hashes_Map_backend () in
      let open Lwt_result_syntax in
      (* Limit the number of hashes stored per page to 2. Because hashes
         have a fixed size of 32 bytes, and 5 bytes are used for the preamble,
         we need 32 * 2 + 5 = 69 bytes to store two hashes in a page. We round
         this value to 70. *)
      let max_page_size = 80 in
      let payload =
        List.repeat 225 (Bytes.of_string "a") |> Bytes.concat Bytes.empty
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
      let module Backend = Hashes_Map_backend () in
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
      let module Backend = Hashes_Map_backend () in
      let open Lwt_result_syntax in
      (* The page size is set to 150. Of these, 5 bytes are used for the page
         preamble, and the reset will contain hashes which are 32 bytes long
         each. The number of hashes that can fit into a page is
         floor((150 - 5)/32) = 4. *)
      let max_page_size = 150 in
      let payload = Bytes.of_string long_payload in
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

module Hash_chain = struct
  (* Return substring of [str] after the first [n] char. Returns the original string
     if n <= 0. Returns an empty string if n > String.length str *)
  let take_after str n =
    let n = max 0 n in
    String.sub str (min (String.length str) n) (max 0 (String.length str - n))

  module V0 = struct
    module Pagination_scheme = Dac_pages_encoding.Hash_chain.V0

    let deserialize_page page :
        [`Node of Sc_rollup_reveal_hash.t * string | `Leaf of string] =
      if String.length page > 3996 then
        let content = String.sub page 0 3996 in
        let hash =
          Stdlib.Option.get
          @@ Sc_rollup_reveal_hash.of_b58check_opt
               (take_after page (3996 + String.length " hash:"))
        in
        `Node (hash, content)
      else `Leaf page

    let rec retrieve_content ~get_page ?(result = "") page_hash =
      let open Lwt_result_syntax in
      let* page = get_page page_hash in
      let* res = return @@ deserialize_page (Bytes.to_string page) in
      match res with
      | `Node (succ_hash, content) ->
          (retrieve_content [@tailcall])
            ~get_page
            ~result:(String.cat result content)
            succ_hash
      | `Leaf content -> return @@ String.cat result content

    let test_make_chain_hash_one_page () =
      let open Lwt_result_syntax in
      let payload = Bytes.of_string "simple payload" in
      let*? pages = Pagination_scheme.make_hash_chain payload in
      let* () = Assert.equal_int ~loc:__LOC__ 1 (List.length pages) in
      let actual_hash, content = Stdlib.List.hd pages in
      let* () =
        assert_equal_bytes ~loc:__LOC__ "Contents not equal" payload content
      in
      let expected_hash =
        Pagination_scheme.to_b58check @@ Pagination_scheme.hash content
      in
      Assert.equal_string
        ~loc:__LOC__
        expected_hash
        (Pagination_scheme.to_b58check actual_hash)

    let test_make_chain_hash_long () =
      let open Lwt_result_syntax in
      let payload = Bytes.of_string long_payload in
      let*? pages = Pagination_scheme.make_hash_chain payload in
      let* () = Assert.equal_int ~loc:__LOC__ 2 (List.length pages) in
      let head_succ =
        Stdlib.List.hd pages |> snd |> fun byt ->
        take_after (String.of_bytes byt) (3996 + String.length " hash:")
      in
      let next_hash, content = Stdlib.List.nth pages 1 in
      let* () =
        Assert.equal_string
          ~loc:__LOC__
          (Pagination_scheme.to_b58check next_hash)
          head_succ
      in
      Assert.equal_string
        ~loc:__LOC__
        (Pagination_scheme.to_b58check next_hash)
        (Pagination_scheme.to_b58check @@ Pagination_scheme.hash content)

    let test_serialize () =
      let module Backend = Hashes_Map_backend () in
      let open Lwt_result_syntax in
      let payload = Bytes.of_string long_payload in
      let* root_hash =
        lwt_map_error Environment.wrap_tztrace
        @@ Pagination_scheme.serialize_payload
             ~for_each_page:Backend.save_page
             payload
      in
      let* () = Assert.equal_int ~loc:__LOC__ (Backend.number_of_pages ()) 2 in
      let* content =
        lwt_map_error Environment.wrap_tztrace
        @@ retrieve_content ~get_page:Backend.load_page root_hash
      in
      Assert.equal_string ~loc:__LOC__ long_payload content

    let test_serialize_empty_payload_fails () =
      let module Backend = Hashes_Map_backend () in
      let payload = Bytes.of_string "" in
      let result =
        Pagination_scheme.serialize_payload
          ~for_each_page:Backend.save_page
          payload
      in
      assert_fails_with
        ~loc:__LOC__
        result
        Dac_pages_encoding.Payload_cannot_be_empty
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
    Tztest.tztest
      "Constructing hash chain (V0) for single page content is correct"
      `Quick
      Hash_chain.V0.test_make_chain_hash_one_page;
    Tztest.tztest
      "Constructing hash chain (V0) for multi page content is correct"
      `Quick
      Hash_chain.V0.test_make_chain_hash_long;
    Tztest.tztest
      "Serializing an empty payload returns an error (Hash chain)"
      `Quick
      Hash_chain.V0.test_serialize_empty_payload_fails;
    Tztest.tztest
      "Contents fitting in more pages can be retrieved after being saved - \
       repeated pages (Hash chain, V0)"
      `Quick
      Hash_chain.V0.test_serialize;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [Test_helpers.Unit_test.spec "pages encoding" tests]
  |> Lwt_main.run
