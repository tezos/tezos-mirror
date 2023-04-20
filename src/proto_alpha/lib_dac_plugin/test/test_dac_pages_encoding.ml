(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech  <contact@trili.tech>                  *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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
    Invocation: dune exec src/proto_alpha/lib_dac_plugin/test/main.exe \
                  -- --file test_dac_pages_encoding.ml
    Subject:    Tests for the SCORU storage module
*)

(** DAC/FIXME: https://gitlab.com/tezos/tezos/-/issues/4021
    Add tests to check actual (sequences of) bytes in serialized pages. *)

(* Tests are run against a mock storage backend where a Hash-indexed/Bytes-valued Map
   is used to simulate adding and retrieving files to a directory.
*)

(** TODO: https://gitlab.com/tezos/tezos/-/issues/4855
    Move tests to libdac_node/test
*)
let dac_plugin = Stdlib.Option.get (Dac_plugin.get Protocol.hash)

module Hashes_map = Map.Make (struct
  type t = Dac_plugin.hash

  let compare h1 h2 =
    let (module Dac_plugin) = dac_plugin in
    let s1 = Dac_plugin.to_hex h1 in
    let s2 = Dac_plugin.to_hex h2 in
    String.compare s1 s2
end)

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

module Hashes_map_backend = struct
  type t = bytes Hashes_map.t ref

  type configuration = unit

  type hash = Dac_plugin.hash

  let init () = ref Hashes_map.empty

  type error += Page_is_missing of Dac_plugin.raw_hash

  let save (_plugin : Dac_plugin.t) t ~(hash : Dac_plugin.hash) ~content =
    let open Lwt_result_syntax in
    let () = t := Hashes_map.add hash content !t in
    return ()

  let mem (_plugin : Dac_plugin.t) t (hash : Dac_plugin.hash) =
    Lwt_result_syntax.return @@ Hashes_map.mem hash !t

  let load (_plugin : Dac_plugin.t) t hash =
    let open Lwt_result_syntax in
    let bytes = Hashes_map.find hash !t in
    match bytes with
    | None -> tzfail @@ Page_is_missing (Dac_plugin.hash_to_raw hash)
    | Some bytes -> return bytes

  let number_of_pages t = List.length @@ Hashes_map.bindings !t
end

(* Page store implementation that uses two in-memory stores (p1, p2).
   Data is loaded from (p2) if present in such a store, otherwise it
   is fetched from (p1) and, if the contents of the page are valid
   with respect to the hash provided, then it is saved to p2.
   Otherwise, an error is returned.
*)

module With_hash_check :
  Page_store.S with type configuration = unit and type t = Hashes_map_backend.t =
  Page_store.Internal_for_tests.With_data_integrity_check (Hashes_map_backend)

module Double_hash_map_backend :
  Page_store.S
    with type configuration = Hashes_map_backend.t * Hashes_map_backend.t =
  Page_store.Internal_for_tests.With_remote_fetch
    (struct
      type remote_context = Hashes_map_backend.t

      let fetch = Hashes_map_backend.load
    end)
    (With_hash_check)

let assert_equal_bytes ~loc msg a b =
  Assert.equal ~loc Bytes.equal msg String.pp_bytes_hex a b

let assert_fails_with ~loc k expected_err =
  let open Lwt_result_syntax in
  let*! res = k in
  Assert.error ~loc res (( = ) expected_err)

module Merkle_tree = struct
  open Pages_encoding.Merkle_tree

  module Make_V0_for_test (C : Pages_encoding.CONFIG) (S : Page_store.S) =
  struct
    module Buffered =
      Internal_for_tests.Make_buffered
        (S)
        (struct
          let content_version = 0

          let hashes_version = 0
        end)
        (C)

    include Internal_for_tests.Make (Buffered)
  end

  module V0 = struct
    let test_serialization_fails_with ~loc ~max_page_size ~payload ~error =
      let open
        Make_V0_for_test
          (struct
            let max_page_size = max_page_size
          end)
          (Hashes_map_backend) in
      let page_store = Hashes_map_backend.init () in
      let serialize_payload =
        serialize_payload dac_plugin ~page_store payload
      in
      assert_fails_with ~loc serialize_payload error

    let test_serialization_roundtrip ?expect_num_of_pages ~loc ~max_page_size
        payload =
      let open
        Make_V0_for_test
          (struct
            let max_page_size = max_page_size
          end)
          (Hashes_map_backend) in
      let page_store = Hashes_map_backend.init () in
      let open Lwt_result_syntax in
      let* hash = serialize_payload dac_plugin ~page_store payload in
      let* retrieved_payload =
        deserialize_payload dac_plugin ~page_store hash
      in
      let* () =
        match expect_num_of_pages with
        | Some expected ->
            let actual = Hashes_map_backend.number_of_pages page_store in
            Assert.equal_int ~loc actual expected
        | None -> return_unit
      in
      assert_equal_bytes
        ~loc
        "Deserialized payload do not match with original"
        payload
        retrieved_payload

    (* We use 50 bytes as the size of a page. Of these, 5 bytes are used for
       the preamble, which leaves 45 bytes of space for storing hashes in a
       page. The overall size of a hash is 33 bytes (32 bytes for the inner hash
       plus 1 byte for the tag identifying the hashing scheme of the hash),
       therefore only floor(45/33) = 1 hashes can be stored for each page.
       Because the serialization process requires a page size that can contain
       at least two hashes, the serialization of any content will fail in this case.
    *)
    let serialize_one_hash_per_page_fails () =
      let payload =
        List.repeat 195 (Bytes.of_string "a") |> Bytes.concat Bytes.empty
      in
      test_serialization_fails_with
        ~loc:__LOC__
        ~max_page_size:50
        ~payload
        ~error:(Pages_encoding.Merkle_tree_branching_factor_not_high_enough 1)

    let serialize_empty_payload_fails () =
      (* Limit the number of hashes stored per page to 2. Because hashes
         have a fixed size of 33 bytes (32 bytes for inner hash and 1 byte for
         hashing sheme tag of the hash), and 5 bytes are used for the preamble,
         we need 33 * 2 + 5 = 71 bytes to store two hashes in a page. We round
         this value to 80. *)
      test_serialization_fails_with
        ~loc:__LOC__
        ~max_page_size:80
        ~payload:Bytes.empty
        ~error:Pages_encoding.Payload_cannot_be_empty

    let one_page_roundtrip () =
      (* Limit the number of hashes stored per page to 2. Because hashes
         have a fixed size of 33 bytes (32 bytes for inner hash and 1 byte for
         hashing sheme tag of the hash), and 5 bytes are used for the preamble,
         we need 33 * 2 + 5 = 71 bytes to store two hashes in a page. We round
         this value to 80. *)
      let max_page_size = 80 in
      let payload = Bytes.of_string "Hello payload" in
      test_serialization_roundtrip
        ~expect_num_of_pages:1
        ~loc:__LOC__
        ~max_page_size
        payload

    let multiple_pages_roundtrip_heterogeneous_payload () =
      (* Each page in tests contains at most 80 bytes, of which 5 are reserved
         for the page prefix. This leaves 75 bytes to store the payload to be
         serialized in a page. It also means that a `Hashes` page can contain
         at most 2 hashes of size 33 bytes each (32 bytes for inner hash and 1
         byte for hashing sheme tag of the hash). If we try to serialize a
         payload between 151 and 225 bytes (included), then the serialized
         payload should be spread among a total of 6 pages. Of these,
         225/75 = 3 pages are used to store the payload, ceil(3/2) = 2 pages
         are used for storing the 3 hashes of the 3 payload pages, and
         ceil(2/2) = 1 page is used for storing the 2 hashes of the previous
         pages. *)
      let max_page_size = 80 in
      let payload =
        Bytes.of_string
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do \
           eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim \
           ad minim veniam, quis nostrud exercitation ullamco"
      in
      test_serialization_roundtrip
        ~expect_num_of_pages:6
        ~loc:__LOC__
        ~max_page_size
        payload

    let deserialization_of_corrupt_data_with_hash_integrity_check_fails () =
      let open Lwt_result_syntax in
      let module Page_size = struct
        let max_page_size = 80
      end in
      let module Mock_remote_codec =
        Make_V0_for_test (Page_size) (Hashes_map_backend)
      in
      let module Mock_synch_codec =
        Make_V0_for_test
          (struct
            let max_page_size = 80
          end)
          (Double_hash_map_backend)
      in
      let mock_remote_store = Hashes_map_backend.init () in
      let mock_local_store = Hashes_map_backend.init () in
      let page_store =
        Double_hash_map_backend.init (mock_remote_store, mock_local_store)
      in
      let payload =
        Bytes.of_string "This is a payload that will be tampered later on"
      in
      let corrupt_payload =
        Bytes.of_string "This is the payload that has been tampered with"
      in
      let* root_hash =
        Mock_remote_codec.serialize_payload
          dac_plugin
          ~page_store:mock_remote_store
          payload
      in
      (* We save the corrupt payload in the store and then retrieve it again,
         to be sure that the content corresponds to a valid page. Then we
         update the content of the original (non corrupt) payload in the store
         to the (serialized) corrupt payload. *)
      let* root_hash_of_corrupt_payload =
        Mock_remote_codec.serialize_payload
          dac_plugin
          ~page_store:mock_remote_store
          corrupt_payload
      in
      let* serialised_corrupt_payload =
        Hashes_map_backend.load
          dac_plugin
          mock_remote_store
          root_hash_of_corrupt_payload
      in
      let* () =
        Hashes_map_backend.save
          dac_plugin
          mock_remote_store
          ~hash:root_hash
          ~content:serialised_corrupt_payload
      in
      let* () =
        assert_fails_with
          ~loc:__LOC__
          (Mock_synch_codec.deserialize_payload
             dac_plugin
             ~page_store
             root_hash)
          (Page_store.Incorrect_page_hash
             {
               expected = Dac_plugin.hash_to_raw root_hash;
               actual = Dac_plugin.hash_to_raw root_hash_of_corrupt_payload;
             })
      in
      (* Check that pages have not been copied from the remote mock store
         to the local one. *)
      Assert.equal_int
        ~loc:__LOC__
        (Hashes_map_backend.number_of_pages mock_local_store)
        0

    let multiple_pages_roundtrip_homogeneous_payload () =
      (* Each page in tests contains at most 80 bytes, of which 5 are reserved
         for the page prefix. This leaves 75 bytes to store the content to be
         serialized in a page. It also means that a `Hashes` page can contain
         at most 2 hashes of size 33 bytes each (32 bytes for inner hash and 1
         byte for hashing sheme tag of the hash). If we try to serialize a
         payload of 225 repetitions of the same character, then only one
         payload page will be produced. However, the hash of this page will be
         repeated three times across two pages represent nodes of the Merkle
         tree. Finally, another page will be used for storing the Merkle tree
         root page, which contains the two hashes of the Merkle tree nodes
         above. In total, the serialization should be spread among 4 pages. *)
      let max_page_size = 80 in
      let payload =
        List.repeat 225 (Bytes.of_string "a") |> Bytes.concat Bytes.empty
      in
      test_serialization_roundtrip
        ~expect_num_of_pages:4
        ~loc:__LOC__
        ~max_page_size
        payload

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
      test_serialization_roundtrip ~loc:__LOC__ ~max_page_size payload

    let long_content_roundtrip () =
      (* To ensure that the serialization and deserialization process work as
         expected, we test a roundtrip for a reasonably long text. We also
         increase the page size to allow for more than two hashes in a page. *)
      (* The page size is set to 150. Of these, 5 bytes are used for the page
         preamble, and the reset will contain hashes which are 32 bytes long
         each. The number of hashes that can fit into a page is
         floor((150 - 5)/32) = 4. *)
      let max_page_size = 150 in
      let payload = Bytes.of_string long_payload in
      test_serialization_roundtrip ~loc:__LOC__ ~max_page_size payload

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4608
        Define helper function that calculates expected number of pages given the
        payload and use it for PBT expected number of pages given payload *)
    module PBT = struct
      module Generators = struct
        open QCheck2.Gen

        (* Serialization requires [~max_page_size] that guarantees at least two
           hashes per page. In orher words, we need at least 71 bytes in total since
           5 bytes is used as a preamble and each hash is 32 bytes long and uses
           aditional 1 byte for the tag used to identify the version scheme:
           5 + 2 (32 + 1) = 71 *)
        let gen_max_page_size = int_range 71 10_000

        let gen_non_empty_payload = bytes_size (int_range 1 10_000)

        let gen_non_empty_payloads =
          list_size (int_range 1 100) gen_non_empty_payload

        let max_page_size_with_non_empty_payload =
          pair gen_max_page_size gen_non_empty_payload

        let max_page_size_with_non_empty_payloads =
          pair gen_max_page_size gen_non_empty_payloads
      end

      (** [serialization_roundtrip_pbt (max_page_size, payload)] tests
          serialization scheme logic of [Pages_encoding.Merkle_tree.V0]
          by varying [max_page_size] and [payload] (arbitrary byte
          sequence). *)
      let serialization_roundtrip_pbt (max_page_size, payload) =
        test_serialization_roundtrip ~loc:__LOC__ ~max_page_size payload

      (** [merkle_tree_make_buffered_roundtrip_pbt (max_page_size, messages)]
          tests [Pages_encoding.Merkle_tree.Make_buffered] functor.
          It takes a pair [max_page_size] and [messages] (a list of non-empty
          byte sequances).Buffered payload handler is instantiated with given
          [max_page_size] and messages are serialized by adding them one by one
          in the order specified by [messages] sequence. Finally, we finalize
          serialization, which results in single preimage hash. To test the
          correctness we deserialize the original payload via beforementioned
          preimage hash. If initial serialization was correct, the obtained
          payload is equal to the result of of concatenating messages from
          original [messages] sequence.
      *)
      let merkle_tree_make_buffered_roundtrip_pbt (max_page_size, messages) =
        let open Lwt_result_syntax in
        let page_store = Hashes_map_backend.init () in
        let open
          Make_V0_for_test
            (struct
              let max_page_size = max_page_size
            end)
            (Hashes_map_backend) in
        let payload_handler = Buffered.empty () in
        let add_message message =
          Buffered.add dac_plugin ~page_store payload_handler message
        in
        let* () = List.iter_es add_message messages in
        let* hash = Buffered.finalize dac_plugin ~page_store payload_handler in
        let* retrieved_payload =
          deserialize_payload dac_plugin ~page_store hash
        in
        let expected_retrieved_payload = Bytes.concat Bytes.empty messages in
        assert_equal_bytes
          ~loc:__LOC__
          "Deserialized payload do not match with original"
          expected_retrieved_payload
          retrieved_payload
    end
  end
end

module Hash_chain = struct
  (* Return substring of [str] after the first [n] char. Returns the original string
     if n <= 0. Returns an empty string if n > String.length str *)
  let take_after str n =
    let n = max 0 n in
    String.sub str (min (String.length str) n) (max 0 (String.length str - n))

  module V0 = struct
    module Pagination_scheme = Pages_encoding.Hash_chain.V0

    let deserialize_page page :
        [`Node of Dac_plugin.hash * string | `Leaf of string] =
      if String.length page > 3996 then
        let content = String.sub page 0 3996 in
        let (module Plugin) = dac_plugin in
        let hash =
          Stdlib.Option.get
          @@ Plugin.of_hex (take_after page (3996 + String.length " hash:"))
        in
        `Node (hash, content)
      else `Leaf page

    let rec retrieve_content ~get_page ?(result = "") hash =
      let open Lwt_result_syntax in
      let* page = get_page hash in
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
      let*? pages = Pagination_scheme.make_hash_chain dac_plugin payload in
      let* () = Assert.equal_int ~loc:__LOC__ 1 (List.length pages) in
      let actual_hash, content = Stdlib.List.hd pages in
      let* () =
        assert_equal_bytes ~loc:__LOC__ "Contents not equal" payload content
      in
      let (module Plugin) = dac_plugin in
      let expected_hash =
        Plugin.to_hex @@ Plugin.hash_bytes ~scheme:Blake2B [content]
      in
      Assert.equal_string ~loc:__LOC__ expected_hash (Plugin.to_hex actual_hash)

    let test_make_chain_hash_long () =
      let open Lwt_result_syntax in
      let payload = Bytes.of_string long_payload in
      let*? pages = Pagination_scheme.make_hash_chain dac_plugin payload in
      let* () = Assert.equal_int ~loc:__LOC__ 2 (List.length pages) in
      let head_succ =
        Stdlib.List.hd pages |> snd |> fun byt ->
        take_after (String.of_bytes byt) (3996 + String.length " hash:")
      in
      let (module Plugin) = dac_plugin in
      let next_hash, content = Stdlib.List.nth pages 1 in
      let* () =
        Assert.equal_string ~loc:__LOC__ (Plugin.to_hex next_hash) head_succ
      in
      Assert.equal_string
        ~loc:__LOC__
        (Plugin.to_hex next_hash)
        (Plugin.to_hex @@ Plugin.hash_bytes ~scheme:Blake2B [content])

    let test_serialize () =
      let open Lwt_result_syntax in
      let payload = Bytes.of_string long_payload in
      let page_store = Hashes_map_backend.init () in
      let* root_hash =
        Pagination_scheme.serialize_payload
          dac_plugin
          ~for_each_page:(fun (hash, content) ->
            Hashes_map_backend.save dac_plugin page_store ~hash ~content)
          payload
      in
      let* () =
        Assert.equal_int
          ~loc:__LOC__
          (Hashes_map_backend.number_of_pages page_store)
          2
      in
      let get_page hash = Hashes_map_backend.load dac_plugin page_store hash in
      let* content = retrieve_content ~get_page root_hash in
      Assert.equal_string ~loc:__LOC__ long_payload content

    let test_serialize_empty_payload_fails () =
      let page_store = Hashes_map_backend.init () in
      let payload = Bytes.of_string "" in
      let result =
        Pagination_scheme.serialize_payload
          ~for_each_page:(fun (hash, content) ->
            Hashes_map_backend.save dac_plugin page_store ~hash ~content)
          dac_plugin
          payload
      in
      assert_fails_with
        ~loc:__LOC__
        result
        Pages_encoding.Payload_cannot_be_empty
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
      "Serialization and deserialization of very long content is correct."
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
    Tztest.tztest
      "Deserialization with integrity check fails if page contents are corrupt"
      `Quick
      Merkle_tree.V0
      .deserialization_of_corrupt_data_with_hash_integrity_check_fails;
    Tztest.tztest_qcheck2
      ~name:"PBT for merkle_tree_V0 serialization/deserialization roundtrip"
      Merkle_tree.V0.PBT.Generators.max_page_size_with_non_empty_payload
      Merkle_tree.V0.PBT.serialization_roundtrip_pbt;
    Tztest.tztest_qcheck2
      ~name:
        "PBT for [Merkle_tree.Make_buffered] functor via \
         serialization/deserialization roundtrip"
      Merkle_tree.V0.PBT.Generators.max_page_size_with_non_empty_payloads
      Merkle_tree.V0.PBT.merkle_tree_make_buffered_roundtrip_pbt;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [Test_helpers.Unit_test.spec "Dac_pages_encoding.ml" tests]
  |> Lwt_main.run
