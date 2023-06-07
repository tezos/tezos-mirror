(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech  <contact@trili.tech>                        *)
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

(* Testing
   -------
   Component:    Lazy containers
   Invocation:   dune exec src/lib_lazy_containers/test/main.exe \
                  -- --file chunked_byte_vector_tests.ml
   Subject:      Test chunked byte vector
*)

open QCheck_alcotest
open QCheck2
open Tezos_tree_encoding

module type S = sig
  module Chunk : sig
    type t

    val of_bytes : bytes -> t

    val to_bytes : t -> bytes

    val size : int64

    val num_needed : int64 -> int64
  end

  type t

  val name : string

  val create :
    ?origin:wrapped_tree -> ?get_chunk:(int64 -> Chunk.t Lwt.t) -> int64 -> t

  val origin : t -> wrapped_tree option

  val allocate : int64 -> t

  val of_string : string -> t

  val of_bytes : bytes -> t

  val to_string : t -> string Lwt.t

  val to_bytes : t -> bytes Lwt.t

  val grow : t -> int64 -> unit

  val length : t -> int64

  val load_byte : t -> int64 -> int Lwt.t

  val load_bytes : t -> int64 -> int64 -> bytes Lwt.t

  val store_byte : t -> int64 -> int -> unit Lwt.t

  val store_bytes : t -> int64 -> bytes -> unit Lwt.t

  val loaded_chunks : t -> (int64 * Chunk.t option) list
end

module Tests (CBV : S) = struct
  open CBV

  let to_name s = String.concat "/" [name; s]

  let create_works =
    Test.make ~name:(to_name "create works") Gen.ui64 (fun len ->
        let vector = create len in
        length vector = len)

  let to_bytes_works =
    Test.make
      ~name:(to_name "from_bytes to_bytes roundtrip")
      Gen.string
      (fun str ->
        let open Lwt.Syntax in
        Lwt_main.run
        @@
        let bytes = Bytes.of_string str in
        let vector = of_bytes bytes in
        let* bytes' = to_bytes vector in
        let vector' = of_bytes bytes' in
        let+ bytes'' = to_bytes vector' in
        bytes = bytes' && bytes = bytes'')

  let load_bytes_works_no_offset =
    Test.make
      ~name:(to_name "load_bytes without offset")
      (Gen.tup3 Gen.string Gen.nat Gen.nat)
      (fun (str, _offset, num_bytes) ->
        let open Lwt.Syntax in
        Lwt_main.run
        @@
        let bytes = Bytes.of_string str in
        let len = Bytes.length bytes in
        let offset = 0 in
        let num_bytes =
          if len > offset + 1 then num_bytes mod (len - offset - 1) else 0
        in
        let vector = of_bytes bytes in
        let+ bytes' =
          load_bytes vector (Int64.of_int offset) (Int64.of_int num_bytes)
        in
        let expected = Bytes.sub bytes offset num_bytes in
        expected = bytes')

  let load_bytes_works =
    Test.make
      ~name:(to_name "load_bytes roundtrip")
      (Gen.tup3 Gen.string Gen.nat Gen.nat)
      (fun (str, offset, num_bytes) ->
        let open Lwt.Syntax in
        Lwt_main.run
        @@
        let bytes = Bytes.of_string str in
        let len = Bytes.length bytes in
        let offset = offset mod max 1 len in
        let num_bytes =
          if len > offset + 1 then num_bytes mod (len - offset - 1) else 0
        in
        let vector = of_bytes bytes in
        let+ bytes' =
          load_bytes vector (Int64.of_int offset) (Int64.of_int num_bytes)
        in
        let expected =
          if num_bytes = 0 then Bytes.empty
          else Bytes.sub bytes offset num_bytes
        in
        expected = bytes')

  let store_load_byte_works =
    Test.make
      ~name:(to_name "store_byte and load_byte work")
      Gen.string
      (fun str ->
        let open Lwt.Syntax in
        Lwt_main.run
        @@
        let bytes = Bytes.of_string str in
        let len = Int64.of_int (Bytes.length bytes) in
        let vector = create len in
        let* mapping =
          Lwt_list.map_s (fun i ->
              let index = Int64.of_int i in
              let byte = Bytes.get_uint8 bytes i in
              let+ () = store_byte vector index byte in
              (index, byte))
          @@ List.init (Bytes.length bytes) Fun.id
        in
        Lwt_list.for_all_p
          (fun (i, c) ->
            let+ v = load_byte vector i in
            v = c)
          mapping)

  let grow_works =
    Test.make
      ~name:(to_name "grow works")
      Gen.(pair string small_int)
      (fun (init_str, grow_len) ->
        let open Lwt.Syntax in
        Lwt_main.run
        @@
        let grow_len = Int64.of_int grow_len in
        let vector = of_string init_str in
        let check_contents () =
          Lwt_list.for_all_p (fun i ->
              let index = Int64.of_int i in
              let+ v = load_byte vector index in
              v = Char.code (String.get init_str i))
          @@ List.init (String.length init_str) Fun.id
        in
        let* check1 = check_contents () in
        grow vector grow_len ;
        let+ check2 = check_contents () in
        let check3 =
          Int64.(length vector = add grow_len (of_int (String.length init_str)))
        in
        check1 && check2 && check3)

  let can_write_after_grow =
    Test.make
      ~name:(to_name "can write after grow")
      Gen.(string_size (101 -- 1_000))
      (fun append_str ->
        let open Lwt.Syntax in
        Lwt_main.run
        @@
        let chunk_size = Chunked_byte_vector.Chunk.size in
        (* We initialize the vector with a string of a size slightly
           under [chunk_size]. This is to be sure that the previous
           value remains accessible after [store_bytes] on the last
           chunk of [vector], that was filled in the process. *)
        let init_size = Int64.(sub chunk_size 100L) in
        let vector =
          of_string (String.make (Int64.to_int chunk_size - 100) 'a')
        in
        let* v = load_byte vector 0L in
        assert (v = Char.code 'a') ;
        grow vector (String.length append_str |> Int64.of_int) ;
        let* () = store_bytes vector init_size @@ Bytes.of_string append_str in
        let* v = load_byte vector 0L in
        assert (v = Char.code 'a') ;
        let* v = load_byte vector init_size in
        assert (v = Char.code (String.get append_str 0)) ;
        let+ v = load_byte vector chunk_size in
        assert (v = Char.code (String.get append_str 100)) ;
        true)

  let internal_num_pages_edge_case =
    let test () =
      let open Alcotest in
      let name = to_name "exact value" in
      check int64 name 0L (Chunk.num_needed 0L) ;
      check int64 name 1L (Chunk.num_needed Chunk.size) ;
      check int64 name 1L (Chunk.num_needed (Int64.pred Chunk.size)) ;
      check int64 name 2L (Chunk.num_needed (Int64.succ Chunk.size))
    in
    (to_name "Chunk: num_pages edge case", `Quick, test)

  let all_tests =
    internal_num_pages_edge_case
    :: List.map
         to_alcotest
         [
           create_works;
           store_load_byte_works;
           grow_works;
           can_write_after_grow;
           load_bytes_works_no_offset;
           to_bytes_works;
           load_bytes_works;
         ]
end

module Mutable_CBV = Tests (struct
  let name = "Mutable CBV"

  include Chunked_byte_vector
end)

(* This is basically adapter from immutable vector to mutable interface *)
module Immutable_CBV = Tests (struct
  module Immutable = Immutable_chunked_byte_vector

  let name = "Immutable CBV"

  module Chunk = Immutable.Chunk

  type t = Immutable.t ref

  let create ?origin ?get_chunk size =
    ref @@ Immutable.create ?origin ?get_chunk size

  let origin t = Immutable.origin !t

  let allocate size = ref @@ Immutable.allocate size

  let of_string str = ref @@ Immutable.of_string str

  let of_bytes bytes = ref @@ Immutable.of_bytes bytes

  let to_string t = Immutable.to_string !t

  let to_bytes t = Immutable.to_bytes !t

  let grow t size = t := Immutable.grow !t size

  let length t = Immutable.length !t

  let load_byte t offset = Immutable.load_byte !t offset

  let load_bytes t offset size = Immutable.load_bytes !t offset size

  let store_byte t offset byte =
    Lwt.map (fun v -> t := v) @@ Immutable.store_byte !t offset byte

  let store_bytes t offset bytes =
    Lwt.map (fun v -> t := v) @@ Immutable.store_bytes !t offset bytes

  let loaded_chunks t = Immutable.loaded_chunks !t
end)

let tests = List.concat [Mutable_CBV.all_tests; Immutable_CBV.all_tests]

let () =
  Alcotest.run ~__FILE__ "Chunked_byte_vector" [("Chunked_byte_vector", tests)]
