(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    _______

    Invocation: dune build @src/lib_stdlib/test/runtest
 *)

module Assert = Assert

(* Verify the default behavior of split is handling multiple instances of the
   separator in a row
*)
let test_split_duplicated_separator () =
  let inputs =
    [
      "Hello World";
      "Hello  World";
      "Hello                       World  ";
      "  Hello                       World  ";
      "  Hello                       World";
    ]
  in
  List.iter
    (fun s -> assert (TzString.split_no_empty ' ' s = ["Hello"; "World"]))
    inputs

let test_split =
  let open QCheck2 in
  let gen =
    let open Gen in
    let* str = string_size ~gen:printable (6 -- 128) in
    let* lim = opt (1 -- 32) in
    let* sep = printable in
    return (str, sep, lim)
  in
  Test.make
    ~name:"TzString.split is reversed by String.concat"
    gen
    (fun (string, sep, limit) ->
      let split = TzString.split sep ?limit string in
      String.concat (String.make 1 sep) split = string)

let split_tests =
  [("handles a duplicated separator", `Quick, test_split_duplicated_separator)]
  @ Qcheck2_helpers.qcheck_wrap [test_split]

let test_chunk_empty () =
  match TzString.chunk_bytes 1 Bytes.empty with
  | Error _ -> assert false
  | Ok strings -> Assert.equal_list ~loc:__LOC__ strings []

let test_chunk_no_error_on_partial_chunk () =
  match TzString.chunk_bytes 2 @@ Bytes.of_string "Hello World" with
  | Error _ -> assert false
  | Ok strings ->
      Assert.equal_list ~loc:__LOC__ strings ["He"; "ll"; "o "; "Wo"; "rl"; "d"]

let test_chunk_error_on_partial_chunk () =
  let error_on_partial_chunk = "Partial chunk detected!" in
  match
    TzString.chunk_bytes ~error_on_partial_chunk 2
    @@ Bytes.of_string "Hello World"
  with
  | Error e -> Assert.equal ~loc:__LOC__ e error_on_partial_chunk
  | Ok _ -> assert false

let test_chunk_no_error_if_complete_chunks () =
  match TzString.chunk_bytes 2 @@ Bytes.of_string "Hello World!" with
  | Error _ -> assert false
  | Ok strings ->
      Assert.equal_list
        ~loc:__LOC__
        strings
        ["He"; "ll"; "o "; "Wo"; "rl"; "d!"]

let test_chunk_concat =
  let open QCheck2 in
  let gen =
    let open Gen in
    let* str = string_size ~gen:printable (6 -- 128) in
    let* chunk_size = 1 -- 32 in
    return (str, chunk_size)
  in
  Test.make
    ~name:"TzString.chunk_bytes chunks bytes correctly"
    gen
    (fun (s, chunk_size) ->
      match TzString.chunk_bytes chunk_size @@ String.to_bytes s with
      | Error _ -> false
      | Ok chunks ->
          String.concat "" chunks = s && List.hd (List.rev chunks) <> "")

let chunk_bytes_tests =
  [
    ("Returns empty list when splitting empty bytes", `Quick, test_chunk_empty);
    ( "Chunks a sequence of bytes - no error, partial chunk",
      `Quick,
      test_chunk_no_error_on_partial_chunk );
    ( "Errors if last chunk is uncomplete - error passed as argument",
      `Quick,
      test_chunk_error_on_partial_chunk );
    ( "Chunks a sequence of bytes - error passed as arguments, no partial chunk",
      `Quick,
      test_chunk_no_error_if_complete_chunks );
  ]
  @ Qcheck2_helpers.qcheck_wrap [test_chunk_concat]

let () =
  Alcotest.run
    ~__FILE__
    "TzString"
    [("split", split_tests); ("chunk_bytes", chunk_bytes_tests)]
