(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:  Protocol (Michelson)
    Invocation: dune exec src/proto_017_PtNairob/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_lazy_storage_diff.ml
    Subject:    Test lazy storage
*)

open Protocol

(** Generation of input data *)

let ids =
  [|1; 42; 1337; 1984|] |> Array.map Z.of_int
  |> Array.map Lazy_storage_kind.Big_map.Id.parse_z

let strs = [|"0"; "True"; "nat"; "bool"|]

let exprs = strs |> Array.map Expr.from_string

let hashes =
  strs |> Array.map (fun x -> [x]) |> Array.map Script_expr_hash.hash_string

let updates_len_existing = [1; 2; 3]

let updates_len_other = 0 :: updates_len_existing

let gen_inits idx :
    (( Lazy_storage_kind.Big_map.Id.t,
       Lazy_storage_kind.Big_map.alloc )
     Lazy_storage_diff.init
    * int list)
    list =
  [
    (Existing, updates_len_existing);
    (Copy {src = ids.(idx - 1)}, updates_len_other);
    ( Alloc {key_type = exprs.(idx); value_type = exprs.(idx - 1)},
      updates_len_other );
  ]

let gen_update_list idx : Lazy_storage_kind.Big_map.update list =
  [None; Some exprs.(idx)]
  |> List.map (fun value ->
         Lazy_storage_kind.Big_map.
           {key = exprs.(idx); key_hash = hashes.(idx); value})

let rec gen_updates updates_len : Lazy_storage_kind.Big_map.updates list =
  if updates_len = 0 then []
  else
    gen_updates (updates_len - 1)
    |> List.map (fun suffix ->
           gen_update_list updates_len
           |> List.map (fun prefix -> prefix :: suffix))
    |> List.flatten

let gen_updates_list updates_lens : Lazy_storage_kind.Big_map.updates list =
  updates_lens |> List.map gen_updates |> List.flatten

let gen_diffs idx :
    ( Lazy_storage_kind.Big_map.Id.t,
      Lazy_storage_kind.Big_map.alloc,
      Lazy_storage_kind.Big_map.updates )
    Lazy_storage_diff.diff
    list =
  let open Lazy_storage_diff in
  Remove
  :: (gen_inits idx
     |> List.map (fun (init, updates_lens) ->
            gen_updates_list updates_lens
            |> List.map (fun updates -> Update {init; updates}))
     |> List.flatten)

let gen_diffs_items idx : Lazy_storage_diff.diffs_item list =
  let id = ids.(idx) in
  gen_diffs idx |> List.map (fun diff -> Lazy_storage_diff.make Big_map id diff)

let rec gen_diffs_list len : Lazy_storage_diff.diffs list =
  if len = 0 then []
  else
    gen_diffs_list (len - 1)
    |> List.map (fun suffix ->
           gen_diffs_items len |> List.map (fun prefix -> prefix :: suffix))
    |> List.flatten

let diffs_list_lens = [0; 1; 2; 3]

let diffs_list : Lazy_storage_diff.diffs list =
  diffs_list_lens |> List.map gen_diffs_list |> List.flatten

(** Properties to check *)

let conversion_roundtrip lazy_storage_diff =
  let legacy_big_map_diff =
    Contract_storage.Legacy_big_map_diff.of_lazy_storage_diff lazy_storage_diff
  in
  let reconverted =
    Contract_storage.Legacy_big_map_diff.to_lazy_storage_diff
      legacy_big_map_diff
  in
  assert (Stdlib.( = ) reconverted lazy_storage_diff)

let encoding_roundtrip lazy_storage_diff =
  let encoded =
    Data_encoding.Binary.to_bytes_exn
      Lazy_storage_diff.encoding
      lazy_storage_diff
  in
  match Data_encoding.Binary.of_bytes Lazy_storage_diff.encoding encoded with
  | Ok decoded -> assert (Stdlib.( = ) decoded lazy_storage_diff)
  | Error _ -> Stdlib.failwith "Decoding failed"

(** Iterator and test definitions *)

let on_diffs f () =
  List.iter f diffs_list ;
  return_unit

(* Marked Slow because they take 5 to 10 seconds and are unlikely to change *)
let tests =
  [
    Tztest.tztest "conversion roundtrip" `Slow (on_diffs conversion_roundtrip);
    Tztest.tztest "encoding roundtrip" `Slow (on_diffs encoding_roundtrip);
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("lazy storage diff", tests)]
  |> Lwt_main.run
