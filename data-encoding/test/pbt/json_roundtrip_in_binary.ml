(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let string1 =
  let open Crowbar in
  with_printer pp_string
  @@ map
       [choose [range ~min:48 10; range ~min:65 26; range ~min:97 26]]
       (fun c -> String.make 1 (Char.chr c))

let string0 =
  let open Crowbar in
  with_printer pp_string @@ choose [string1; const ""]

let rec printer fmt =
  let open Format in
  function
  | `Null -> pp_print_string fmt "null"
  | `Bool true -> pp_print_string fmt "true"
  | `Bool false -> pp_print_string fmt "false"
  | `Float _ -> pp_print_string fmt "<float>"
  | `String s -> fprintf fmt "%S" s
  | `A js ->
      pp_print_string fmt "[" ;
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
        printer
        fmt
        js ;
      pp_print_string fmt "]"
  | `O kvs ->
      pp_print_string fmt "{" ;
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
        (fun fmt (k, v) ->
          fprintf fmt "%S" k ;
          pp_print_string fmt ": " ;
          printer fmt v)
        fmt
        kvs ;
      pp_print_string fmt "}"

let g : Data_encoding.Json.t Crowbar.gen =
  let open Crowbar in
  with_printer printer
  @@ fix (fun g ->
         choose
           [
             const @@ `Null;
             const @@ `Bool true;
             const @@ `Bool false;
             map [string0] (fun s -> `String s);
             map [float] (fun f -> `Float f);
             map [list g] (fun xs -> `A xs);
             map [list (map [string1; g] (fun k v -> (k, v)))] (fun xs -> `O xs);
           ])

(* Setting up the actual tests *)
let test_json (j : Data_encoding.Json.t) =
  let json =
    try Data_encoding.Json.construct Data_encoding.Json.encoding j
    with exc -> failwith ("Cannot construct: " ^ Printexc.to_string exc)
  in
  let jj =
    try Data_encoding.Json.destruct Data_encoding.Json.encoding json
    with Data_encoding.Json.Cannot_destruct (_, _) ->
      failwith "Cannot destruct"
  in
  assert (j = jj)

let () = Crowbar.add_test ~name:"json_json_roundtrip" [g] test_json

(* The basic binary test would fail. This is because:
   - The binary encoding of JSON values uses the BSON representation
   - The BSON representation of Arrays is identical to that of an Object indexed
     by number ([`A [x; y]] is the same as [`O [("1", x); ("2", y)]]

   Instead of testing a strict equality, we test equality modulo this `A-`O
   equivalence. *)
let test_binary (j : Data_encoding.Json.t) =
  let s =
    try Data_encoding.Binary.to_string_exn Data_encoding.Json.encoding j
    with exc -> failwith ("Cannot construct: " ^ Printexc.to_string exc)
  in
  let jj =
    try Data_encoding.Binary.of_string_exn Data_encoding.Json.encoding s
    with _ -> failwith "Cannot destruct"
  in
  let rec assert_equal a b =
    match (a, b) with
    | `Null, `Null -> ()
    | `Bool a, `Bool b -> assert (a = b)
    | `Float a, `Float b -> assert (a = b)
    | `String a, `String b -> assert (a = b)
    | `A a, `A b -> List.iter2 (fun a b -> assert_equal a b) a b
    | `O a, `O b ->
        List.iter2
          (fun (n, a) (m, b) ->
            assert (n = m) ;
            assert_equal a b)
          a
          b
    | `A b, `O a | `O a, `A b ->
        let b = List.mapi (fun i x -> (string_of_int i, x)) b in
        List.iter2
          (fun (n, a) (m, b) ->
            assert (n = m) ;
            assert_equal a b)
          a
          b
    | _ -> assert false
  in
  assert_equal j jj

let () = Crowbar.add_test ~name:"json_binary(bson)_roundtrip" [g] test_binary
