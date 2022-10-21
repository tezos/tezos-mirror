(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = int list

let non_empty_int_lists = [[0]; [-1]; [1]; [1; 2; 3; 6; 9; 10]; [1025; 242]]

let all_lists = [] :: non_empty_int_lists

let encoding = Json_encoding.(list int)

type tt = int list list

let non_empty_int_list_lists =
  [
    [[]];
    [[]; []; []; []];
    [[0]];
    [[-1]];
    [[0; -1]];
    [[1]; [0]; [1]];
    [[0]; [1; 2; 3; 6; 9; 10]; [1025; 242]];
  ]

let all_list_lists = [] :: non_empty_int_list_lists

let encoding_encoding = Json_encoding.(list (list int))

type ot = int list option

let all_int_list_options =
  [None; Some []; Some [1]; Some [0]; Some [1; 3; 6; 234234; 2]]

let encoding_option = Json_encoding.(option (list int))

type t2 = int list * string list

let all_t2_list =
  [
    ([], []);
    ([0], []);
    ([], [""]);
    ([], ["\000"]);
    ([], ["0"]);
    ([34; 1234], [""; "asdf"]);
  ]

let encoding_2 = Json_encoding.(tup2 (list int) (list string))

let roundtrip ~bson_relaxation encoding v =
  let json = Json_encoding.construct encoding v in
  let bson =
    Json_repr.convert
      (module Json_repr.Ezjsonm)
      (module Json_repr_bson.Repr)
      json
  in
  let bytes = Json_repr_bson.bson_to_bytes bson in
  let re_bson = Json_repr_bson.bytes_to_bson ~copy:false bytes in
  let re_json =
    Json_repr.convert
      (module Json_repr_bson.Repr)
      (module Json_repr.Ezjsonm)
      re_bson
  in
  match Json_encoding.destruct ~bson_relaxation encoding re_json with
  | exception (Json_encoding.Cannot_destruct _ as exc) ->
      Error
        (Format.asprintf
           "ERROR: %a"
           (Json_encoding.print_error ~print_unknown:(fun fmt exc ->
                Format.pp_print_string fmt (Printexc.to_string exc)))
           exc)
  | re_v -> Ok re_v

let roundtrip_relaxed () =
  List.iter
    (fun v ->
      match roundtrip ~bson_relaxation:true encoding v with
      | Error msg -> failwith msg
      | Ok r -> assert (r = v))
    all_lists

let roundtrip_nested_relaxed () =
  List.iter
    (fun v ->
      match roundtrip ~bson_relaxation:true encoding_encoding v with
      | Error msg -> failwith msg
      | Ok r -> assert (r = v))
    all_list_lists

let roundtrip_option_relaxed () =
  List.iter
    (fun v ->
      match roundtrip ~bson_relaxation:true encoding_option v with
      | Error msg -> failwith msg
      | Ok r -> assert (r = v))
    all_int_list_options

let roundtrip_2_relaxed () =
  List.iter
    (fun v ->
      match roundtrip ~bson_relaxation:true encoding_2 v with
      | Error msg -> failwith msg
      | Ok r -> assert (r = v))
    all_t2_list

let roundtrip_strict () =
  (* We test success on the empty list which is special cased. *)
  match roundtrip ~bson_relaxation:false encoding [] with
  | Error msg -> failwith msg
  | Ok r -> assert (r = [])

let roundtrip_failures () =
  List.iter
    (fun v ->
      match roundtrip ~bson_relaxation:false encoding v with
      | Error _ -> ()
      | Ok _ -> assert false)
    non_empty_int_lists

let roundtrip_nested_failures () =
  List.iter
    (fun v ->
      match roundtrip ~bson_relaxation:false encoding_encoding v with
      | Error _ -> ()
      | Ok _ -> assert false)
    non_empty_int_list_lists

let tests =
  [
    ("relaxed roundtrip", `Quick, roundtrip_relaxed);
    ("relaxed nested roundtrip", `Quick, roundtrip_nested_relaxed);
    ("relaxed option roundtrip", `Quick, roundtrip_option_relaxed);
    ("relaxed tuple roundtrip", `Quick, roundtrip_2_relaxed);
    ("strict roundtrip", `Quick, roundtrip_strict);
    ("failed roundtrip", `Quick, roundtrip_failures);
    ("failed nested roundtrip", `Quick, roundtrip_nested_failures);
  ]

let () = Alcotest.run "json-data-encoding" [("bson-relaxation", tests)]
