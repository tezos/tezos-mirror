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

open Generators

let () =
  let open Data_encoding in
  let ding = option int64 in
  let expect =
    Data_encoding__Binary_size.uint8 (* tag *)
    + max 0 Data_encoding__Binary_size.int64
  in
  match Binary.maximum_length ding with
  | None -> assert false
  | Some n -> assert (n = expect)

let () =
  let open Data_encoding in
  let ding = list ~max_length:10 (result int64 (Fixed.string 10)) in
  let expect =
    Data_encoding__Binary_size.uint30 (* dynamic size *)
    + 10 (* max number of elements in the list *)
      * (Data_encoding__Binary_size.uint8 (* tag *)
        + max Data_encoding__Binary_size.int64 10 (* fixed-string size *))
  in
  match Binary.maximum_length ding with
  | None -> assert false
  | Some n -> assert (n = expect)

let is_fixed_has_max ding =
  match Data_encoding.classify ding with
  | `Variable | `Dynamic -> Crowbar.bad_test ()
  | `Fixed n ->
      Crowbar.check_eq (Data_encoding.Binary.maximum_length ding) (Some n)

let () =
  Crowbar.add_test
    ~name:"classify->maximum-length"
    [gen_full]
    (fun (AnyFull full) ->
      let module Full = (val full) in
      is_fixed_has_max Full.encoding)

let has_no_max_is_dyn_or_var ding =
  match Data_encoding.Binary.maximum_length ding with
  | Some _ -> Crowbar.bad_test ()
  | None -> (
      match Data_encoding.classify ding with
      | `Variable | `Dynamic -> ()
      | `Fixed _ ->
          Crowbar.fail "Encoding without a maximum length has a fixed length")

let () =
  Crowbar.add_test
    ~name:"maximum-length->classify"
    [gen_full]
    (fun (AnyFull full) ->
      let module Full = (val full) in
      has_no_max_is_dyn_or_var Full.encoding)

let check_size_doesnt_increase_max_len ding checked_size =
  match Data_encoding.Binary.maximum_length ding with
  | None ->
      let checked_ding = Data_encoding.check_size checked_size ding in
      Crowbar.check_eq
        (Data_encoding.Binary.maximum_length checked_ding)
        (Some checked_size)
  | Some n ->
      let checked_ding = Data_encoding.check_size checked_size ding in
      Crowbar.check_eq
        (Data_encoding.Binary.maximum_length checked_ding)
        (Some (min n checked_size))

let () =
  Crowbar.add_test
    ~name:"maximum-length(check_size)"
    [gen_full; Crowbar.uint16]
    (fun (AnyFull full) checked_size ->
      let module Full = (val full) in
      check_size_doesnt_increase_max_len Full.encoding checked_size)
