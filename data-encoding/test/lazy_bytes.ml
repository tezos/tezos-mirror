(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let test () =
  let base_encoding =
    let open Data_encoding.Encoding in
    Bounded.string' ~length_kind:`N Hex 10
  in

  let lazy_encoding = Data_encoding.Encoding.lazy_encoding base_encoding in

  let base_value = "this" in

  let lazy_value = Data_encoding.Encoding.make_lazy base_encoding base_value in

  let string_from_lazy =
    Data_encoding.Binary.to_string_exn lazy_encoding lazy_value
  in
  let string_from_base =
    Data_encoding.Binary.to_string_exn base_encoding base_value
  in
  let wrapped_string_from_base =
    let d = String.length string_from_base in
    let prefix =
      Data_encoding.Binary.to_string_exn Data_encoding.Encoding.int31 d
    in
    prefix ^ string_from_base
  in
  assert (string_from_lazy = wrapped_string_from_base) ;

  let json_from_lazy = Data_encoding.Json.construct lazy_encoding lazy_value in
  let json_from_base = Data_encoding.Json.construct base_encoding base_value in
  assert (json_from_lazy = json_from_base) ;

  (* some tomfoolery to break the internal representation *)
  let broken_lazy_value =
    Data_encoding.Binary.of_string_exn lazy_encoding string_from_lazy
  in
  let () =
    Data_encoding.Encoding.apply_lazy
      ~fun_value:(fun _ -> ())
      ~fun_bytes:(fun b -> Bytes.set b 0 '\xff')
      ~fun_combine:(fun () () -> ())
      broken_lazy_value
  in
  let broken_lazy_bytes =
    Data_encoding.Encoding.force_bytes broken_lazy_value
  in

  assert (
    Option.is_none
      (Data_encoding.Binary.of_bytes_opt lazy_encoding broken_lazy_bytes)) ;

  let json_from_broken =
    Data_encoding.Json.construct lazy_encoding broken_lazy_value
  in
  let broken_lazy_value_from_json =
    Data_encoding.Json.destruct lazy_encoding json_from_broken
  in
  let json_from_broken_from_broken =
    Data_encoding.Json.construct lazy_encoding broken_lazy_value_from_json
  in
  assert (json_from_broken = json_from_broken_from_broken) ;
  let bytes_from_broken_from_broken =
    Data_encoding.Encoding.force_bytes broken_lazy_value_from_json
  in
  assert (broken_lazy_bytes = bytes_from_broken_from_broken) ;

  ()

let tests = [("base scenario", `Quick, test)]
