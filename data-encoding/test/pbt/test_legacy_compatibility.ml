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

open Generators

let legacy_result ok_enc error_enc =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        (Tag 1)
        ok_enc
        ~title:"Ok"
        (function Ok x -> Some x | Error _ -> None)
        (fun x -> Ok x);
      case
        (Tag 0)
        error_enc
        ~title:"Result"
        (function Ok _ -> None | Error x -> Some x)
        (fun x -> Error x);
    ]

let full_legacy_result : type a b. a full -> b full -> (a, b) result full =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  (module struct
    type t = (Fulla.t, Fullb.t) result

    let ty = Result (Fulla.ty, Fullb.ty)

    let eq = ( = )

    let pp ppf = function
      | Ok a -> Crowbar.pp ppf "ok(%a)" Fulla.pp a
      | Error b -> Crowbar.pp ppf "error(%a)" Fullb.pp b

    let gen = Crowbar.result Fulla.gen Fullb.gen

    let encoding = legacy_result Fulla.encoding Fullb.encoding
  end)

let trip_binary pp code_ding decode_ding v =
  let bin =
    try Data_encoding.Binary.to_bytes_exn code_ding v
    with Data_encoding.Binary.Write_error we ->
      Format.kasprintf
        Crowbar.fail
        "Cannot construct: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_write_error
        we
  in
  let vv =
    try Data_encoding.Binary.of_bytes_exn decode_ding bin
    with Data_encoding.Binary.Read_error re ->
      Format.kasprintf
        Crowbar.fail
        "Cannot destruct: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_read_error
        re
  in
  Crowbar.check_eq ~pp v vv

let test_binary_compat_legacy_res (fulla_and_v : full_and_v)
    (fullb_and_v : full_and_v) =
  match (fulla_and_v, fullb_and_v) with
  | FullAndV (fulla, a), FullAndV (fullb, b) ->
      let fullr = full_result fulla fullb in
      let module Fullr = (val fullr) in
      let fullleg = full_legacy_result fulla fullb in
      let module Fullleg = (val fullleg) in
      trip_binary Fullr.pp Fullr.encoding Fullleg.encoding (Ok a) ;
      trip_binary Fullr.pp Fullleg.encoding Fullr.encoding (Ok a) ;
      trip_binary Fullr.pp Fullr.encoding Fullleg.encoding (Error b) ;
      trip_binary Fullr.pp Fullleg.encoding Fullr.encoding (Error b) ;
      ()

let () =
  Crowbar.add_test
    ~name:"binary compat legacy"
    [gen; gen]
    test_binary_compat_legacy_res ;
  ()
