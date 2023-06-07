(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type error += No_worker_for_source of Signature.Public_key_hash.t

let () =
  register_error_kind
    ~id:"injector.no_worker_for_source"
    ~title:"No injecting queue for source"
    ~description:
      "An L1 operation could not be queued because its source has no worker."
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "No worker for source %a"
        Signature.Public_key_hash.pp
        s)
    `Permanent
    Data_encoding.(obj1 (req "source" Signature.Public_key_hash.encoding))
    (function No_worker_for_source s -> Some s | _ -> None)
    (fun s -> No_worker_for_source s)

type error += No_worker_for_tag of string

let () =
  register_error_kind
    ~id:"injector.no_worker_for_tag"
    ~title:"No injecting queue for tag"
    ~description:
      "An L1 operation could not be queued because its tag has no worker."
    ~pp:(fun ppf t -> Format.fprintf ppf "No worker for tag %s" t)
    `Permanent
    Data_encoding.(obj1 (req "tag" Data_encoding.string))
    (function No_worker_for_tag t -> Some t | _ -> None)
    (fun t -> No_worker_for_tag t)

type error += Step_failed of string

let () =
  register_error_kind
    ~id:"injector.step_failed"
    ~title:"A step failed in the injector"
    ~description:"A step failed in the injector."
    ~pp:(fun ppf step ->
      Format.fprintf ppf "%s failed in injector" (String.capitalize_ascii step))
    `Temporary
    Data_encoding.(obj1 (req "step" string))
    (function Step_failed s -> Some s | _ -> None)
    (fun s -> Step_failed s)
