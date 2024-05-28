(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type error += Bad_amount_param of (string * string)

let msg_parameter _param =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ s -> return s)

let amount_parameter param =
  let open Lwt_result_syntax in
  Tezos_clic.parameter (fun _ s ->
      match Int32.of_string_opt s with
      | Some amount -> return amount
      | None -> tzfail (Bad_amount_param (param, s)))

let amount_param ~name ~desc next =
  Tezos_clic.param ~name ~desc (amount_parameter name) next

let msg_param ~name ~desc next =
  Tezos_clic.param ~name ~desc (msg_parameter name) next

let () =
  register_error_kind
    `Permanent
    ~id:"demo.client.badAmountParam"
    ~title:"Bad Amount Param"
    ~description:"Invalid amount parameter."
    ~pp:(fun ppf (arg_name, literal) ->
      Format.fprintf
        ppf
        "Invalid literal %s for parameter %s. Should be 32-bytes integer."
        arg_name
        literal)
    Data_encoding.(obj2 (req "parameter" string) (req "literal" string))
    (function
      | Bad_amount_param (parameter, literal) -> Some (parameter, literal)
      | _ -> None)
    (fun (parameter, literal) -> Bad_amount_param (parameter, literal))
