(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
(* Copyright (c) 2023 Marigold  <contact@marigold.dev>                       *)
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

type error += DAC_node_not_ready of string

let () =
  register_error_kind
    `Permanent
    ~id:"dac_node_not_ready"
    ~title:"DAC Node is not ready"
    ~description:
      "RPC server of DAC node is not started and plugin is not resolved."
    ~pp:(fun ppf message ->
      Format.fprintf ppf "DAC Node is not ready, current status is: %s" message)
    Data_encoding.(obj1 (req "value" string))
    (function DAC_node_not_ready message -> Some message | _ -> None)
    (fun message -> DAC_node_not_ready message)

let handle_get_health_live node_ctxt =
  match Node_context.get_status node_ctxt with
  | Ready _ | Starting -> Lwt_result_syntax.return true

let handle_get_health_ready node_ctxt =
  match Node_context.get_status node_ctxt with
  | Ready _ -> Lwt_result_syntax.return true
  | Starting -> Lwt_result_syntax.tzfail @@ DAC_node_not_ready "starting"
