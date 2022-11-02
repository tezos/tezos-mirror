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

type error +=
  | Deposit_as_external
  | Invalid_deposit_amount
  | Invalid_deposit_ticket
  | Wrong_deposit_parameters
  | Ticket_payload_size_limit_exceeded of {payload_size : int; limit : int}

let () =
  register_error_kind
    `Temporary
    ~id:"operation.zk_rollup_deposit_as_external"
    ~title:"Zk_rollup: attempted a deposit through an external op"
    ~description:"Zk_rollup: attempted a deposit through an external op"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Zk_rollup: attempted a deposit through an external op")
    Data_encoding.empty
    (function Deposit_as_external -> Some () | _ -> None)
    (fun () -> Deposit_as_external) ;
  register_error_kind
    `Temporary
    ~id:"operation.zk_rollup_invalid_deposit_amount"
    ~title:"Zk_rollup: attempted a deposit with an invalid amount"
    ~description:"Zk_rollup: attempted a deposit with an invalid amount"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Zk_rollup: attempted a deposit with an invalid amount")
    Data_encoding.empty
    (function Invalid_deposit_amount -> Some () | _ -> None)
    (fun () -> Invalid_deposit_amount) ;
  register_error_kind
    `Temporary
    ~id:"operation.zk_rollup_invalid_deposit_ticket"
    ~title:"Zk_rollup: attempted a deposit with an invalid ticket"
    ~description:"Zk_rollup: attempted a deposit with an invalid ticket"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Zk_rollup: attempted a deposit with an invalid ticket")
    Data_encoding.empty
    (function Invalid_deposit_ticket -> Some () | _ -> None)
    (fun () -> Invalid_deposit_ticket) ;
  register_error_kind
    `Permanent
    ~id:"operation.zk_rollup_wrong_deposit_parameters"
    ~title:"Zk_rollup: attempted a deposit with invalid parameters"
    ~description:"Zk_rollup: attempted a deposit with invalid parameters"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Zk_rollup: attempted a deposit with an invalid parameters")
    Data_encoding.empty
    (function Wrong_deposit_parameters -> Some () | _ -> None)
    (fun () -> Wrong_deposit_parameters) ;
  register_error_kind
    `Permanent
    ~id:"zk_rollup_ticket_payload_size_limit_exceeded"
    ~title:"The payload of the deposited ticket exceeded the size limit"
    ~description:"The payload of the deposited ticket exceeded the size limit"
    Data_encoding.(obj2 (req "payload_size" int31) (req "limit" int31))
    (function
      | Ticket_payload_size_limit_exceeded {payload_size; limit} ->
          Some (payload_size, limit)
      | _ -> None)
    (fun (payload_size, limit) ->
      Ticket_payload_size_limit_exceeded {payload_size; limit})
