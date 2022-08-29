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

type error += Zk_rollup_does_not_exist of Zk_rollup_repr.t

let () =
  register_error_kind
    `Temporary
    ~id:"Zk_rollup_does_not_exist"
    ~title:"ZK Rollup does not exist"
    ~description:"Attempted to use a ZK rollup that has not been originated."
    ~pp:(fun ppf x ->
      Format.fprintf ppf "Rollup %a does not exist" Zk_rollup_repr.Address.pp x)
    Data_encoding.(obj1 (req "rollup" Zk_rollup_repr.Address.encoding))
    (function Zk_rollup_does_not_exist x -> Some x | _ -> None)
    (fun x -> Zk_rollup_does_not_exist x)

let originate ctxt static ~init_state =
  let open Lwt_result_syntax in
  let*? ctxt, nonce = Raw_context.increment_origination_nonce ctxt in
  let*? address = Zk_rollup_repr.Address.from_nonce nonce in
  let initial_account =
    Zk_rollup_account_repr.{static; dynamic = {state = init_state}}
  in
  let* ctxt, account_size =
    Storage.Zk_rollup.Account.init ctxt address initial_account
  in
  let init_pl = Zk_rollup_repr.(Empty {next_index = 0L}) in
  let* ctxt, pl_size =
    Storage.Zk_rollup.Pending_list.init ctxt address init_pl
  in
  let address_size = Zk_rollup_repr.Address.size in
  let size = Z.of_int (address_size + account_size + pl_size) in
  return (ctxt, address, size)

let exists ctxt rollup = Storage.Zk_rollup.Account.mem ctxt rollup
