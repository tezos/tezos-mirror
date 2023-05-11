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

open Protocol
open Alpha_context
open Client_proto_contracts

let group =
  {
    Tezos_clic.name = "contracts";
    title = "Commands for managing the record of known contracts";
  }

let commands () =
  let open Tezos_clic in
  [
    command
      ~group
      ~desc:"Add a contract to the wallet."
      (args1 (Raw_contract_alias.force_switch ()))
      (prefixes ["remember"; "contract"]
      @@ Raw_contract_alias.fresh_alias_param @@ Raw_contract_alias.source_param
      @@ stop)
      (fun force name hash cctxt ->
        Raw_contract_alias.of_fresh cctxt force name >>=? fun name ->
        Raw_contract_alias.add ~force cctxt name hash);
    command
      ~group
      ~desc:"Remove a contract from the wallet."
      no_options
      (prefixes ["forget"; "contract"] @@ Raw_contract_alias.alias_param @@ stop)
      (fun () (name, _) cctxt -> Raw_contract_alias.del cctxt name);
    command
      ~group
      ~desc:"Lists all known contracts in the wallet."
      no_options
      (fixed ["list"; "known"; "contracts"])
      (fun () (cctxt : Alpha_client_context.full) ->
        list_contracts cctxt >>=? fun contracts ->
        List.iter_es
          (fun (prefix, alias, contract) ->
            cctxt#message
              "%s%s: %s"
              prefix
              alias
              (Contract.to_b58check contract)
            >>= return)
          contracts);
    command
      ~group
      ~desc:"Forget the entire wallet of known contracts."
      (args1 (Raw_contract_alias.force_switch ()))
      (fixed ["forget"; "all"; "contracts"])
      (fun force cctxt ->
        fail_unless force (error_of_fmt "this can only used with option -force")
        >>=? fun () -> Raw_contract_alias.set cctxt []);
    command
      ~group
      ~desc:"Display a contract from the wallet."
      no_options
      (prefixes ["show"; "known"; "contract"]
      @@ Raw_contract_alias.alias_param @@ stop)
      (fun () (_, contract) (cctxt : Alpha_client_context.full) ->
        cctxt#message "%a\n%!" Contract.pp contract >>= fun () -> return_unit);
  ]
