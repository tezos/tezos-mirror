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
      (args1 (RawContractAlias.force_switch ()))
      (prefixes ["remember"; "contract"]
      @@ RawContractAlias.fresh_alias_param @@ RawContractAlias.source_param
      @@ stop)
      (fun force name hash cctxt ->
        let open Lwt_result_syntax in
        let* name = RawContractAlias.of_fresh cctxt force name in
        RawContractAlias.add ~force cctxt name hash);
    command
      ~group
      ~desc:"Remove a contract from the wallet."
      no_options
      (prefixes ["forget"; "contract"] @@ RawContractAlias.alias_param @@ stop)
      (fun () (name, _) cctxt -> RawContractAlias.del cctxt name);
    command
      ~group
      ~desc:"Lists all known contracts in the wallet."
      no_options
      (fixed ["list"; "known"; "contracts"])
      (fun () (cctxt : Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let* contracts = list_contracts cctxt in
        let*! () =
          List.iter_s
            (fun (prefix, alias, contract) ->
              cctxt#message
                "%s%s: %s"
                prefix
                alias
                (Contract.to_b58check contract))
            contracts
        in
        return_unit);
    command
      ~group
      ~desc:"Forget the entire wallet of known contracts."
      (args1 (RawContractAlias.force_switch ()))
      (fixed ["forget"; "all"; "contracts"])
      (fun force cctxt ->
        let open Lwt_result_syntax in
        let* () =
          fail_unless
            force
            (error_of_fmt "this can only used with option -force")
        in
        RawContractAlias.set cctxt []);
    command
      ~group
      ~desc:"Display a contract from the wallet."
      no_options
      (prefixes ["show"; "known"; "contract"]
      @@ RawContractAlias.alias_param @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let*! () = cctxt#message "%a\n%!" Contract.pp contract in
        return_unit);
  ]
