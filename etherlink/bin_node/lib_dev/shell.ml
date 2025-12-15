(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_layer2_shell

let rec pp_rlp fmt = function
  | Rlp.Value data -> Format.fprintf fmt "0x%a" Hex.pp (Hex.of_bytes data)
  | List items ->
      Format.fprintf
        fmt
        "@[<hov 1>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
           pp_rlp)
        items

let () =
  Tezos_layer2_shell.Pp.register_pp ~name:"rlp" @@ fun fmt str ->
  match Rlp.decode str with
  | Ok rlp -> pp_rlp fmt rlp
  | Error err ->
      Format.fprintf
        fmt
        "<not a rlp value: %a>"
        Error_monad.pp_print_top_error_of_trace
        err

let run ~(config : Configuration.t) block_param k =
  let open Lwt_result_syntax in
  let pool = Lwt_domain.setup_pool 1 in
  let*! populated = Data_dir.populated ~data_dir:config.data_dir in
  let* () = unless populated @@ fun () -> failwith "Empty data dir" in
  let* ro_ctxt = Evm_ro_context.load ~pool config in
  let (module Rpc_backend) = Evm_ro_context.ro_backend ro_ctxt config in
  let* block_number =
    Rpc_backend.block_param_to_block_number ~chain_family:EVM block_param
  in
  let* store_result =
    Evm_store.(
      use ro_ctxt.store @@ fun conn -> Context_hashes.find conn block_number)
  in
  match store_result with
  | None ->
      failwith
        "Cannot get a state for block %a"
        Ethereum_types.Block_parameter.pp_extended
        block_param
  | Some hash ->
      let*! context = Pvm.Context.checkout_exn ro_ctxt.index hash in
      let*! tree = Pvm.State.get context in
      k ro_ctxt tree

let ls = Commands.ls ~subkeys:Evm_state.subkeys ~inspect:Evm_state.inspect

let cat = Commands.cat ~inspect:Evm_state.inspect

let commands = [Commands.Command ls; Command cat]

let main ~config block_param =
  let open Lwt_result_syntax in
  run ~config block_param @@ fun _ro_ctxt tree ->
  let*! () = LTerm_inputrc.load () in
  let*! term = Lazy.force LTerm.stdout in
  Lwt_result.ok (Repl.loop commands tree term)

let cat ~(config : Configuration.t) block_param pp path =
  run ~config block_param @@ fun _ro_ctxt tree ->
  Commands.run (Printer.format_printer Format.std_formatter) tree cat (path, pp)

let ls ~(config : Configuration.t) block_param path =
  run ~config block_param @@ fun _ro_ctxt tree ->
  Commands.run (Printer.format_printer Format.std_formatter) tree ls path
