(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let block_param ~name ~desc t =
  Tezos_clic.param
    ~name
    ~desc
    (Tezos_clic.parameter (fun _ str -> Lwt.return (Block_hash.of_b58check str)))
    t

let operation_param ~name ~desc t =
  Tezos_clic.param
    ~name
    ~desc
    (Tezos_clic.parameter (fun _ str ->
         Lwt.return (Operation_hash.of_b58check str)))
    t

let commands () =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  let group =
    {
      name = "admin";
      title = "Commands to perform privileged operations on the node";
    }
  in
  [
    command
      ~group
      ~desc:"Make the node forget its decision of rejecting blocks."
      no_options
      (prefixes ["unmark"; "invalid"]
      @@ seq_of_param
           (block_param
              ~name:"block"
              ~desc:"blocks to remove from invalid list"))
      (fun () blocks (cctxt : #Client_context.full) ->
        List.iter_es
          (fun block ->
            let* () = Shell_services.Invalid_blocks.delete cctxt block in
            let*! () =
              cctxt#message
                "Block %a no longer marked invalid."
                Block_hash.pp
                block
            in
            return_unit)
          blocks);
    command
      ~group
      ~desc:"Make the node forget every decision of rejecting blocks."
      no_options
      (prefixes ["unmark"; "all"; "invalid"; "blocks"] @@ stop)
      (fun () (cctxt : #Client_context.full) ->
        let* invalid_blocks = Shell_services.Invalid_blocks.list cctxt () in
        List.iter_es
          (fun {Chain_services.hash; _} ->
            let* () = Shell_services.Invalid_blocks.delete cctxt hash in
            let*! () =
              cctxt#message
                "Block %a no longer marked invalid."
                Block_hash.pp_short
                hash
            in
            return_unit)
          invalid_blocks);
    command
      ~group
      ~desc:
        "Retrieve the current checkpoint and display it in a format compatible \
         with node argument `--checkpoint`."
      no_options
      (fixed ["show"; "current"; "checkpoint"])
      (fun () (cctxt : #Client_context.full) ->
        let* checkpoint_hash, checkpoint_level =
          Shell_services.Chain.Levels.checkpoint cctxt ~chain:cctxt#chain ()
        in
        let*! () =
          cctxt#message
            "@[<v 0>Checkpoint: %a@,Checkpoint level: %ld@]"
            Block_hash.pp
            checkpoint_hash
            checkpoint_level
        in
        return_unit);
    command
      ~group
      ~desc:
        "Remove an operation from the mempool if present, reverting its effect \
         if it was applied. Add it to the set of banned operations to prevent \
         it from being fetched/processed/injected in the future. Note: If the \
         baker has already received the operation, then it's necessary to \
         restart it to flush the operation from it."
      no_options
      (prefixes ["ban"; "operation"]
      @@ operation_param ~name:"operation" ~desc:"hash of operation to ban"
      @@ stop)
      (fun () op_hash (cctxt : #Client_context.full) ->
        let* () =
          Shell_services.Mempool.ban_operation cctxt ~chain:cctxt#chain op_hash
        in
        let*! () =
          cctxt#message "Operation %a is now banned." Operation_hash.pp op_hash
        in
        return_unit);
    command
      ~group
      ~desc:
        "Remove an operation from the set of banned operations (nothing \
         happens if it was not banned)."
      no_options
      (prefixes ["unban"; "operation"]
      @@ operation_param ~name:"operation" ~desc:"hash of operation to unban"
      @@ stop)
      (fun () op_hash (cctxt : #Client_context.full) ->
        let* () =
          Shell_services.Mempool.unban_operation
            cctxt
            ~chain:cctxt#chain
            op_hash
        in
        let*! () =
          cctxt#message
            "Operation %a is now unbanned."
            Operation_hash.pp
            op_hash
        in
        return_unit);
    command
      ~group
      ~desc:"Clear the set of banned operations."
      no_options
      (fixed ["unban"; "all"; "operations"])
      (fun () (cctxt : #Client_context.full) ->
        let* () =
          Shell_services.Mempool.unban_all_operations
            cctxt
            ~chain:cctxt#chain
            ()
        in
        let*! () = cctxt#message "All operations are now unbanned." in
        return_unit);
  ]
