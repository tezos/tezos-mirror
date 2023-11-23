(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let build_rpc_directory ~(commit_info : Node_version.commit_info) validator
    mainchain_validator =
  let open Lwt_syntax in
  let distributed_db = Validator.distributed_db validator in
  let store = Distributed_db.store distributed_db in
  let dir : unit Tezos_rpc.Directory.t ref = ref Tezos_rpc.Directory.empty in
  let gen_register0 s f =
    dir := Tezos_rpc.Directory.gen_register !dir s (fun () p q -> f p q)
  in
  let gen_register1 s f =
    dir := Tezos_rpc.Directory.gen_register !dir s (fun ((), a) p q -> f a p q)
  in
  gen_register0 Monitor_services.S.bootstrapped (fun () () ->
      let block_stream, stopper =
        Chain_validator.new_head_watcher mainchain_validator
      in
      let first_run = ref true in
      let next () =
        if !first_run then (
          first_run := false ;
          let chain_store = Chain_validator.chain_store mainchain_validator in
          let* head = Store.Chain.current_head chain_store in
          let head_hash = Store.Block.hash head in
          let head_header = Store.Block.header head in
          Lwt.return_some (head_hash, head_header.shell.timestamp))
        else
          Lwt.pick
            [
              (let+ o = Lwt_stream.get block_stream in
               Option.map
                 (fun (hash, header) ->
                   (hash, header.Block_header.shell.timestamp))
                 o);
              (let+ () = Chain_validator.bootstrapped mainchain_validator in
               None);
            ]
      in
      let shutdown () = Lwt_watcher.shutdown stopper in
      Tezos_rpc.Answer.return_stream {next; shutdown}) ;
  gen_register0 Monitor_services.S.applied_blocks (fun q () ->
      let block_stream, stopper = Store.global_block_watcher store in
      let shutdown () = Lwt_watcher.shutdown stopper in
      let in_chains (chain_store, _block) =
        match q#chains with
        | [] -> Lwt.return_true
        | chains ->
            let that_chain_id = Store.Chain.chain_id chain_store in
            List.exists_p
              (fun chain ->
                let+ o = Chain_directory.get_chain_id_opt store chain in
                match o with
                | None -> false
                | Some this_chain_id ->
                    Chain_id.equal this_chain_id that_chain_id)
              chains
      in
      let in_protocols (chain_store, block) =
        match q#protocols with
        | [] -> Lwt.return_true
        | protocols -> (
            let* o = Store.Block.read_predecessor_opt chain_store block in
            match o with
            | None -> Lwt.return_false (* won't happen *)
            | Some pred ->
                let* context = Store.Block.context_exn chain_store pred in
                let* protocol = Context_ops.get_protocol context in
                Lwt.return
                  (List.exists (Protocol_hash.equal protocol) protocols))
      in
      let in_next_protocols (chain_store, block) =
        match q#next_protocols with
        | [] -> Lwt.return_true
        | protocols ->
            let* context = Store.Block.context_exn chain_store block in
            let* next_protocol = Context_ops.get_protocol context in
            Lwt.return
              (List.exists (Protocol_hash.equal next_protocol) protocols)
      in
      let stream =
        Lwt_stream.filter_map_s
          (fun ((chain_store, block) as elt) ->
            let* in_chains = in_chains elt in
            let* in_next_protocols = in_next_protocols elt in
            let* in_protocols = in_protocols elt in
            if in_chains && in_protocols && in_next_protocols then
              let chain_id = Store.Chain.chain_id chain_store in
              Lwt.return_some
                ( chain_id,
                  Store.Block.hash block,
                  Store.Block.header block,
                  Store.Block.operations block )
            else Lwt.return_none)
          block_stream
      in
      let next () = Lwt_stream.get stream in
      Tezos_rpc.Answer.return_stream {next; shutdown}) ;
  gen_register0 Monitor_services.S.validated_blocks (fun q () ->
      let* chains =
        match q#chains with
        | [] ->
            let* all_chain_stores = Store.all_chain_stores store in
            Lwt.return
              (List.map (fun cs -> Store.Chain.chain_id cs) all_chain_stores)
        | l ->
            List.map_s (fun chain -> Chain_directory.get_chain_id store chain) l
      in
      let* block_streams, stoppers =
        List.fold_left_s
          (fun (block_streams, stoppers) chain_id ->
            let* r = Store.get_chain_store store chain_id in
            match r with
            | Error _ -> Lwt.fail Not_found
            | Ok chain_store ->
                let block_stream, stopper =
                  Store.Chain.validated_watcher chain_store
                in
                let bs =
                  Lwt_stream.map
                    (fun b -> (chain_id, chain_store, b))
                    block_stream
                in
                return (bs :: block_streams, stopper :: stoppers))
          ([], [])
          chains
      in
      let block_stream = Lwt_stream.choose block_streams in
      let shutdown () = List.iter Lwt_watcher.shutdown stoppers in
      let in_next_protocols chain_store block =
        match q#next_protocols with
        | [] -> Lwt.return_true
        | protocols -> (
            let* block_protocol_opt =
              Store.Chain.find_protocol
                chain_store
                ~protocol_level:(Store.Block.proto_level block)
            in
            match block_protocol_opt with
            | None ->
                (* If we do not know the protocol hash associated to the
                   proto level, it means either that it is a transition
                   block for which we do not know the protocol yet, or
                   that something really bad occurred. In both cases, we
                   do not advertise it. *)
                Lwt.return_false
            | Some next_protocol ->
                Lwt.return
                  (List.exists (Protocol_hash.equal next_protocol) protocols))
      in
      let stream =
        Lwt_stream.filter_map_s
          (fun (chain_id, chain_store, block) ->
            let* in_next_protocols = in_next_protocols chain_store block in
            if in_next_protocols then
              Lwt.return_some
                ( chain_id,
                  Store.Block.hash block,
                  Store.Block.header block,
                  Store.Block.operations block )
            else Lwt.return_none)
          block_stream
      in
      let next () = Lwt_stream.get stream in
      Tezos_rpc.Answer.return_stream {next; shutdown}) ;
  gen_register1 Monitor_services.S.heads (fun chain q () ->
      (* TODO: when `chain = `Test`, should we reset then stream when
         the `testnet` change, or dias we currently do ?? *)
      let* chain_store = Chain_directory.get_chain_store_exn store chain in
      match Validator.get validator (Store.Chain.chain_id chain_store) with
      | Error _ -> Lwt.fail Not_found
      | Ok chain_validator ->
          let block_stream, stopper =
            Chain_validator.new_head_watcher chain_validator
          in
          let* head = Store.Chain.current_head chain_store in
          let shutdown () = Lwt_watcher.shutdown stopper in
          let within_protocols header =
            let find_protocol protocol_level =
              let+ p = Store.Chain.find_protocol chain_store ~protocol_level in
              WithExceptions.Option.to_exn
                ~none:
                  (Failure
                     (Format.sprintf "Cannot find protocol %d" protocol_level))
                p
            in
            let next_protocol (header : Block_header.t) =
              find_protocol header.shell.proto_level
            in
            let current_protocol (header : Block_header.t) =
              let* pred =
                Store.Block.read_block chain_store header.shell.predecessor
              in
              match pred with
              | Error e ->
                  Format.kasprintf
                    Stdlib.failwith
                    "Cannot find current protocol because missing predecessor: \
                     %a"
                    pp_print_trace
                    e
              | Ok pred_block ->
                  let pred_header = Store.Block.header pred_block in
                  find_protocol pred_header.shell.proto_level
            in
            let within protocols get_protocol header =
              match protocols with
              | [] -> return_true
              | _ ->
                  let+ p = get_protocol header in
                  List.exists (Protocol_hash.equal p) protocols
            in
            let* ok_next = within q#next_protocols next_protocol header in
            let* ok_current = within q#protocols current_protocol header in
            return (ok_current && ok_next)
          in
          let stream =
            Lwt_stream.filter_map_s
              (fun (hash, header) ->
                let* within_protocols = within_protocols header in
                if within_protocols then Lwt.return_some (hash, header)
                else Lwt.return_none)
              block_stream
          in
          let* first_block_is_within_protocols =
            within_protocols (Store.Block.header head)
          in
          let first_call =
            (* Skip the first block if this is false *)
            ref first_block_is_within_protocols
          in
          let next () =
            if !first_call then (
              first_call := false ;
              Lwt.return_some (Store.Block.hash head, Store.Block.header head))
            else Lwt_stream.get stream
          in
          Tezos_rpc.Answer.return_stream {next; shutdown}) ;
  gen_register0 Monitor_services.S.protocols (fun () () ->
      let stream, stopper = Store.Protocol.protocol_watcher store in
      let shutdown () = Lwt_watcher.shutdown stopper in
      let next () = Lwt_stream.get stream in
      Tezos_rpc.Answer.return_stream {next; shutdown}) ;
  gen_register0 Monitor_services.S.commit_hash (fun () () ->
      Tezos_rpc.Answer.return commit_info.commit_hash) ;
  gen_register0 Monitor_services.S.active_chains (fun () () ->
      let stream, stopper = Validator.chains_watcher validator in
      let shutdown () = Lwt_watcher.shutdown stopper in
      let first_call =
        (* Only notify the newly created chains if this is false *)
        ref true
      in
      let next () =
        let convert (chain_id, b) =
          if not b then Lwt.return (Monitor_services.Stopping chain_id)
          else if
            Chain_id.equal
              (Store.Chain.chain_id (Store.main_chain_store store))
              chain_id
          then Lwt.return (Monitor_services.Active_main chain_id)
          else
            let* o = Store.get_chain_store_opt store chain_id in
            match o with
            | None -> Lwt.fail Not_found
            | Some chain_store ->
                let {Genesis.protocol; _} = Store.Chain.genesis chain_store in
                let expiration_date =
                  WithExceptions.Option.to_exn_f
                    ~none:(fun () ->
                      Invalid_argument
                        (Format.asprintf
                           "Monitor.active_chains: no expiration date for the \
                            chain %a"
                           Chain_id.pp
                           chain_id))
                    (Store.Chain.expiration chain_store)
                in
                Lwt.return
                  (Monitor_services.Active_test
                     {chain = chain_id; protocol; expiration_date})
        in
        if !first_call then (
          first_call := false ;
          let* l =
            List.map_p
              (fun c -> convert (c, true))
              (Validator.get_active_chains validator)
          in
          Lwt.return_some l)
        else
          let* o = Lwt_stream.get stream in
          match o with
          | None -> Lwt.return_none
          | Some c ->
              let* status = convert c in
              Lwt.return_some [status]
      in
      Tezos_rpc.Answer.return_stream {next; shutdown}) ;
  !dir
