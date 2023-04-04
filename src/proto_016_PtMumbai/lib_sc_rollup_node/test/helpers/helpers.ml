(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>.                   *)
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

let uid = ref 0

let block_hash_of_level level =
  let s = Z.of_int32 level |> Z.to_bits in
  let len = String.length s in
  let s =
    String.init Block_hash.size (fun i -> if i >= len then '\000' else s.[i])
  in
  Block_hash.of_string_exn s

module type S = sig
  val with_node_context :
    ?constants:Constants.Parametric.t ->
    Sc_rollup.Kind.t ->
    boot_sector:string ->
    ([`Read | `Write] Node_context.t ->
    genesis:Sc_rollup_block.t ->
    'a tzresult Lwt.t) ->
    'a tzresult Lwt.t

  val add_l2_genesis_block :
    [`Read | `Write] Node_context.t ->
    boot_sector:string ->
    ((Sc_rollup_block.header, unit) Sc_rollup_block.block, tztrace) result Lwt.t

  val append_l2_block :
    [`Read | `Write] Node_context.t ->
    Sc_rollup.Inbox_message.t trace ->
    ((Sc_rollup_block.header, unit) Sc_rollup_block.block, tztrace) result Lwt.t
end

module Make (PVM : Pvm.S) = struct
  module Daemon = Daemon.Make (PVM)
  module Components = Daemon.Components

  let default_constants =
    let constants = Default_parameters.constants_test in
    let sc_rollup =
      {
        constants.sc_rollup with
        arith_pvm_enable = true;
        challenge_window_in_blocks = 4032;
        commitment_period_in_blocks = 3;
      }
    in
    {constants with sc_rollup}

  let add_l2_genesis_block (node_ctxt : _ Node_context.t) ~boot_sector =
    let open Lwt_result_syntax in
    let head =
      Layer1.
        {
          hash = Block_hash.zero;
          level = Raw_level.to_int32 node_ctxt.genesis_info.level;
        }
    in
    let* () = Node_context.save_level node_ctxt head in
    let predecessor = head in
    let predecessor_timestamp = Time.Protocol.epoch in
    let*? inbox =
      Environment.wrap_tzresult
      @@ Sc_rollup.Inbox.genesis
           ~predecessor_timestamp
           ~predecessor:predecessor.hash
           node_ctxt.genesis_info.level
    in
    let* inbox_hash = Node_context.save_inbox node_ctxt inbox in
    let inbox_witness = Sc_rollup.Inbox.current_witness inbox in
    let ctxt = Context.empty node_ctxt.context in
    let num_ticks = 0L in
    let initial_tick = Sc_rollup.Tick.initial in
    let*! initial_state = PVM.initial_state ~empty:(PVM.State.empty ()) in
    let*! state = PVM.install_boot_sector initial_state boot_sector in
    let*! genesis_state_hash = PVM.state_hash state in
    let*! ctxt = PVM.State.set ctxt state in
    let*! context_hash = Context.commit ctxt in
    let commitment =
      Sc_rollup.Commitment.genesis_commitment
        ~origination_level:node_ctxt.genesis_info.level
        ~genesis_state_hash
    in
    let* commitment_hash = Node_context.save_commitment node_ctxt commitment in
    let previous_commitment_hash = node_ctxt.genesis_info.commitment_hash in
    let header =
      Sc_rollup_block.
        {
          block_hash = head.hash;
          level = node_ctxt.genesis_info.level;
          predecessor = predecessor.hash;
          commitment_hash = Some commitment_hash;
          previous_commitment_hash;
          context = context_hash;
          inbox_witness;
          inbox_hash;
        }
    in
    let l2_block =
      Sc_rollup_block.{header; content = (); num_ticks; initial_tick}
    in
    let* () = Node_context.save_l2_head node_ctxt l2_block in
    return l2_block

  let initialize_node_context ?(constants = default_constants) kind ~boot_sector
      =
    let open Lwt_result_syntax in
    incr uid ;
    (* To avoid any conflict with previous runs of this test. *)
    let pid = Unix.getpid () in
    let data_dir =
      Filename.(concat @@ get_temp_dir_name ())
        (Format.sprintf "sc-rollup-node-test-%s-%d-%d" Protocol.name pid !uid)
    in
    let base_dir =
      Filename.(concat @@ get_temp_dir_name ())
        (Format.sprintf
           "sc-rollup-node-test-%s-base-%d-%d"
           Protocol.name
           pid
           !uid)
    in
    let filesystem = String.Hashtbl.create 10 in
    let cctxt =
      new Protocol_client_context.wrap_full
        (new Faked_client_context.unix_faked ~base_dir ~filesystem)
    in
    let* ctxt =
      Node_context.Internal_for_tests.create_node_context
        cctxt
        ~constants
        ~data_dir
        kind
    in
    let* genesis = add_l2_genesis_block ctxt ~boot_sector in
    let commitment_hash =
      WithExceptions.Option.get ~loc:__LOC__ genesis.header.commitment_hash
    in
    let ctxt =
      {ctxt with genesis_info = {ctxt.genesis_info with commitment_hash}}
    in
    return (ctxt, genesis, [data_dir; base_dir])

  let with_node_context ?constants kind ~boot_sector f =
    let open Lwt_result_syntax in
    let* node_ctxt, genesis, dirs_to_clean =
      initialize_node_context ?constants kind ~boot_sector
    in
    Lwt.finalize (fun () -> f node_ctxt ~genesis) @@ fun () ->
    let open Lwt_syntax in
    let* _ = Node_context.close node_ctxt in
    let* () =
      List.iter_s Tezos_stdlib_unix.Lwt_utils_unix.remove_dir dirs_to_clean
    in
    return_unit

  let append_l2_block (node_ctxt : _ Node_context.t) messages =
    let open Lwt_result_syntax in
    let* predecessor_l2_block =
      Node_context.last_processed_head_opt node_ctxt
    in
    let* predecessor_l2_block =
      match predecessor_l2_block with
      | Some b -> return b
      | None ->
          failwith "No genesis block, please add one with add_l2_genesis_block"
    in
    let level =
      Raw_level.(to_int32 @@ succ predecessor_l2_block.header.level)
    in
    let hash = block_hash_of_level level in
    let predecessor =
      Layer1.
        {
          hash = predecessor_l2_block.header.block_hash;
          level = Raw_level.to_int32 predecessor_l2_block.header.level;
        }
    in
    let head = Layer1.{hash; level} in
    let predecessor_timestamp =
      Time.Protocol.of_seconds (Int64.of_int32 level)
    in
    Daemon.Internal_for_tests.process_messages
      node_ctxt
      ~predecessor
      ~predecessor_timestamp
      head
      messages
end

let l2_chain_builders =
  List.map
    (fun kind ->
      let module PVM = (val Components.pvm_of_kind kind) in
      (kind, (module Make (PVM) : S)))
    Sc_rollup.Kind.all

let l2_chain_builder kind = Stdlib.List.assoc kind l2_chain_builders

let with_node_context ?constants kind ~boot_sector =
  let module L = (val l2_chain_builder kind) in
  L.with_node_context ?constants kind ~boot_sector

let add_l2_genesis_block (node_ctxt : _ Node_context.t) =
  let module L = (val l2_chain_builder node_ctxt.kind) in
  L.add_l2_genesis_block node_ctxt

let append_l2_block (node_ctxt : _ Node_context.t) =
  let module L = (val l2_chain_builder node_ctxt.kind) in
  L.append_l2_block node_ctxt

let append_l2_blocks node_ctxt message_batches =
  List.map_es (append_l2_block node_ctxt) message_batches

let append_dummy_l2_chain node_ctxt ~length =
  let open Lwt_result_syntax in
  let* head = Node_context.last_processed_head_opt node_ctxt in
  let head_level =
    match head with
    | None -> 0
    | Some h -> h.header.level |> Raw_level.to_int32 |> Int32.to_int
  in
  let batches =
    Stdlib.List.init length (fun i ->
        [
          Sc_rollup.Inbox_message.External
            (Z.to_bits (Z.of_int (i + head_level + 1)));
        ])
  in
  append_l2_blocks node_ctxt batches

module Assert = struct
  module Make_with_encoding (X : sig
    type t

    val encoding : t Data_encoding.t
  end) =
  Assert.Make_equalities (struct
    type t = X.t

    let eq (b1 : X.t) (b2 : X.t) =
      Bytes.equal
        (Data_encoding.Binary.to_bytes_exn X.encoding b1)
        (Data_encoding.Binary.to_bytes_exn X.encoding b2)

    let pp ppf (b : X.t) =
      Data_encoding.Json.pp ppf (Data_encoding.Json.construct X.encoding b)
  end)

  module L2_block = Make_with_encoding (Sc_rollup_block)
  module Commitment = Make_with_encoding (Sc_rollup.Commitment)
  module Commitment_hash = Make_with_encoding (Sc_rollup.Commitment.Hash)
  module State_hash = Make_with_encoding (Sc_rollup.State_hash)
end

let alcotest name speed ?constants kind ~boot_sector f =
  Alcotest_lwt.test_case name speed @@ fun _lwt_switch () ->
  let open Lwt_result_syntax in
  let*! r = with_node_context ?constants kind ~boot_sector f in
  match r with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Format.printf "@\n%a@." pp_print_trace err ;
      Lwt.fail Alcotest.Test_error
