(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Sequencer
   Invocation:   dune exec tezt/tests/main.exe -- --file sc_sequencer.ml
*)

open Sc_rollup_helpers
open Tezos_protocol_alpha.Protocol

let pvm_kind = "wasm_2_0_0"

type full_sequencer_setup = {
  node : Node.t;
  client : Client.t;
  sc_sequencer_node : Sc_rollup_node.t;
  sc_rollup_client : Sc_rollup_client.t;
  sc_rollup_address : string;
  originator_key : string;
  sequencer_key : string;
}

let next_rollup_level {node; client; sc_sequencer_node; _} =
  let* () = Client.bake_for_and_wait client in
  Sc_rollup_node.wait_for_level
    ~timeout:30.
    sc_sequencer_node
    (Node.get_level node)

let setup_sequencer_kernel
    ?(originator_key = Constant.bootstrap1.public_key_hash)
    ?(sequencer_key = Constant.bootstrap1.public_key_hash) protocol =
  let* node, client = setup_l1 protocol in
  let sc_sequencer_node =
    Sc_rollup_node.create
      Custom
      node
      ~path:"./octez-smart-rollup-sequencer-node"
      ~base_dir:(Client.base_dir client)
      ~default_operator:sequencer_key
  in
  (* Prepare sequencer kernel & originate it *)
  let* boot_sector =
    prepare_installer_kernel
      ~base_installee:"./"
      ~preimages_dir:
        (Filename.concat
           (Sc_rollup_node.data_dir sc_sequencer_node)
           "wasm_2_0_0")
      "sequenced_kernel"
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector
      ~parameters_ty:"unit"
      ~src:originator_key
      client
  in
  (* Start a sequencer node *)
  let* () =
    Sc_rollup_node.run_sequencer
      sc_sequencer_node
      sc_rollup_address
      ["--log-kernel-debug"]
  in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_sequencer_node in
  return
    {
      node;
      client;
      sc_sequencer_node;
      sc_rollup_client;
      sc_rollup_address;
      originator_key;
      sequencer_key;
    }

let wrap_with_framed rollup_address msg =
  (* Byte from framing protocol, then smart rollup address, then message bytes *)
  String.concat
    ""
    [
      "\000";
      Data_encoding.Binary.to_string_exn
        Sc_rollup_repr.Address.encoding
        rollup_address;
      msg;
    ]

let send_message ~src client raw_msg =
  Client.Sc_rollup.send_message
    ~hooks
    ~src
    ~msg:(sf "hex:[%S]" @@ hex_encode raw_msg)
    client

let wait_for_sequence_debug_message sc_node =
  Sc_rollup_node.wait_for sc_node "kernel_debug.v0" @@ fun json ->
  let message = JSON.as_string json in
  if String.starts_with ~prefix:"Received a sequence message" message then
    Some message
  else None

let test_delayed_inbox_consumed =
  Protocol.register_test
    ~__FILE__
    ~tags:["sequencer"]
    ~title:"Originate sequencer kernel & consume delayed inbox messages"
  @@ fun protocol ->
  let* ({client; sc_rollup_address; sc_sequencer_node; _} as setup) =
    setup_sequencer_kernel protocol
  in
  let sc_rollup_address =
    Sc_rollup_repr.Address.of_b58check_exn sc_rollup_address
  in
  let* () =
    send_message ~src:Constant.bootstrap2.alias client
    @@ wrap_with_framed sc_rollup_address "\000\000\000"
  in
  let* () =
    send_message ~src:Constant.bootstrap3.alias client
    @@ wrap_with_framed sc_rollup_address "\000\000\001"
  in

  (* Start async collection of sequence debug messages from the kernel *)
  let collected_sequences = ref [] in
  let _ =
    let rec collect_sequences () =
      let* c = wait_for_sequence_debug_message sc_sequencer_node in
      collected_sequences := c :: !collected_sequences ;
      collect_sequences ()
    in
    collect_sequences ()
  in

  (* Bake block with those user messages, which has level 3, origination level is 2 *)
  let* _ = next_rollup_level setup in

  (* At this moment delayed inbox corresponding to the previous block is empty,
     hence, no Sequences have been batched. *)

  (* Bake a block with level 4 *)
  let* _ = next_rollup_level setup in

  (* At this moment delayed inbox corresponding to the previous block have 5 messages:
     [SoL, IpL, "\000\000\000", "\000\000\001", EoL].
     Seq_batcher has batched a Sequence with
     5 delayed inbox messages and 0 L2 messages, which denoted S1.
  *)

  (* Bake a block with level 5, no injected batches here *)
  let* _ = next_rollup_level setup in

  (* At this moment delayed inbox corresponding to the previous block have 8 messages:
     [SoL3, IpL3, "\000\000\000", "\000\000\001", EoL3, SoL4, IpL4, EoL4].
     For now no delayed inbox messages consumed, so we just expect delayed inbox messages being accumulated.
     Seq_batcher has batched a Sequence with
     8 delayed inbox messages and 0 L2 messages, which denoted S2.
  *)

  (* Inject S1 into an upcoming block with level 6 *)

  (* Bake a block with level 6, containing S1 *)
  let* _ = next_rollup_level setup in

  (* Feed to the sequencer kernel S1 sequence *)

  (* Inject S2 into an upcoming block with level 7 *)
  (* Following S3 is not going to be injected within this test, so we ingore consideration of it *)

  (* Bake a block with level 7, containing S2 *)
  let* _ = next_rollup_level setup in

  (* Feed to the sequencer kernel S2 sequence *)
  let expected_sequences =
    List.map (fun (delayed_inbox_prefix, delayed_inbox_suffix) ->
        Format.sprintf
          "Received a sequence message UnverifiedSigned { body: Framed { \
           destination: SmartRollupAddress { hash: SmartRollupHash(\"%s\") }, \
           payload: Sequence(Sequence { nonce: 0, delayed_messages_prefix: %d, \
           delayed_messages_suffix: %d, messages: [] }) }, signature: \
           Signature(\"edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q\") \
           }"
          (Sc_rollup_repr.Address.to_b58check sc_rollup_address)
          delayed_inbox_prefix
          delayed_inbox_suffix)
    @@ [(4, 1); (7, 1)]
  in
  Check.(
    ( = )
      expected_sequences
      (List.rev !collected_sequences)
      ~__LOC__
      (list string)
      ~error_msg:"Unexpected debug messages emitted") ;
  Lwt.return_unit

let register ~protocols = test_delayed_inbox_consumed protocols
