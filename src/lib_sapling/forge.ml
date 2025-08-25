(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

module Core = Core.Client
module S = Storage

module Input = struct
  include Core.Forge.Input

  let pos i = i.pos

  let amount i = i.amount

  let address i = i.address

  let is_spent forge_input state xfvk =
    let nf =
      Core.Nullifier.compute
        forge_input.address
        xfvk
        ~amount:forge_input.amount
        forge_input.rcm
        ~position:forge_input.pos
    in
    S.mem_nullifier state nf

  let get state pos vk =
    let existing_cm, cipher = S.get state pos in
    match of_ciphertext ~pos cipher vk with
    | None -> None
    | Some (memo, forge_input) ->
        if check_cm forge_input existing_cm then Some (memo, forge_input)
        else None

  let get_out state pos ovk =
    let existing_cm, cipher = S.get state pos in
    match of_ciphertext_out ~pos cipher ovk existing_cm with
    | None -> None
    | Some (memo, forge_input) ->
        if check_cm forge_input existing_cm then Some (memo, forge_input)
        else None

  let mem state pos = S.mem state pos
end

type output = Core.Forge.Output.t

let make_output address amount memo = Core.Forge.Output.{address; amount; memo}

let dummy_input anti_replay ctx dummy_witness root =
  let rcm = Core.Rcm.random () in
  let seed = Tezos_crypto.Hacl.Rand.gen 32 in
  let sk = Core.Spending_key.of_seed seed in
  let vk = Core.Viewing_key.of_sk sk in
  let addr = Core.Viewing_key.dummy_address () in
  let nf = Core.Nullifier.compute addr vk ~amount:0L rcm ~position:0L in
  let ar = Core.Proving.ar_random () in
  (* The proof is considered valid even with a dummy witness if the amount
       given is 0. *)
  let cv, rk, proof_i =
    Core.Proving.spend_proof
      ctx
      vk
      sk
      addr
      rcm
      ar
      ~amount:0L
      ~root
      ~witness:dummy_witness
  in
  let signature = Core.Proving.spend_sig sk ar cv nf rk proof_i anti_replay in
  Core.UTXO.{cv; nf; rk; proof_i; signature}

let create_dummy_inputs n state anti_replay ctx =
  (* assertion already checked by callers *)
  assert (n >= 0) ;
  (* In order to get a valid witness we simply use the one of the first element *)
  let not_empty = S.mem state 0L in
  if not_empty then
    (* Doesn't make sense to create dummy_inputs with an empty storage *)
    let dummy_witness = S.get_witness state 0L in
    let root = S.get_root state in
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@
    (* n is checked above *)
    List.init ~when_negative_length:() n (fun _ ->
        dummy_input anti_replay ctx dummy_witness root)
  else []

let dummy_output pctx ~memo_size =
  let addr = Core.Viewing_key.dummy_address () in
  let amount = 0L in
  let o = make_output addr amount (Tezos_crypto.Hacl.Rand.gen memo_size) in
  let rcm = Core.Rcm.random () in
  let esk = Core.DH.esk_random () in
  let cv_o, proof_o = Core.Proving.output_proof pctx esk addr rcm ~amount in
  let ciphertext, cm =
    Core.Forge.Output.to_ciphertext_without_ovk o rcm esk cv_o
  in
  Core.UTXO.{cm; proof_o; ciphertext}

let forge_transaction ?(number_dummy_inputs = 0) ?(number_dummy_outputs = 0)
    (forge_inputs : Input.t list) (forge_outputs : output list)
    (sp : Core.Spending_key.t) (anti_replay : string) ~(bound_data : string)
    (state : S.state) =
  if number_dummy_inputs < 0 then
    raise
      (Invalid_argument
         "Tezos_sapling.Forge.forge_transaction: number_dummy_inputs is \
          negative") ;
  if number_dummy_outputs < 0 then
    raise
      (Invalid_argument
         "Tezos_sapling.Forge.forge_transaction: number_dummy_outputs is \
          negative") ;
  let memo_size = S.get_memo_size state in
  List.iter
    (fun forge_output ->
      assert (Bytes.length Core.Forge.Output.(forge_output.memo) = memo_size))
    forge_outputs ;
  Core.Proving.with_proving_ctx (fun ctx ->
      let vk = Core.Viewing_key.of_sk sp in
      let root = S.get_root state in
      (* Forge the inputs *)
      let real_inputs =
        List.map
          (fun i ->
            let open Input in
            let ar = Core.Proving.ar_random () in
            let witness = S.get_witness state i.pos in
            let cv, rk, proof_i =
              Core.Proving.spend_proof
                ctx
                vk
                sp
                i.address
                i.rcm
                ar
                ~amount:i.amount
                ~root
                ~witness
            in
            let nf =
              Core.Nullifier.compute
                i.address
                vk
                ~amount:i.amount
                i.rcm
                ~position:i.pos
            in
            let signature =
              Core.Proving.spend_sig sp ar cv nf rk proof_i anti_replay
            in
            Core.UTXO.{cv; nf; rk; proof_i; signature})
          forge_inputs
      in
      let real_outputs =
        List.map
          (fun forge_output ->
            let rcm = Core.Rcm.random () in
            (* encryption of the ciphertext containing diversifier, amount,
               commitment randomness, natural language memo *)
            (* the encoding of the amount to string is done by default in decimal.
               We fill the bigstring with 0s to get it at a length of 22
               which is enough to hold 2^64 *)
            let open Core.Forge.Output in
            let esk = Core.DH.esk_random () in
            let cv_o, proof_o =
              Core.Proving.output_proof
                ctx
                esk
                forge_output.address
                rcm
                ~amount:forge_output.amount
            in
            let ciphertext, cm = to_ciphertext forge_output cv_o vk rcm esk in
            Core.UTXO.{cm; proof_o; ciphertext})
          forge_outputs
      in
      let balance =
        let balance_inputs =
          List.fold_left
            (fun res forge_input -> Int64.add res Input.(forge_input.amount))
            0L
            forge_inputs
        in
        List.fold_left
          (fun res forge_output ->
            Int64.sub res Core.Forge.Output.(forge_output.amount))
          balance_inputs
          forge_outputs
      in
      let dummy_inputs =
        create_dummy_inputs number_dummy_inputs state anti_replay ctx
      in
      let inputs = real_inputs @ dummy_inputs in
      let dummy_outputs =
        WithExceptions.Result.get_ok ~loc:__LOC__
        @@
        (* checked at entrance of function *)
        List.init ~when_negative_length:() number_dummy_outputs (fun _ ->
            dummy_output ctx ~memo_size)
      in
      let outputs = real_outputs @ dummy_outputs in
      let binding_sig =
        Core.Proving.make_binding_sig
          ctx
          inputs
          outputs
          ~balance
          ~bound_data
          anti_replay
      in
      Core.UTXO.{inputs; outputs; binding_sig; bound_data; balance; root})

(* Forge a transaction without sapling key (t to z). Add an optional amount
     of dummy inputs and outputs, set to 0 by default.
     The ovk encryption cannot be done without key and is therefore replaced by a
     dummy one *)
let forge_shield_transaction ?(number_dummy_inputs = 0)
    ?(number_dummy_outputs = 0) (forge_outputs : output list) (balance : int64)
    (anti_replay : string) ~(bound_data : string) (state : S.state) =
  if number_dummy_inputs < 0 then
    raise
      (Invalid_argument
         "Tezos_sapling.Forge.forge_shield_transaction: number_dummy_inputs is \
          negative") ;
  if number_dummy_outputs < 0 then
    raise
      (Invalid_argument
         "Tezos_sapling.Forge.forge_shield_transaction: number_dummy_outputs \
          is negative") ;
  let memo_size = S.get_memo_size state in
  List.iter
    (fun forge_output ->
      assert (Bytes.length Core.Forge.Output.(forge_output.memo) = memo_size))
    forge_outputs ;
  Core.Proving.with_proving_ctx (fun ctx ->
      (* Compute the root of the current tree *)
      let root = S.get_root state in
      let real_outputs =
        List.map
          (fun forge_output ->
            let rcm = Core.Rcm.random () in
            (* encryption of the ciphertext containing diversifier, amount,
               commitment randomness, natural language memo *)
            (* the encoding of the amount to string is done by default in decimal.
               We fill the bigstring with 0s to get it at a length of 22
               which is enough to hold 2^64 *)
            let open Core.Forge.Output in
            let esk = Core.DH.esk_random () in
            let cv_o, proof_o =
              Core.Proving.output_proof
                ctx
                esk
                forge_output.address
                rcm
                ~amount:forge_output.amount
            in
            let ciphertext, cm =
              to_ciphertext_without_ovk forge_output rcm esk cv_o
            in
            Core.UTXO.{cm; proof_o; ciphertext})
          forge_outputs
      in
      let balance_outputs =
        List.fold_left
          (fun res forge_output ->
            Int64.sub res Core.Forge.Output.(forge_output.amount))
          0L
          forge_outputs
      in
      let dummy_inputs =
        create_dummy_inputs number_dummy_inputs state anti_replay ctx
      in
      let dummy_outputs =
        WithExceptions.Result.get_ok ~loc:__LOC__
        @@
        (* checked at entrance of function *)
        List.init ~when_negative_length:() number_dummy_outputs (fun _ ->
            dummy_output ctx ~memo_size)
      in
      let outputs = real_outputs @ dummy_outputs in
      let binding_sig =
        Core.Proving.make_binding_sig
          ctx
          []
          outputs
          ~balance:balance_outputs
          ~bound_data
          anti_replay
      in
      Core.UTXO.
        {inputs = dummy_inputs; outputs; binding_sig; bound_data; balance; root})

let forge_shield_transaction_legacy ?(number_dummy_inputs = 0)
    ?(number_dummy_outputs = 0) (forge_outputs : output list) (balance : int64)
    (anti_replay : string) (state : S.state) =
  let Core.UTXO.{inputs; outputs; binding_sig; balance; root; bound_data = _} =
    forge_shield_transaction
      ~number_dummy_inputs
      ~number_dummy_outputs
      forge_outputs
      balance
      anti_replay
      ~bound_data:""
      state
  in
  Core.UTXO.Legacy.{inputs; outputs; binding_sig; balance; root}

let forge_transaction_legacy ?number_dummy_inputs ?number_dummy_outputs
    (forge_inputs : Input.t list) (forge_outputs : output list)
    (sp : Core.Spending_key.t) (anti_replay : string) (state : S.state) =
  let Core.UTXO.{inputs; outputs; binding_sig; balance; root; _} =
    forge_transaction
      ?number_dummy_inputs
      ?number_dummy_outputs
      forge_inputs
      forge_outputs
      sp
      anti_replay
      ~bound_data:""
      state
  in
  Core.UTXO.Legacy.{inputs; outputs; binding_sig; balance; root}
