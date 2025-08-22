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

open Tezos_error_monad.Error_monad
open Tezos_error_monad.TzLwtreslib

module Client = struct
  module Core = Core.Client
  module InputSet = Set.Make (Forge.Input)

  let shuffle_list l =
    l
    |> List.map (fun c -> (Random.bits (), c))
    |> List.sort compare |> List.map snd

  type wallet = {
    sk : Core.Spending_key.t;
    vk : Core.Viewing_key.t;
    mutable idx : Core.Viewing_key.index;
    unspent_inputs : InputSet.t;
    balance : Int64.t;
    scanned : Int64.t;
  }

  let new_wallet sk =
    {
      sk;
      vk = Core.Viewing_key.of_sk sk;
      idx = Core.Viewing_key.default_index;
      unspent_inputs = InputSet.empty;
      balance = 0L;
      scanned = 0L;
    }

  let new_address wallet =
    let idx, address = Core.Viewing_key.new_address wallet.vk wallet.idx in
    wallet.idx <- idx ;
    address

  let scan w chain_state =
    let rec aux pos set balance =
      if Forge.Input.mem chain_state pos then
        match Forge.Input.get chain_state pos w.vk with
        | Some (_memo, forge_input) ->
            let is_spent = Forge.Input.is_spent forge_input chain_state w.vk in
            if not (InputSet.mem forge_input set || is_spent) then
              aux
                (Int64.succ pos)
                (InputSet.add forge_input set)
                (Int64.add balance (Forge.Input.amount forge_input))
            else
              (* input already known or spent *)
              aux (Int64.succ pos) set balance
        | None ->
            (* decryption failed or incorrect commitment *)
            aux (Int64.succ pos) set balance
      else if
        (* cipher not present *)
        Int64.(equal pos 0L)
      then (pos, set, balance)
      else (Int64.pred pos, set, balance)
    in
    let scanned, unspent_inputs, balance =
      aux w.scanned w.unspent_inputs w.balance
    in
    {w with unspent_inputs; balance; scanned}

  let scan_ovk ovk chain_state =
    let rec aux pos res =
      if Storage.mem chain_state pos then
        if Forge.Input.mem chain_state pos then
          match Forge.Input.get_out chain_state pos ovk with
          | Some (memo, forge_input) ->
              aux (Int64.succ pos) ((memo, forge_input) :: res)
          | None ->
              (* decryption failed or incorrect commitment *)
              aux (Int64.succ pos) res
        else (* cipher not present *)
          res
      else res
    in
    aux 0L []

  (* Create dummy output, can be used to execute a z to t payment and still
     have two outputs: one is dummy and one is the change *)
  let pay_dummy wallet memo tez chain_state key =
    assert (Int64.add wallet.balance tez >= 0L) ;
    let rec gather_input to_pay balance inputs unspent_inputs =
      if to_pay > 0L then
        let input_to_add =
          WithExceptions.Option.get ~loc:__LOC__
          @@ InputSet.choose unspent_inputs
        in
        let amount = Forge.Input.amount input_to_add in
        gather_input
          (Int64.sub to_pay amount)
          (Int64.sub balance amount)
          (input_to_add :: inputs)
          (InputSet.remove input_to_add unspent_inputs)
      else (inputs, balance, unspent_inputs, Int64.abs to_pay)
    in
    let inputs, balance, unspent_inputs, change =
      gather_input (Int64.sub 0L tez) wallet.balance [] wallet.unspent_inputs
    in
    let payment_output =
      Forge.make_output (Core.Viewing_key.dummy_address ()) 0L memo
    in
    let new_index, address =
      Core.Viewing_key.new_address wallet.vk wallet.idx
    in
    wallet.idx <- new_index ;
    let change_output =
      Forge.make_output
        address
        change
        (Bytes.concat
           Bytes.empty
           [Bytes.of_string "change for this message : "; memo])
    in
    (* We randomize the order of output in order to avoid some analysis of metadata *)
    let outputs =
      if Random.bool () then [payment_output; change_output]
      else [change_output; payment_output]
    in
    let transaction =
      Forge.forge_transaction
        ~number_dummy_inputs:3
        ~number_dummy_outputs:3
        (shuffle_list inputs)
        outputs
        wallet.sk
        key
        chain_state
    in
    let new_state = {wallet with unspent_inputs; balance} in
    (transaction, new_state)

  let pay wallet address_o amount ~memo ?(bound_data = "") tez chain_state key =
    let memo = Bytes.of_string memo in
    assert (Int64.(add wallet.balance tez) >= amount) ;
    let rec gather_input to_pay balance inputs unspent_input =
      if to_pay > 0L then
        let input_to_add =
          WithExceptions.Option.get ~loc:__LOC__
          @@ InputSet.choose unspent_input
        in
        let amount = Forge.Input.amount input_to_add in
        gather_input
          (Int64.sub to_pay amount)
          (Int64.sub balance amount)
          (input_to_add :: inputs)
          (InputSet.remove input_to_add unspent_input)
      else (inputs, balance, unspent_input, Int64.abs to_pay)
    in
    let inputs, balance, unspent_inputs, change =
      gather_input
        (Int64.sub amount tez)
        wallet.balance
        []
        wallet.unspent_inputs
    in
    let payment_output = Forge.make_output address_o amount memo in
    let change_address = new_address wallet in
    let change_output = Forge.make_output change_address change memo in
    (* We randomize the order of output to avoid some analysis of meta data *)
    let outputs =
      if Random.bool () then [payment_output; change_output]
      else [change_output; payment_output]
    in
    let transaction =
      Forge.forge_transaction
        ~number_dummy_inputs:3
        ~number_dummy_outputs:3
        (shuffle_list inputs)
        outputs
        wallet.sk
        key
        ~bound_data
        chain_state
    in
    let new_state = {wallet with unspent_inputs; balance} in
    (transaction, new_state)
end

module Validator = struct
  module Core = Core.Validator

  type error +=
    | Too_old_root of Core.Hash.t
    | Input_spent of Core.UTXO.input
    | Output_incorrect of Core.UTXO.output
    | Input_incorrect of Core.UTXO.input
    | Binding_sig_incorrect of Core.UTXO.binding_sig
    | Too_many_inputs of Core.UTXO.input list
    | Too_many_outputs of Core.UTXO.output list
    | Incoherent_memo_size of unit

  let () =
    register_error_kind
      `Branch
      ~id:"sapling.too_old_root"
      ~title:"Too old root"
      ~description:"This root is not a root of a recent merkle tree"
      ~pp:(fun ppf hash ->
        Format.fprintf
          ppf
          "@[<v 1>This is not the root of a recent merkle tree:@,%s@]@."
          (match
             Data_encoding.Binary.to_bytes
               Data_encoding.(obj1 (req "root" Core.Hash.encoding))
               hash
           with
          | Ok bytes -> Hex.show @@ Hex.of_bytes @@ bytes
          | Error _ -> "root corrupted, cannot be decoded"))
      Data_encoding.(obj1 (req "root" Core.Hash.encoding))
      (function Too_old_root hash -> Some hash | _ -> None)
      (fun hash -> Too_old_root hash)

  let () =
    register_error_kind
      `Branch
      ~id:"sapling.input_spent"
      ~title:"Input already spent"
      ~description:"This input has already been spent"
      ~pp:(fun ppf input ->
        Format.fprintf
          ppf
          "@[<v 1>This input has already been spent:@,%s@]@."
          (match
             Data_encoding.Binary.to_bytes Core.UTXO.input_encoding input
           with
          | Ok bytes -> Hex.show @@ Hex.of_bytes @@ bytes
          | Error _ -> "input corrupted, cannot be decoded"))
      Core.UTXO.input_encoding
      (function Input_spent i -> Some i | _ -> None)
      (fun i -> Input_spent i)

  let () =
    register_error_kind
      `Permanent
      ~id:"sapling.incorrect_output"
      ~title:"Incorrect output"
      ~description:
        "This output has an incorrect zk proof, or does not respect the zk \
         statement"
      ~pp:(fun ppf output ->
        Format.fprintf
          ppf
          "@[<v 1>This output has an incorrect zk proof, or does not respect \
           the zk statement:@,\
           %s@]@."
          (match
             Data_encoding.Binary.to_bytes Core.UTXO.output_encoding output
           with
          | Ok bytes -> Hex.show @@ Hex.of_bytes @@ bytes
          | Error _ -> "output corrupted, cannot be decoded"))
      Core.UTXO.output_encoding
      (function Output_incorrect i -> Some i | _ -> None)
      (fun i -> Output_incorrect i)

  let () =
    register_error_kind
      `Branch (* Can be branch or permanent *)
      ~id:"sapling.incorrect_input"
      ~title:"Incorrect input"
      ~description:
        "This input has an incorrect zk proof, does not respect the zk \
         statement or has an incorrect signature"
      ~pp:(fun ppf input ->
        Format.fprintf
          ppf
          "@[<hov 0>This input has an incorrect zk proof,@ does not respect \
           the zk statement or has an incorrect signature:@ %s@]@."
          (match
             Data_encoding.Binary.to_bytes Core.UTXO.input_encoding input
           with
          | Ok bytes -> Hex.show @@ Hex.of_bytes @@ bytes
          | Error _ -> "input corrupted, cannot be decoded"))
      Core.UTXO.input_encoding
      (function Input_incorrect i -> Some i | _ -> None)
      (fun i -> Input_incorrect i)

  let () =
    register_error_kind
      `Permanent
      ~id:"sapling.incorrect_binding_sig"
      ~title:"Incorrect binding_sig"
      ~description:"This is not the signature associated with the transaction"
      ~pp:(fun ppf signature ->
        Format.fprintf
          ppf
          "Incorrect signature: %s"
          (match
             Data_encoding.Binary.to_bytes
               Core.UTXO.binding_sig_encoding
               signature
           with
          | Ok bytes -> Hex.show @@ Hex.of_bytes @@ bytes
          | Error _ -> "input binding signature, cannot be decoded"))
      (obj1 (req "binding_sig" Core.UTXO.binding_sig_encoding))
      (function Binding_sig_incorrect bs -> Some bs | _ -> None)
      (fun bs -> Binding_sig_incorrect bs)

  let () =
    register_error_kind
      `Permanent
      ~id:"crypto.sapling.too_many_inputs"
      ~title:"Too many inputs"
      ~description:"The number of inputs cannot be more than 5208"
      ~pp:(fun ppf input_list ->
        Format.fprintf
          ppf
          "@[<v 1>This transaction has too many inputs:@,%d@]@."
          (List.length input_list))
      (Data_encoding.obj1
         (Data_encoding.req
            "input list"
            (Data_encoding.list Core.UTXO.input_encoding)))
      (function Too_many_inputs l -> Some l | _ -> None)
      (fun l -> Too_many_inputs l)

  let () =
    register_error_kind
      `Permanent
      ~id:"crypto.sapling.too_many_outputs"
      ~title:"Too many outputs"
      ~description:"The number of outputs cannot be more than 2109"
      ~pp:(fun ppf output_list ->
        Format.fprintf
          ppf
          "@[<v 1>This transaction has too many outputs:@,%d@]@."
          (List.length output_list))
      (Data_encoding.obj1
         (Data_encoding.req
            "output list"
            (Data_encoding.list Core.UTXO.output_encoding)))
      (function Too_many_outputs l -> Some l | _ -> None)
      (fun l -> Too_many_outputs l)

  let () =
    register_error_kind
      `Branch
      ~id:"sapling.incoherent_memo_size"
      ~title:"incoherent memo size"
      ~description:"The memo size are incoherent"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "@[<v 1>Memo size are incoherent]@.")
      Data_encoding.unit
      (function Incoherent_memo_size () -> Some () | _ -> None)
      (fun () -> Incoherent_memo_size ())

  let verify_update (transaction : Core.UTXO.transaction)
      (state : Storage.state) (key : string) :
      (Int64.t * Storage.state) tzresult Lwt.t =
    let open Lwt_result_syntax in
    (* Check the transaction *)
    (* Check the memo_size*)
    let coherence_of_memo_size =
      List.for_all
        (fun output ->
          Core.Ciphertext.get_memo_size Core.UTXO.(output.ciphertext)
          = Storage.get_memo_size state)
        transaction.outputs
    in
    if not coherence_of_memo_size then tzfail (Incoherent_memo_size ())
    else if
      (* To avoid overflowing the balance, the number of inputs and outputs must be
         bounded *)
      Compare.List_length_with.(transaction.inputs >= 5208)
    then tzfail (Too_many_inputs transaction.inputs)
    else if Compare.List_length_with.(transaction.outputs >= 2019) then
      tzfail (Too_many_outputs transaction.outputs)
    else if
      (* Check the root is a recent state *)
      not (Storage.mem_root state transaction.root)
    then tzfail (Too_old_root transaction.root)
    else
      let* () =
        Core.Verification.with_verification_ctx (fun ctx ->
            (* Check all the output ZK proofs *)
            let* () =
              List.iter_es
                (fun output ->
                  fail_unless
                    (Core.Verification.check_output ctx output)
                    (Output_incorrect output))
                transaction.outputs
            in
            (* Check all the input Zk proofs and signatures *)
            let* () =
              List.iter_es
                (fun input ->
                  if
                    Core.Verification.check_spend ctx input transaction.root key
                  then return_unit
                  else tzfail (Input_incorrect input))
                transaction.inputs
            in
            (* Check the signature and balance of the whole transaction *)
            fail_unless
              (Core.Verification.final_check ctx transaction key)
              (Binding_sig_incorrect transaction.binding_sig))
      in
      (* Check that each nullifier is not already present in the state and add it.
             Important to avoid spending the same input twice in a transaction. *)
      let* state =
        List.fold_left_es
          (fun state input ->
            if Storage.mem_nullifier state Core.UTXO.(input.nf) then
              tzfail (Input_spent input)
            else return (Storage.add_nullifier state Core.UTXO.(input.nf)))
          state
          transaction.inputs
      in
      (* Add the commitments to the state *)
      let state =
        let open Core.UTXO in
        let list_cm_cipher =
          List.map
            (fun output -> (output.cm, output.ciphertext))
            transaction.outputs
        in
        Storage.add state list_cm_cipher
      in
      return (transaction.balance, state)
end
