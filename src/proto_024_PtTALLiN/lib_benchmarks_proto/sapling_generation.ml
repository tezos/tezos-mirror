(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* ------------------------------------------------------------------------- *)
type sapling_gen_options = {
  max_inputs : int;
  max_outputs : int;
  max_nullifiers : int;
  max_additional_commitments : int;
  seed : int option;
}

let default_sapling_gen_options =
  {
    max_inputs = 100;
    max_outputs = 100;
    max_nullifiers = 100;
    max_additional_commitments = 50;
    seed = None;
  }

(* ------------------------------------------------------------------------- *)
(* Evil incantations *)

(* We have to break the protocol abstraction boundary quite often in this
   module. Props to whoever finds a way to avoid these calls. *)

let alpha_to_raw (x : Alpha_context.t) : Raw_context.t = Obj.magic x

let raw_to_alpha (x : Raw_context.t) : Alpha_context.t = Obj.magic x

(* ------------------------------------------------------------------------- *)
(* Helpers *)

(* sample a random permutation of [0 ; ... ; n-1] *)
let fisher_yates n state =
  let a = Array.init n (fun i -> i) in
  for i = 0 to Array.length a - 1 do
    let j = Random.State.int state (i + 1) in
    let tmp = a.(j) in
    a.(j) <- a.(i) ;
    a.(i) <- tmp
  done ;
  a

(* sample a random injection of [0 ; ... ; m-1 ] in [0 ; ... ; n - 1] *)
let random_injection m n state =
  if m > n then invalid_arg "random_injection"
  else
    let a = fisher_yates n state in
    Array.sub a 0 m

(* ------------------------------------------------------------------------- *)
(* Sapling generation *)

(* Sapling state spec + sapling transaction valid for that state. *)
type sapling_transition = {
  state_seed : int64;
  nullifier_count : int64;
  commitment_count : int64;
  sapling_tx : Alpha_context.Sapling.transaction;
}

type forge_info = {
  rcm : Tezos_sapling.Core.Client.Rcm.t;
  position : int64;
  amount : int64;
  address : Tezos_sapling.Core.Client.Viewing_key.address;
  nf : Tezos_sapling.Core.Client.Nullifier.t;
}

let random_amount state sum =
  Random.State.int64
    state
    (Int64.sub Tezos_sapling.Core.Validator.UTXO.max_amount sum)

let reverse diff =
  Protocol.Sapling_repr.
    {
      diff with
      commitments_and_ciphertexts = List.rev diff.commitments_and_ciphertexts;
    }

let pp_rpc_diff fmtr (diff : Protocol.Sapling_repr.diff) =
  let json =
    Data_encoding.Json.construct Protocol.Sapling_repr.diff_encoding diff
  in
  Format.fprintf fmtr "%a" Data_encoding.Json.pp json

let random_bytes state size =
  Bytes.init size (fun _ -> Char.chr (Random.State.int state 256))

let rec gen_rcm state =
  let rcm =
    Data_encoding.Binary.of_bytes_exn
      Tezos_sapling.Core.Client.Rcm.encoding
      (random_bytes state 32)
  in
  try
    Tezos_sapling.Core.Client.Rcm.assert_valid rcm ;
    rcm
  with _ -> gen_rcm state

(* Adds a commitment, ciphertext, cv to an rpc_diff *)
let add_input diff vk index position sum state =
  let open Lwt_result_syntax in
  let rcm = gen_rcm state in
  let amount = random_amount state sum in
  let new_idx, address =
    Tezos_sapling.Core.Client.Viewing_key.new_address vk index
  in
  let cv =
    Tezos_sapling.Core.Client.CV.of_bytes (random_bytes state 32)
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  let ciphertext, cm =
    Tezos_sapling.Core.Client.Forge.Output.to_ciphertext
      Tezos_sapling.Core.Client.Forge.Output.
        {address; amount; memo = Bytes.empty}
      cv
      vk
      rcm
      (Tezos_sapling.Core.Client.DH.esk_random ())
  in
  let nf =
    Tezos_sapling.Core.Client.Nullifier.compute address vk ~amount rcm ~position
  in
  let diff =
    Protocol.Sapling_repr.
      {
        diff with
        commitments_and_ciphertexts =
          (cm, ciphertext) :: diff.commitments_and_ciphertexts;
      }
  in
  return (diff, {rcm; position; amount; address; nf}, new_idx)

let generate_commitments ~vk ~nb_input ~nb_cm ~nb_nf ~diff ~index state =
  let open Lwt_result_syntax in
  let inj = random_injection nb_input nb_cm state in
  let use_for_input i = Array.exists (fun k -> k = i) inj in
  let rec loop i cm_index nb_nf diff to_forge sum =
    if i = nb_cm then return (reverse diff, to_forge)
    else if use_for_input i then
      (* create commitment for input *)
      let* diff, forge_info, next_index =
        add_input diff vk cm_index (Int64.of_int i) sum state
      in
      let sum = Int64.add sum forge_info.amount in
      loop (i + 1) next_index nb_nf diff (forge_info :: to_forge) sum
    else
      (* create commitment (not for input) *)
      let* diff, {nf; _}, next_index =
        add_input diff vk cm_index (Int64.of_int i) sum state
      in
      (* can we use a nullifier? *)
      if nb_nf = 0 then (* No. *)
        loop (i + 1) next_index nb_nf diff to_forge sum
      else
        (* Yes! Grab it. *)
        let diff =
          Protocol.Sapling_repr.{diff with nullifiers = nf :: diff.nullifiers}
        in
        loop (i + 1) next_index (nb_nf - 1) diff to_forge sum
  in
  loop 0 index nb_nf diff [] 0L

(* Add roots to the storage. One cm has to be added for every root. *)
let rec add_root nb_root ctxt id vk index size diff state =
  let open Lwt_result_syntax in
  if nb_root > 0 then
    let* diff_to_add, {position = size; _}, new_idx =
      add_input Protocol.Sapling_storage.empty_diff vk index size 0L state
    in
    let* ctxt, _ =
      let*! result = Protocol.Sapling_storage.apply_diff ctxt id diff_to_add in
      Lwt.return (Environment.wrap_tzresult result)
    in
    (* We call it nb_root -1 because one root is already present*)
    add_root
      (nb_root - 1)
      ctxt
      id
      vk
      new_idx
      (Int64.succ size)
      Protocol.Sapling_repr.
        {
          diff with
          commitments_and_ciphertexts =
            diff.commitments_and_ciphertexts
            @ diff_to_add.commitments_and_ciphertexts;
        }
      state
  else return (ctxt, diff)

(* Compute a state as an OCaml object to compute the witness *)
let state_from_rpc_diff rpc_diff =
  Tezos_sapling.Storage.add
    (Tezos_sapling.Storage.empty ~memo_size:0)
    rpc_diff.Protocol.Sapling_repr.commitments_and_ciphertexts

(* Create an (unspendable) output from a proving context and a vk *)
let output proving_ctx vk sum state =
  let address = Tezos_sapling.Core.Client.Viewing_key.dummy_address () in
  let amount = random_amount state sum in
  let rcm = Tezos_sapling.Core.Client.Rcm.random () in
  let esk = Tezos_sapling.Core.Client.DH.esk_random () in
  let cv_o, proof_o =
    Tezos_sapling.Core.Client.Proving.output_proof
      proving_ctx
      esk
      address
      rcm
      ~amount
  in
  let ciphertext, cm =
    Tezos_sapling.Core.Client.Forge.Output.to_ciphertext
      Tezos_sapling.Core.Client.Forge.Output.
        {address; amount; memo = Bytes.empty}
      cv_o
      vk
      rcm
      esk
  in
  (Tezos_sapling.Core.Validator.UTXO.{cm; proof_o; ciphertext}, amount)

(* Returns a list of outputs and the sum of their amount *)
let outputs nb_output proving_ctx vk state =
  let rec aux output_amount list_outputs nb_output sum =
    match nb_output with
    | 0 -> (output_amount, list_outputs)
    | nb_output ->
        let output, amount = output proving_ctx vk sum state in
        assert (
          Int64.compare
            amount
            (Int64.sub
               Int64.max_int
               Tezos_sapling.Core.Validator.UTXO.max_amount)
          < 0) ;
        aux
          (Int64.add output_amount amount)
          (output :: list_outputs)
          (nb_output - 1)
          (Int64.add sum amount)
  in
  aux 0L [] nb_output 0L

(* Create the list of inputs. To use once the merkle tree is completed. *)
let make_inputs to_forge local_state proving_ctx sk vk root anti_replay =
  let open Lwt_result_syntax in
  List.map_ep
    (fun {rcm; position; amount; address; nf} ->
      let witness = Tezos_sapling.Storage.get_witness local_state position in
      let ar = Tezos_sapling.Core.Client.Proving.ar_random () in
      let cv, rk, proof =
        Tezos_sapling.Core.Client.Proving.spend_proof
          proving_ctx
          vk
          sk
          address
          rcm
          ar
          ~amount
          ~root
          ~witness
      in
      let signature =
        Tezos_sapling.Core.Client.Proving.spend_sig
          sk
          ar
          cv
          nf
          rk
          proof
          anti_replay
      in
      return
        Tezos_sapling.Core.Validator.UTXO.
          {cv; nf; rk; proof_i = proof; signature})
    to_forge

let init_fresh_sapling_state ctxt =
  let open Lwt_result_syntax in
  let* ctxt, id =
    Protocol.Lazy_storage_diff.fresh
      Protocol.Lazy_storage_kind.Sapling_state
      ~temporary:false
      ctxt
  in
  let* ctxt =
    Protocol.Sapling_storage.init ctxt id ~memo_size:0
    (* TODO CHECK *)
  in
  return (ctxt, id)

let generate_spending_and_viewing_keys state =
  let sk =
    Tezos_sapling.Core.Client.Spending_key.of_seed (random_bytes state 32)
  in
  let vk = Tezos_sapling.Core.Client.Viewing_key.of_sk sk in
  (sk, vk)

let prepare_seeded_state_internal ~(nb_input : int) ~(nb_nf : int)
    ~(nb_cm : int) (ctxt : Raw_context.t) (state : Random.State.t) :
    (Sapling_repr.diff
    * forge_info list
    * Tezos_sapling.Core.Client.Spending_key.t
    * Tezos_sapling.Core.Client.Viewing_key.t
    * Raw_context.t
    * Protocol.Lazy_storage_kind.Sapling_state.Id.t)
    tzresult
    Lwt.t =
  let open Lwt_result_syntax in
  let* ctxt, id =
    let*! result = init_fresh_sapling_state ctxt in
    Lwt.return (Environment.wrap_tzresult result)
  in
  let index_start = Tezos_sapling.Core.Client.Viewing_key.default_index in
  let sk, vk = generate_spending_and_viewing_keys state in
  let* diff, to_forge =
    generate_commitments
      ~vk
      ~nb_input
      ~nb_cm
      ~nb_nf
      ~diff:Protocol.Sapling_storage.empty_diff
      ~index:index_start
      state
  in
  let* ctxt, _size =
    let*! result = Protocol.Sapling_storage.apply_diff ctxt id (reverse diff) in
    Lwt.return (Environment.wrap_tzresult result)
  in
  return (diff, to_forge, sk, vk, ctxt, id)

let prepare_seeded_state
    {state_seed; nullifier_count; commitment_count; sapling_tx} ctxt =
  let open Lwt_result_syntax in
  let rng_state = Random.State.make [|Int64.to_int state_seed|] in
  let* diff, forge_info, spending_key, viewing_key, raw_ctxt, raw_id =
    prepare_seeded_state_internal
      ~nb_input:(List.length sapling_tx.inputs)
      ~nb_nf:(Int64.to_int nullifier_count)
      ~nb_cm:(Int64.to_int commitment_count)
      (alpha_to_raw ctxt)
      rng_state
  in
  let id = Protocol.Lazy_storage_kind.Sapling_state.Id.unparse_to_z raw_id in
  return (diff, forge_info, spending_key, viewing_key, raw_to_alpha raw_ctxt, id)

let generate ~(nb_input : int) ~(nb_output : int) ~(nb_nf : int) ~(nb_cm : int)
    ~(anti_replay : string) ~ctxt state =
  let open Lwt_result_syntax in
  assert (nb_input <= nb_cm) ;
  assert (nb_nf <= nb_cm - nb_input) ;
  let* diff, to_forge, sk, vk, ctxt, id =
    prepare_seeded_state_internal ~nb_input ~nb_nf ~nb_cm ctxt state
  in
  let local_state = state_from_rpc_diff diff in
  let root = Tezos_sapling.Storage.get_root local_state in
  let* transaction =
    Tezos_sapling.Core.Client.Proving.with_proving_ctx (fun proving_ctx ->
        let* inputs =
          make_inputs to_forge local_state proving_ctx sk vk root anti_replay
        in
        let output_amount, outputs = outputs nb_output proving_ctx vk state in
        let input_amount =
          List.fold_left
            (fun sum {amount; _} ->
              assert (
                Int64.compare
                  sum
                  (Int64.sub
                     Int64.max_int
                     Tezos_sapling.Core.Validator.UTXO.max_amount)
                < 0) ;
              Int64.add sum amount)
            0L
            to_forge
        in
        let balance = Int64.sub input_amount output_amount in
        let bound_data =
          (* The bound data are benched separately so we add
             empty bound data*)
          ""
        in
        let binding_sig =
          Tezos_sapling.Core.Client.Proving.make_binding_sig
            proving_ctx
            inputs
            outputs
            ~balance
            ~bound_data
            anti_replay
        in
        let transaction =
          Tezos_sapling.Core.Validator.UTXO.
            {inputs; outputs; binding_sig; balance; root; bound_data}
        in
        return transaction)
  in
  assert (Compare.List_length_with.(transaction.inputs = nb_input)) ;
  assert (Compare.List_length_with.(transaction.outputs = nb_output)) ;
  return (transaction, (ctxt, id))

(* ------------------------------------------------------------------------- *)
(* Nicely packaging sapling generation for snoop *)

let sapling_transition_encoding =
  let open Data_encoding in
  conv
    (fun {state_seed; nullifier_count; commitment_count; sapling_tx} ->
      (state_seed, nullifier_count, commitment_count, sapling_tx))
    (fun (state_seed, nullifier_count, commitment_count, sapling_tx) ->
      {state_seed; nullifier_count; commitment_count; sapling_tx})
    (obj4
       (req "state_seed" int64)
       (req "nullifier_count" int64)
       (req "commitment_count" int64)
       (req "sapling_tx" Alpha_context.Sapling.transaction_encoding))

let sapling_dataset_encoding = Data_encoding.list sapling_transition_encoding

let save ~filename ~txs =
  let str =
    match Data_encoding.Binary.to_string sapling_dataset_encoding txs with
    | Error err ->
        Format.eprintf
          "Sapling_generation.save: encoding failed (%a); exiting@."
          Data_encoding.Binary.pp_write_error
          err ;
        exit 1
    | Ok res -> res
  in
  ignore (* TODO handle error *)
    (Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.create_file filename str)

let load_file filename =
  let open Lwt_syntax in
  Lwt_main.run
  @@ let* str = Tezos_stdlib_unix.Lwt_utils_unix.read_file filename in
     Format.eprintf "Sapling_generation.load: loaded %s@." filename ;
     match Data_encoding.Binary.of_string sapling_dataset_encoding str with
     | Ok result ->
         let result = List.map (fun tx -> (filename, tx)) result in
         return result
     | Error err ->
         Format.eprintf
           "Sapling_generation.load: can't load file (%a); exiting@."
           Data_encoding.Binary.pp_read_error
           err ;
         exit 1

let get_all_sapling_data_files directory =
  let is_sapling_data file =
    let regexp = Str.regexp ".*\\.sapling" in
    Str.string_match regexp file 0
  in
  let lift file = directory ^ "/" ^ file in
  let handle = Unix.opendir directory in
  let rec loop acc =
    match Unix.readdir handle with
    | file -> if is_sapling_data file then loop (lift file :: acc) else loop acc
    | exception End_of_file ->
        Unix.closedir handle ;
        acc
  in
  loop []

type type_transaction = Empty | Non_empty

let load ~filename type_transaction =
  if not (Sys.file_exists filename) then (
    Format.eprintf "Sapling_generation.load: file does not exist@." ;
    Stdlib.failwith "Sapling_generation.load")
  else if Sys.is_directory filename then
    let () =
      Format.eprintf
        "Sapling_generation.load: loading all *.sapling files from directory \
         %s@."
        filename
    in
    let files = get_all_sapling_data_files filename in
    List.concat_map load_file files
    |> List.filter (fun (_str, transac) ->
           match type_transaction with
           | Empty ->
               List.is_empty transac.sapling_tx.outputs
               && List.is_empty transac.sapling_tx.inputs
           | Non_empty ->
               (not (List.is_empty transac.sapling_tx.outputs))
               || not (List.is_empty transac.sapling_tx.inputs))
  else load_file filename

let shared_seed = [|9798798; 217861209; 876786|]

let generate (save_to : string) (tx_count : int)
    (sapling_gen_options : sapling_gen_options) =
  let open Lwt_result_syntax in
  let result =
    Lwt_main.run
      (let {
         max_inputs;
         max_outputs;
         max_nullifiers;
         max_additional_commitments;
         seed;
       } =
         sapling_gen_options
       in
       let rng_state =
         (* /!\ This must match the seed used at benchmark time,
            defined in Runner.benchmark_sapling. /!\ *)
         Random.State.make
         @@ Option.fold ~none:shared_seed ~some:(fun seed -> [|seed|]) seed
       in
       let* ctxt, step_constants = Execution_context.make ~rng_state () in
       let address = Contract_hash.to_b58check step_constants.self in
       let chain_id =
         Environment.Chain_id.to_b58check step_constants.chain_id
       in
       let anti_replay = address ^ chain_id in
       let ctxt = alpha_to_raw ctxt in
       (match sapling_gen_options.seed with
       | None -> Random.self_init ()
       | Some seed -> Random.init seed) ;
       let seeds =
         Stdlib.List.init tx_count (fun i -> (i, Random.int 0x3FFFFFFF))
       in
       let rec loop seeds acc =
         match seeds with
         | [] -> return acc
         | (i, seed) :: tl ->
             let nb_input =
               if max_inputs = 0 then 0 else 1 + Random.int max_inputs
             in
             let nb_output =
               if max_outputs = 0 then 0 else 1 + Random.int max_outputs
             in
             let nb_nf = 1 + Random.int max_nullifiers in
             let nb_cm =
               nb_input + nb_nf + Random.int max_additional_commitments
             in
             let () =
               Format.eprintf "@." ;
               Format.eprintf "generating sapling tx %i/%d@." (i + 1) tx_count ;
               Format.eprintf "saving to file %s@." save_to ;
               Format.eprintf "nb_input = %d@." nb_input ;
               Format.eprintf "nb_output = %d@." nb_output ;
               Format.eprintf "nb_nf = %d@." nb_nf ;
               Format.eprintf "nb_cm = %d@." nb_cm ;
               Format.eprintf "anti_replay = %s@." anti_replay
             in
             let state = Random.State.make [|seed|] in
             let* tx, (_ctxt, _state_id) =
               generate
                 ~nb_input
                 ~nb_output
                 ~nb_nf
                 ~nb_cm
                 ~anti_replay
                 ~ctxt
                 state
             in
             let result =
               {
                 state_seed = Int64.of_int seed;
                 nullifier_count = Int64.of_int nb_nf;
                 commitment_count = Int64.of_int nb_cm;
                 sapling_tx = Obj.magic tx;
               }
             in
             loop tl (result :: acc)
       in
       loop seeds [])
  in
  match result with Ok txs -> save ~filename:save_to ~txs | Error _ -> ()

let apply_diff ctxt id diff =
  let open Lwt_result_syntax in
  let* ctxt, size = Sapling_storage.apply_diff (alpha_to_raw ctxt) id diff in
  return (raw_to_alpha ctxt, size)
