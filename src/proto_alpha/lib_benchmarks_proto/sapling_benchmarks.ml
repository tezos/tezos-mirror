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
module Size = Gas_input_size

let ns = Namespace.make Registration_helpers.ns "sapling"

let fv s = Free_variable.of_namespace (ns s)

module Apply_diff_bench : Benchmark.S = struct
  include Interpreter_benchmarks.Default_config
  include Interpreter_benchmarks.Default_boilerplate

  let name = ns "SAPLING_APPLY_DIFF"

  let info = "Benchmarking SAPLING_APPLY_DIFF"

  let tags = ["sapling"]

  let diff_from_tx (tx : Alpha_context.Sapling.transaction) =
    let open Environment.Sapling.UTXO in
    let commitments_and_ciphertexts =
      List.map (fun x -> (x.cm, x.ciphertext)) tx.outputs
    in
    {
      Protocol.Sapling_repr.commitments_and_ciphertexts;
      nullifiers = List.map (fun (x : input) -> x.nf) tx.inputs;
    }

  type workload = {nb_input : int; nb_output : int; nb_cm : int; nb_nf : int}

  let workload_encoding : workload Data_encoding.t =
    let open Data_encoding in
    def "diff_arg_encoding"
    @@ conv
         (fun {nb_input; nb_output; nb_cm; nb_nf} ->
           (nb_input, nb_output, nb_cm, nb_nf))
         (fun (nb_input, nb_output, nb_cm, nb_nf) ->
           {nb_input; nb_output; nb_cm; nb_nf})
         (tup4 Size.encoding Size.encoding Size.encoding Size.encoding)

  let workload_to_vector {nb_input; nb_output; nb_cm = _; nb_nf = _} =
    let l =
      [
        ("nb_input", float_of_int nb_input);
        ("nb_output", float_of_int nb_output);
      ]
    in
    Sparse_vec.String.of_list l

  let model =
    Model.make
      ~conv:(fun {nb_input; nb_output; _} -> (nb_input, (nb_output, ())))
      ~model:
        (Model.bilinear_affine
           ~name
           ~intercept:(fv "apply_diff_const")
           ~coeff1:(fv "apply_diff_inputs")
           ~coeff2:(fv "apply_diff_outputs"))

  let models = [("apply_diff", model)]

  let benchmark_apply_diff seed sapling_transition () =
    let sapling_forge_rng_state =
      Random.State.make
      @@ Option.fold
           ~none:Sapling_generation.shared_seed
           ~some:(fun seed -> [|seed|])
           seed
    in
    Lwt_main.run
      ( Execution_context.make ~rng_state:sapling_forge_rng_state
      >>=? fun (ctxt, step_constants) ->
        Sapling_generation.prepare_seeded_state sapling_transition ctxt
        >>=? fun (_, _, _, _, ctxt, state_id) ->
        let external_state_id = Alpha_context.Sapling.Id.parse_z state_id in
        let internal_state_id =
          Lazy_storage_kind.Sapling_state.Id.parse_z state_id
        in
        Alpha_context.Sapling.(state_from_id ctxt external_state_id)
        >|= Environment.wrap_tzresult
        >>=? fun (state, ctxt) ->
        Format.eprintf "state hash: %d@." (Hashtbl.hash state.diff) ;
        Format.eprintf
          "tx hash: %d@."
          (Hashtbl.hash sapling_transition.sapling_tx) ;
        let address = Contract_hash.to_b58check step_constants.self in
        let chain_id =
          Environment.Chain_id.to_b58check step_constants.chain_id
        in
        let anti_replay = address ^ chain_id in
        Format.eprintf "anti-replay: %s@." anti_replay ;
        let diff = diff_from_tx sapling_transition.sapling_tx in
        let closure () =
          ignore
            (Lwt_main.run
               (Sapling_generation.apply_diff ctxt internal_state_id diff))
        in
        let workload =
          {
            nb_input = List.length sapling_transition.sapling_tx.inputs;
            nb_output = List.length sapling_transition.sapling_tx.outputs;
            nb_cm = Int64.to_int sapling_transition.commitment_count;
            nb_nf = Int64.to_int sapling_transition.nullifier_count;
          }
        in
        return (Generator.Plain {workload; closure}) )
    |> function
    | Ok closure -> closure
    | Error errs ->
        Format.eprintf
          "Runner.benchmarkable_from_instr_str:\n%a@."
          (Format.pp_print_list Error_monad.pp)
          errs ;
        exit 1

  let create_benchmarks ~rng_state ~bench_num config =
    ignore rng_state ;
    match config.sapling with
    | {sapling_txs_file; seed} ->
        let transitions =
          Sapling_generation.load
            ~filename:sapling_txs_file
            Sapling_generation.Full_transaction
        in
        let length = List.length transitions in
        if length < bench_num then
          Format.eprintf
            "KSapling_verify_update: warning, only %d available transactions \
             (requested %d)@."
            length
            bench_num ;
        let transitions = List.take_n (min bench_num length) transitions in
        List.map
          (fun (_filename, tx) -> benchmark_apply_diff seed tx)
          transitions
end

let () = Registration_helpers.register (module Apply_diff_bench)
