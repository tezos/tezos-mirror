(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Bls
open Utils
open Identities
open Communication
module SMap = Plonk.SMap

module type S = sig
  module MP : Plonk.Main_protocol.S

  module Msg : Message.S

  module D :
    Distributed_wrapper.Enriched_process
      with type 'a M.step = 'a Msg.step
       and type 'a M.request = 'a Msg.request
       and type 'a M.reply = 'a Msg.reply
       and type 'a io = 'a Lwt.t

  val distributed_prover_main :
    workers:Distributed.Process_id.t list ->
    inputs:MP.prover_inputs ->
    MP.prover_public_parameters ->
    MP.proof D.t
end

module Make_common (MP : Distribution.Main_protocol.S) = struct
  module MP = MP
  module Msg = Message.Make (MP)
  module D = Distributed_wrapper.Make (Msg)

  let pc_distributed_prove_main ~workers pp transcript query_list answers_list
      secret_list prover_aux_list =
    let open MP.PP in
    let transcript =
      PC.distributed_expand_transcript transcript query_list answers_list
    in
    (* The main thread simulates a worker, since it is the only one who knows
       the information about t_map, g_map, plook_map. We need to pad a dummy
       secret and a dummy prover_aux at the end, corresponding to f_map, which
       the main thread does not have information about. *)
    let worker_message, state =
      PC.distributed_prove_main1
        pp
        transcript
        query_list
        answers_list
        (secret_list @ [SMap.empty])
        (prover_aux_list @ [PC.Commitment.empty_prover_aux])
    in
    let open D in
    let request content ~index = Msg.PC_Distribution {index; content} in
    let reply (Msg.PC_Distribution_res {content; _}) =
      Some (fun () -> return content)
    in
    let* prover_messages =
      dmap
        ~pids:workers
        ~request
        ~reply
        (List.init (List.length workers) (fun _ -> worker_message))
    in
    return (PC.distributed_prove_main2 state prover_messages)

  let pp_distributed_prove_aux ~workers pc_public_parameters transcript ~n
      ~generator ~secrets_main ~eval_points_main ~eval_points_worker
      ~evaluated_perm_ids ~nb_of_t_chunks =
    let open MP.PP in
    let open D in
    let module M = Msg in
    let alpha, transcript = Fr_generation.random_fr transcript in
    let* ids_keys =
      dmap
        ~pids:workers
        ~request:(fun content ~index -> M.PP_prepare_ids {index; content})
        ~reply:(function
          | M.PP_prepare_ids_res {content; _} -> Some (fun () -> return content))
        (List.map (Fun.const transcript) workers)
    in
    let perm_ids_keys, perm_ids =
      List.split @@ SMap.bindings evaluated_perm_ids
    in
    let all_ids_keys =
      List.concat (perm_ids_keys :: ids_keys) |> List.sort String.compare
    in
    let* batched_evaluated_ids_list =
      dmap
        ~pids:workers
        ~request:(fun content ~index -> M.PP_commit_to_t {index; content})
        ~reply:(function
          | M.PP_commit_to_t_res {content; _} -> Some (fun () -> return content))
        (List.map (Fun.const (all_ids_keys, alpha)) workers)
    in
    let batched_perm =
      let powers_map =
        SMap.of_list @@ List.mapi (fun i s -> (s, i)) all_ids_keys
      in
      let powers =
        List.map (fun s -> SMap.find s powers_map) perm_ids_keys
        |> List.map (fun i -> Scalar.pow alpha @@ Z.of_int i)
      in
      Evaluations.linear_c ~evaluations:perm_ids ~linear_coeffs:powers ()
    in
    let batched_ids =
      Evaluations.linear_c
        ~evaluations:(batched_perm :: batched_evaluated_ids_list)
        ()
    in
    (* batched_ids contains just one element, so [alpha] will not be used in
       the call to compute_t; this is intended *)
    let t =
      compute_t ~n ~alpha ~nb_of_t_chunks (SMap.singleton "batched" batched_ids)
    in
    let cm_t, t_prover_aux = PC.Commitment.commit pc_public_parameters t in
    let transcript = Transcript.expand PC.Commitment.t cm_t transcript in
    let* pc_answers_worker =
      dmap
        ~pids:workers
        ~request:(fun content ~index -> M.PP_KZG_eval_at_x {index; content})
        ~reply:(function
          | M.PP_KZG_eval_at_x_res {content; _} ->
              Some (fun () -> return content))
        (List.init (List.length workers) (Fun.const transcript))
    in
    let x, transcript = Fr_generation.random_fr transcript in
    let prover_aux_list_main = t_prover_aux :: List.map snd secrets_main in
    let polys_list_main = t :: List.map fst secrets_main in
    let eval_points_main = [X] :: eval_points_main in
    let query_list_main =
      List.map (convert_eval_points ~generator ~x) eval_points_main
    in
    let query_list_worker =
      List.map (convert_eval_points ~generator ~x) eval_points_worker
    in
    let pc_answers_main =
      List.map2 PC.evaluate polys_list_main query_list_main
    in
    let answer_list =
      pc_answers_main @ Plonk.List.mapn PC.merge_answers pc_answers_worker
    in
    return
      ( (alpha, x, answer_list, cm_t),
        polys_list_main,
        prover_aux_list_main,
        query_list_main @ query_list_worker,
        transcript )

  let pp_distributed_prove_main ~workers pc_public_parameters transcript
      (generator, n, nb_of_t_chunks) ~secrets_main ~eval_points_main
      ~eval_points_worker ~evaluated_perm_ids =
    let open D in
    let module M = Msg in
    let* ( (_, _, answer_list, cm_t),
           polys_list,
           prover_aux_list,
           query_list,
           transcript ) =
      pp_distributed_prove_aux
        ~workers
        pc_public_parameters
        transcript
        ~n
        ~generator
        ~secrets_main
        ~eval_points_main
        ~eval_points_worker
        ~evaluated_perm_ids
        ~nb_of_t_chunks
    in
    let* pc_proof, transcript, _aux =
      pc_distributed_prove_main
        ~workers
        pc_public_parameters
        transcript
        query_list
        answer_list
        polys_list
        prover_aux_list
    in
    return (MP.PP.{cm_t; pc_proof; pc_answers = answer_list}, transcript)

  let distributed_prover ~(workers : Distributed.Process_id.t list) ~pp_prove pp
      ~inputs =
    let open D in
    let open MP in
    let module M = Msg in
    let nb_workers = List.length workers in
    (* send the public inputs to the workers *)
    let workers_inputs_map = split_inputs_map ~nb_workers inputs in

    let* replies0 =
      dmap
        ~pids:workers
        ~request:(fun content ~index -> M.Commit_to_wires {index; content})
        ~reply:(fun (M.Commit_to_wires_res {content; _}) ->
          Some (fun () -> return content))
        workers_inputs_map
    in
    let commitment_wires = PP.PC.Commitment.recombine replies0 in

    (* update the transcript with the commitment to wires *)
    let transcript =
      Transcript.expand PP.PC.Commitment.t commitment_wires (get_transcript pp)
    in
    let* replies =
      dmap
        ~pids:workers
        ~request:(fun content ~index -> M.Commit_to_plook {index; content})
        ~reply:(fun (M.Commit_to_plook_res {content; _}) ->
          Some (fun () -> return content))
        (List.init nb_workers (Fun.const transcript))
    in
    let* _ = lift_io @@ Lwt_io.flush_all () in

    let randomness, transcript = build_gates_randomness transcript in

    let f_map_perm, evaluated_perm_ids, (cmt_perm, perm_prover_aux) =
      shared_perm_rc_argument pp nb_workers randomness inputs replies
    in

    let cmt_plook =
      PP.PC.Commitment.recombine (List.map (fun r -> r.cmt) replies)
    in
    let cmt_perm_and_plook = PP.PC.Commitment.recombine [cmt_plook; cmt_perm] in
    (* update the transcript with the commitment to perm and plook *)
    let transcript =
      Transcript.expand PP.PC.Commitment.t cmt_perm_and_plook transcript
    in
    let f_map_plook =
      SMap.union_disjoint_list (List.map (fun r -> r.f_map) replies)
    in
    let f_perm_and_plook_map = SMap.union_disjoint f_map_perm f_map_plook in
    let perm_and_plook_prover_aux =
      PP.PC.Commitment.recombine_prover_aux
        (perm_prover_aux :: List.map (fun r -> r.prover_aux) replies)
    in
    let secrets_main =
      make_secret pp (f_perm_and_plook_map, perm_and_plook_prover_aux)
    in
    let eval_points_main, eval_points_worker = make_eval_points pp in

    let params = get_gen_n_nbt pp in
    let* pp_proof, transcript =
      pp_prove
        ~workers
        (get_srs pp)
        transcript
        params
        ~secrets_main
        ~eval_points_main
        ~eval_points_worker
        ~evaluated_perm_ids
    in
    let* () = lift_io @@ Lwt_io.(flush stdout) in
    return
      ( pp_proof,
        transcript,
        ( cmt_perm_and_plook,
          commitment_wires,
          randomness.beta_perm,
          randomness.gamma_perm,
          randomness.beta_rc,
          randomness.gamma_rc,
          randomness.delta ) )

  let distributed_prover_main ~workers ~inputs
      (pp : MP.prover_public_parameters) =
    let open D in
    let open MP in
    check_no_zk pp ;
    let pp = filter_prv_pp_circuits pp inputs in
    let pp =
      update_prover_public_parameters
        (hash_verifier_inputs (to_verifier_inputs pp inputs))
        pp
    in
    let* pp_proof, _transcript, (perm_and_plook, wires_cm, _, _, _, _, _) =
      distributed_prover ~workers ~pp_prove:pp_distributed_prove_main pp ~inputs
    in
    return {perm_and_plook; wires_cm; pp_proof}
end

module PC_Kzg = Distribution.Kzg.Kzg_impl
module PP_Kzg = Distribution.Polynomial_protocol.Make (PC_Kzg)
module Main_Kzg = Distribution.Main_protocol.Make (PP_Kzg)
module PC_Pack = Distribution.Kzg_pack.Kzg_pack_impl
module PP_Pack =
  Distribution.Polynomial_protocol.MakeSuper
    (PC_Pack)
    (Main_Kzg.Input_commitment)
module Main_Pack = Distribution.Main_protocol.MakeSuper (PP_Pack)
module Make_aPlonk = Aplonk.Main_protocol.Make_impl (Main_Kzg) (Main_Pack)

module Super_impl (PI : Aplonk.Pi_parameters.S) = struct
  module MP = Make_aPlonk (PI)
  module Common = Make_common (Main_Pack)
  module D = Common.D
  module Msg = Common.Msg
  open PC_Pack

  let pc_distributed_prove_super_aggregation_main ~workers
      (pp : Public_parameters.prover) transcript query_list answers_list
      secret_list (prover_aux_list : Commitment.prover_aux list) =
    let open Common in
    let transcript = Transcript.list_expand query_t query_list transcript in
    let worker_message, state =
      distributed_prove_main1
        pp
        transcript
        query_list
        answers_list
        (secret_list @ [SMap.empty])
        (prover_aux_list @ [Commitment.empty_prover_aux])
    in
    let open D in
    let request content ~index = Msg.PC_Distribution {index; content} in
    let reply (Msg.PC_Distribution_res {content; _}) =
      Some (fun () -> return content)
    in
    let* prover_messages =
      dmap
        ~pids:workers
        ~request
        ~reply
        (List.init (List.length workers) (fun _ -> worker_message))
    in
    let proof, transcript, {s_list; r} =
      distributed_prove_main2 state prover_messages
    in
    return (proof, s_list, r, transcript)

  let pp_distributed_prove_super_aggregation_main ~workers pc_public_parameters
      transcript (generator, n, nb_of_t_chunks) ~commit_to_answers_map
      ~secrets_main ~eval_points_main ~eval_points_worker ~evaluated_perm_ids =
    let open Common in
    let open D in
    let open PP_Pack in
    let module M = Msg in
    let* ( (alpha, x, answer_list, cm_t),
           polys_list,
           prover_aux_list,
           query_list,
           transcript ) =
      pp_distributed_prove_aux
        ~workers
        pc_public_parameters
        transcript
        ~n
        ~generator
        ~secrets_main
        ~eval_points_main
        ~eval_points_worker
        ~evaluated_perm_ids
        ~nb_of_t_chunks
    in
    let t_answers, cms_answers, transcript =
      update_transcript_with_formatted_answers
        transcript
        commit_to_answers_map
        answer_list
    in
    let* pc_proof, s_list, r, transcript =
      pc_distributed_prove_super_aggregation_main
        ~workers
        pc_public_parameters
        transcript
        query_list
        answer_list
        polys_list
        prover_aux_list
    in
    return
      ( PP_Pack.
          ( {cm_t; pc_proof; pc_answers = []},
            {
              answers = answer_list;
              batch = s_list;
              alpha;
              x;
              r;
              cms_answers;
              t_answers;
            } ),
        transcript )

  let distributed_prove_super_aggregation ~workers
      (pp : Main_Pack.prover_public_parameters) ~input_commit_funcs ~inputs =
    let open Common in
    let open Main_Pack in
    let open D in
    (* TODO: can we commit only to the hidden pi?*)
    let public_inputs_map, cms_pi = hash_pi pp input_commit_funcs inputs in
    (* add the PI in the transcript *)
    let pp = update_prv_pp_transcript_with_pi pp cms_pi in
    let* ( (pp_proof, PP.{answers; batch; alpha; x; r; cms_answers; t_answers}),
           _transcript,
           (perm_and_plook, wires_cm, beta, gamma, beta_rc, gamma_rc, delta) ) =
      distributed_prover
        ~workers
        ~pp_prove:
          (pp_distributed_prove_super_aggregation_main
             ~commit_to_answers_map:
               (SMap.map (fun f -> f.answers) input_commit_funcs))
        pp
        ~inputs
    in
    let ids_batch =
      (* TODO #5551
         Implement Plookup
      *)
      let rd =
        {
          beta_perm = beta;
          gamma_perm = gamma;
          beta_plook = Scalar.one;
          gamma_plook = Scalar.one;
          beta_rc;
          gamma_rc;
          delta;
        }
      in
      compute_ids_batch pp rd alpha x public_inputs_map answers cms_answers
    in
    return
      ( {perm_and_plook; wires_cm; pp_proof},
        {
          answers;
          batch;
          alpha;
          beta;
          gamma;
          beta_rc;
          gamma_rc;
          delta;
          x;
          r;
          cms_answers;
          cms_pi;
          ids_batch;
          t_answers;
        } )

  let distributed_prover_main ~workers ~(inputs : MP.prover_inputs)
      (pp : MP.prover_public_parameters) =
    let open MP in
    let open D in
    let* distributed_proof =
      distributed_prove_super_aggregation
        ~workers
        pp.main_pp
        ~input_commit_funcs:(input_commit_funcs pp inputs)
        ~inputs
    in
    return (meta_proof pp inputs distributed_proof)
end

module Make (MP : Distribution.Main_protocol.S) : S = Make_common (MP)

module Super (PI : Aplonk.Pi_parameters.S) : S = Super_impl (PI)
