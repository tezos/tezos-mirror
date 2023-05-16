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

open Communication

module type S = sig
  module Msg : Message.S

  module D :
    Distributed_wrapper.Enriched_process
      with type 'a M.step = 'a Msg.step
       and type 'a M.request = 'a Msg.request
       and type 'a M.reply = 'a Msg.reply
       and type 'a io = 'a Lwt.t

  val worker_proc : string -> Distributed.Process_id.t -> unit -> 'a D.t
end

module Make (Main : Distribution.Main_protocol.S) : S = struct
  module Msg = Message.Make (Main)
  module D = Distributed_wrapper.Make (Msg)
  open Main

  (**
  Worker loop.
  Implements the worker side of the proving protocol.
  *)
  let worker_proc pp_file main_pid () =
    let open D in
    let* () = lift_io @@ Lwt_io.printlf "Spawn" in
    let* fd = lift_io Lwt_io.(open_file ~mode:Input pp_file) in
    let* () = lift_io @@ Lwt_io.printlf "Open PP file" in
    let* s = lift_io @@ Lwt_io.read fd in
    let* () = lift_io @@ Lwt_io.printlf "Read PP file" in
    let pp =
      Plompiler.Utils.of_bytes prover_public_parameters_t (Bytes.of_string s)
    in
    let* () = lift_io @@ Lwt_io.close fd in
    let* () = lift_io @@ Lwt_io.printlf "Loaded PP\n" in
    let rec loop () =
      let* {
             all_f_wires;
             wires_list_map;
             inputs_map;
             shifts_map;
             cm_aux_wires;
             _;
           } =
        handle_request
          main_pid
          ~step:Msg.S_ctw
          ~handler:(fun (Msg.Commit_to_wires {index; content}) ->
            Some
              (fun () ->
                let content, rem = worker_commit_to_wires pp content in
                return (Msg.Commit_to_wires_res {index; content}, rem)))
      in
      let* {f_map; _}, {beta_plook; gamma_plook; beta_rc; gamma_rc} =
        handle_request
          main_pid
          ~step:Msg.S_ctp
          ~handler:(fun (Msg.Commit_to_plook {index; content}) ->
            Some
              (fun () ->
                let reply, rem =
                  commit_to_plook_rc pp shifts_map content wires_list_map
                in
                return
                  ( Msg.Commit_to_plook_res {index; content = reply},
                    (reply, rem) )))
      in
      let evaluations =
        Prover.build_evaluations pp (SMap.union_disjoint all_f_wires f_map)
      in
      let identities =
        Prover.build_gates_plook_rc1_identities
          ~shifts_map
          pp
          {
            beta_plook;
            gamma_plook;
            beta_rc;
            gamma_rc;
            beta_perm = Bls.Scalar.zero;
            gamma_perm = Bls.Scalar.zero;
            delta = Bls.Scalar.zero;
          }
          inputs_map
      in
      let evaluated_ids = identities evaluations in
      let ids_keys = SMap.bindings evaluated_ids |> List.map fst in
      let* _transcript =
        handle_request
          main_pid
          ~step:Msg.S_pppi
          ~handler:(fun (Msg.PP_prepare_ids {index; content = transcript}) ->
            Some
              (fun () ->
                return
                  ( Msg.PP_prepare_ids_res {index; content = ids_keys},
                    transcript )))
      in
      let* () =
        handle_request
          main_pid
          ~step:Msg.S_ppctt
          ~handler:(fun
                     (Msg.PP_commit_to_t {index; content = all_ids_keys, alpha})
                   ->
            Some
              (fun () ->
                let batched_ids =
                  batch_evaluated_ids ~alpha evaluated_ids all_ids_keys
                in
                return
                  (Msg.PP_commit_to_t_res {index; content = batched_ids}, ())))
      in
      let generator, _, _ = get_gen_n_nbt pp in
      let* () =
        handle_request
          main_pid
          ~step:Msg.S_ppkeax
          ~handler:(fun (Msg.PP_KZG_eval_at_x {index; content = transcript}) ->
            Some
              (fun () ->
                let secrets_worker = [(all_f_wires, cm_aux_wires)] in
                let r = kzg_eval_at_x pp transcript secrets_worker generator in
                return (Msg.PP_KZG_eval_at_x_res {index; content = r}, ())))
      in
      let* () =
        (* The next two functions are used to pad the query to
           [distributed_prover_worker] with dummy values in the positions that
           the worker does not have information about, namely, those of:
           t_map, g_map, plook_map, which are ATM handled by the main thread *)
        let pad_secrets l = (List.init 3 @@ Fun.const SMap.empty) @ l in
        let pad_prover_aux l =
          (List.init 3 @@ Fun.const PP.PC.Commitment.empty_prover_aux) @ l
        in
        handle_request main_pid ~step:Msg.S_pcd ~handler:(function
            | Msg.PC_Distribution {index; content = worker_msg} ->
            Some
              (fun () ->
                let secrets = pad_secrets [all_f_wires] in
                let prover_aux = pad_prover_aux [cm_aux_wires] in
                let prvr_main_msg =
                  PP.PC.distributed_prove_worker secrets prover_aux worker_msg
                in
                return
                  (Msg.PC_Distribution_res {index; content = prvr_main_msg}, ())))
      in
      loop ()
    in
    loop ()
end
