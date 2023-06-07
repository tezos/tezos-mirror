(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Tezos_scoru_wasm
open Wasm_pvm_state.Internal_state

include (Wasm_vm : Wasm_vm_sig.S)

let compute_until_snapshot ~max_steps ?write_debug pvm_state =
  Wasm_vm.compute_step_many_until
    ~max_steps
    ?write_debug
    (fun pvm_state ->
      Lwt.return
      @@
      match pvm_state.tick_state with
      | Snapshot -> false
      | _ -> Wasm_vm.should_compute pvm_state)
    pvm_state

let compute_fast ~reveal_builtins ~write_debug pvm_state =
  let open Lwt.Syntax in
  let* version = Wasm_vm.get_wasm_version pvm_state in
  (* Execute! *)
  let* durable =
    Exec.compute
      ~version
      ~reveal_builtins
      ~write_debug
      pvm_state.durable
      pvm_state.buffers
  in
  (* The WASM PVM does several maintenance operations at the end of
     each [kernel_run] call, using the very last [Padding]
     tick. Instead of replicating the same logic in the Fast VM, we
     reuse it by crafting the PVM state just before that critical
     tick. *)
  let ticks = pvm_state.max_nb_ticks in
  let current_tick = Z.(pred @@ add pvm_state.last_top_level_call ticks) in
  let pvm_state =
    {pvm_state with durable; current_tick; tick_state = Padding}
  in
  (* Here, we don't reuse the [write_debug] argument, because we are
     in the [Padding] stage of the WASM PVM execution, so there is no
     WASM execution during this tick.

     Calling [compute_step_with_debug ~write_debug] has a significant
     performance cost, because the host functions registry is
     reconstructed.

     As a consequence of these two facts, we call [compute_step] to
     avoid this penalty.*)
  let* pvm_state = Wasm_vm.compute_step pvm_state in

  Lwt.return pvm_state

let rec compute_step_many accum_ticks ?reveal_builtins
    ?(write_debug = Builtins.Noop) ?(after_fast_exec = fun () -> ())
    ?(stop_at_snapshot = false) ~max_steps pvm_state =
  let open Lwt.Syntax in
  assert (max_steps > 0L) ;
  let eligible_for_fast_exec =
    Z.Compare.(pvm_state.max_nb_ticks <= Z.of_int64 max_steps)
  in
  let inbox_snapshot =
    Tezos_webassembly_interpreter.Input_buffer.snapshot pvm_state.buffers.input
  in
  let* outbox_snapshot =
    Tezos_webassembly_interpreter.Output_buffer.snapshot
      pvm_state.buffers.output
  in
  let backup pvm_state =
    let+ pvm_state, ticks =
      Wasm_vm.compute_step_many
        ?reveal_builtins
        ~write_debug
        ~stop_at_snapshot
        ~max_steps
        {
          pvm_state with
          buffers = {input = inbox_snapshot; output = outbox_snapshot};
        }
    in
    (pvm_state, Int64.add accum_ticks ticks)
  in
  match reveal_builtins with
  | Some reveal_builtins when eligible_for_fast_exec -> (
      let goto_snapshot_and_retry () =
        let* pvm_state, ticks =
          compute_until_snapshot ~write_debug ~max_steps pvm_state
        in
        match pvm_state.tick_state with
        | Snapshot when not stop_at_snapshot ->
            let max_steps = Int64.sub max_steps ticks in
            let accum_ticks = Int64.add accum_ticks ticks in
            let may_compute_more = Wasm_vm.should_compute pvm_state in
            if may_compute_more && max_steps > 0L then
              (compute_step_many [@tailcall])
                accum_ticks
                ~reveal_builtins
                ~write_debug
                ~stop_at_snapshot
                ~after_fast_exec
                ~max_steps
                pvm_state
            else Lwt.return (pvm_state, accum_ticks)
        | _ -> Lwt.return (pvm_state, ticks)
      in
      let go_like_the_wind () =
        let* pvm_state = compute_fast ~write_debug ~reveal_builtins pvm_state in
        let accum_ticks =
          Int64.add accum_ticks (Z.to_int64 pvm_state.max_nb_ticks)
        in
        after_fast_exec () ;
        let max_steps =
          Int64.sub max_steps (Z.to_int64 pvm_state.max_nb_ticks)
        in
        if
          max_steps > 0L
          && Wasm_vm.should_compute pvm_state
          && not stop_at_snapshot
        then
          (compute_step_many [@tailcall])
            accum_ticks
            ~max_steps
            ~reveal_builtins
            ~write_debug
            ~stop_at_snapshot
            ~after_fast_exec
            pvm_state
        else Lwt.return (pvm_state, accum_ticks)
      in
      match pvm_state.tick_state with
      | Snapshot -> Lwt.catch go_like_the_wind (fun _ -> backup pvm_state)
      | _ -> goto_snapshot_and_retry ())
  | _ ->
      (* The number of ticks we're asked to do is lower than the maximum number
         of ticks for a top-level cycle or no builtins were supplied. Fast
         Execution cannot be applied in this case. *)
      backup pvm_state

let compute_step_many = compute_step_many 0L

let get_wasm_version = Wasm_vm.get_wasm_version

module Internal_for_tests = struct
  let compute_step_many_with_hooks = compute_step_many
end

let compute_step_many = compute_step_many ?after_fast_exec:None
