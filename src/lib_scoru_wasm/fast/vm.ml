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

let compute_until_snapshot ~max_steps pvm_state =
  Wasm_vm.compute_step_many_until
    ~max_steps
    (fun pvm_state ->
      Lwt.return
      @@
      match pvm_state.tick_state with
      | Snapshot -> false
      | _ -> Wasm_vm.should_compute pvm_state)
    pvm_state

let assert_decode = function Decode _ -> true | _ -> false

let compute_fast pvm_state =
  let open Lwt.Syntax in
  (* Move from [Snapshot] to [Decode], to patch the durable state. *)
  let* pvm_state = Wasm_vm.compute_step pvm_state in
  assert (assert_decode pvm_state.tick_state) ;
  (* Execute! *)
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4123
     Support performing multiple calls to [Eval.compute]. *)
  let durable = pvm_state.durable in
  let+ durable = Exec.compute durable pvm_state.buffers in
  (* Compute the new tick counter. *)
  let ticks = pvm_state.max_nb_ticks in
  let current_tick = Z.(add pvm_state.last_top_level_call ticks) in
  (* Revert the tick state to [Snapshot]. *)
  let tick_state = Snapshot in
  (* Assemble state *)
  let pvm_state = {pvm_state with durable; current_tick; tick_state} in
  (pvm_state, Z.to_int64 ticks)

let rec compute_step_many accum_ticks ?(after_fast_exec = fun () -> ())
    ?(stop_at_snapshot = false) ~max_steps pvm_state =
  let open Lwt.Syntax in
  assert (max_steps > 0L) ;
  let eligible_for_fast_exec =
    Z.Compare.(pvm_state.max_nb_ticks <= Z.of_int64 max_steps)
  in
  let backup pvm_state =
    let+ pvm_state, ticks =
      Wasm_vm.compute_step_many ~stop_at_snapshot ~max_steps pvm_state
    in
    (pvm_state, Int64.add accum_ticks ticks)
  in
  if eligible_for_fast_exec then
    let goto_snapshot_and_retry () =
      let* pvm_state, ticks = compute_until_snapshot ~max_steps pvm_state in
      match pvm_state.tick_state with
      | Snapshot when not stop_at_snapshot ->
          let max_steps = Int64.sub max_steps ticks in
          let accum_ticks = Int64.add accum_ticks ticks in
          let may_compute_more = Wasm_vm.should_compute pvm_state in
          if may_compute_more && max_steps > 0L then
            (compute_step_many [@tailcall])
              ~stop_at_snapshot
              accum_ticks
              ~max_steps
              pvm_state
          else Lwt.return (pvm_state, accum_ticks)
      | _ -> Lwt.return (pvm_state, ticks)
    in
    let go_like_the_wind () =
      let+ pvm_state, ticks = compute_fast pvm_state in
      after_fast_exec () ;
      (pvm_state, Int64.(add ticks accum_ticks))
    in
    match pvm_state.tick_state with
    | Snapshot -> (
        let* reboot_mark = Wasm_vm.mark_for_reboot pvm_state in
        match reboot_mark with
        | `Reboot -> Lwt.catch go_like_the_wind (fun _ -> backup pvm_state)
        | `Restarting | `Forcing_restart ->
            (* Go to next [Collect] state *)
            backup pvm_state)
    | _ -> goto_snapshot_and_retry ()
  else
    (* The number of ticks we're asked to do is lower than the maximum number
       of ticks for a top-level cycle. Fast Execution cannot be applied in this
       case. *)
    backup pvm_state

let compute_step_many = compute_step_many 0L

module Internal_for_tests = struct
  let compute_step_many_with_hooks = compute_step_many
end

let compute_step_many = compute_step_many ?after_fast_exec:None
