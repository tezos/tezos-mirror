(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  include Internal_event.Simple

  let section = Events.section @ ["shadow"]

  let started =
    declare_1
      ~section
      ~name:"shadow_kernel_started"
      ~msg:"shadow mode: blueprints are also applied with the kernel {kernel}"
      ~level:Notice
      ("kernel", Data_encoding.string)

  let no_block =
    declare_1
      ~section
      ~name:"shadow_kernel_no_block"
      ~msg:"shadow mode: the shadow kernel produced no block for level {level}"
      ~level:Warning
      ("level", Data_encoding.z)

  let failed =
    declare_2
      ~section
      ~name:"shadow_kernel_failed"
      ~msg:
        "shadow mode: cannot apply level {level} with the shadow kernel: \
         {trace}"
      ~level:Warning
      ~pp2:pp_print_trace
      ("level", Data_encoding.z)
      ("trace", Events.trace_encoding)
end

module Types = struct
  type parameters = {
    kernel_path : Pvm_types.kernel;
    data_dir : string;
    chain_family : L2_types.ex_chain_family;
    config : Pvm_types.config;
  }

  type state = {
    kernel : string;
    data_dir : string;
    chain_family : L2_types.ex_chain_family;
    config : Pvm_types.config;
    pool : Lwt_domain.pool;
        (** Pool dedicated to the shadow executions, so that they do not
            compete with the executions of the node (whose pool has a single
            domain). *)
  }
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "shadow_kernel"]

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t =
    | Apply : {
        level : Z.t;
        pre_state : Evm_state.t;
        chunks : Sequencer_blueprint.unsigned_chunked_blueprint;
      }
        -> (unit, tztrace) t

  type view = {level : Z.t}

  let view : type a b. (a, b) t -> view = function Apply {level; _} -> {level}

  let encoding =
    let open Data_encoding in
    conv (fun {level} -> level) (fun level -> {level}) (obj1 (req "level" z))

  let pp ppf {level} = Format.fprintf ppf "apply level %a" Z.pp_print level
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type t = Worker.infinite Worker.queue Worker.t

(** Puts the shadow kernel in [pre_state], in place of the kernel of the
    node. *)
let patch_state (state : Types.state) pre_state =
  Durable_storage.write
    Kernel_boot_wasm
    (Bytes.of_string state.kernel)
    pre_state

(** Logs the failure of the shadow execution [result] of level [level], if
    any. *)
let log_failure ~level result =
  match result with
  | Ok (Evm_state.Apply_success _) -> Lwt.return_unit
  | Ok Apply_failure -> Event.(emit no_block) level
  | Error trace -> Event.(emit failed) (level, trace)

(** Applies [chunks] on [pre_state] with the shadow kernel. It never fails: the
    shadow kernel is only observed, its failures are logged. *)
let apply (state : Types.state) ~level ~pre_state ~chunks =
  let open Lwt_syntax in
  let started_at = Unix.gettimeofday () in
  let* result =
    protect @@ fun () ->
    let open Lwt_result_syntax in
    let* shadow_pre_state = patch_state state pre_state in
    Metrics.set_shadow_patch_time ~seconds:(Unix.gettimeofday () -. started_at) ;
    let (Ex_chain_family chain_family) = state.chain_family in
    Evm_state.apply_unsigned_chunks
      ~pool:state.pool
      ~native_execution_policy:Never
      ~data_dir:state.data_dir
      ~chain_family
      ~config:state.config
      shadow_pre_state
      chunks
  in
  Metrics.set_shadow_execution_time ~seconds:(Unix.gettimeofday () -. started_at) ;
  Metrics.set_shadow_executed_level ~level ;
  let* () = log_failure ~level result in
  return_ok_unit

module Handlers = struct
  type self = t

  let on_request : type r request_error.
      t -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun w request ->
    match request with
    | Apply {level; pre_state; chunks} ->
        apply (Worker.state w) ~level ~pre_state ~chunks

  type launch_error = tztrace

  let on_launch _w () Types.{kernel_path; data_dir; chain_family; config} =
    let open Lwt_result_syntax in
    let* kernel, is_binary = Pvm.Kernel.read_kernel kernel_path in
    let*! kernel =
      if is_binary then Lwt.return kernel
      else Wasm_utils_functor.wat2wasm kernel
    in
    let*! () =
      Event.(emit started)
        (match kernel_path with
        | In_memory _ -> "<memory>"
        | On_disk path -> path)
    in
    return
      Types.
        {kernel; data_dir; chain_family; config; pool = Lwt_domain.setup_pool 1}

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    Lwt_result_syntax.return `Continue

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  (* Called before the state is dropped. Waits for the execution in progress
     on the pool, if any, which cannot be cancelled. The pool is torn down from
     another thread: the execution needs the main thread for each storage
     access. *)
  let on_close w =
    Lwt_preemptive.detach Lwt_domain.teardown_pool (Worker.state w).pool
end

let table = Worker.create_table Queue

let create ~kernel_path ~data_dir ~chain_family ~config =
  Worker.launch
    table
    ()
    {kernel_path; data_dir; chain_family = Ex_chain_family chain_family; config}
    (module Handlers)

let shutdown = Worker.shutdown

(** When the shadow kernel is slower than the node, requests pile up (each one
    holding a state in memory). Beyond this number of pending requests, blocks
    are not applied with the shadow kernel. *)
let max_pending = 8

let push_blueprint worker ~level ~pre_state ~chunks =
  if Worker.Queue.pending_requests_length worker >= max_pending then
    Metrics.inc_shadow_skipped ()
  else Worker.Queue.push_request_now worker (Apply {level; pre_state; chunks})
