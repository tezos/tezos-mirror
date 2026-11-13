(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node / observer shadow kernel
    Invocation:   dune exec etherlink/tezt/tests/main.exe -- \
                    --file shadow_kernel.ml
    Subject:      An observer started with [--shadow-kernel] also applies each
                  blueprint with the shadow kernel, on the same pre-state,
                  and never fails because of it.
*)

open Test_helpers

(* Reads a metric of [evm_node], [None] if it is not (yet) exported. *)
let metric evm_node name =
  let* body =
    Runnable.run
    @@ Curl.get_raw
         ~name:("curl#" ^ Evm_node.name evm_node)
         (Evm_node.endpoint evm_node ^ "/metrics")
  in
  let re = rex (sf "^%s ([0-9.]+)$" (Re.Str.quote name)) in
  List.find_map
    (fun line -> line =~* re |> Option.map float_of_string)
    (String.split_on_char '\n' body)
  |> return

(* Waits until the observer has applied [level] with the shadow kernel (or
   skipped it, which the tests prevent by producing few blocks). *)
let wait_for_shadow_execution observer ~level =
  let name = "octez_evm_node_shadow_executed_level" in
  let rec loop attempts =
    let* executed = metric observer name in
    match executed with
    | Some executed when executed >= float_of_int level -> unit
    | _ when attempts = 0 ->
        Test.fail "the shadow kernel did not apply the level %d in time" level
    | _ ->
        let* () = Lwt_unix.sleep 0.5 in
        loop (attempts - 1)
  in
  loop 240

let produce_blocks {sandbox; observer} ~nb_blocks =
  let open Rpc.Syntax in
  let* () =
    Lwt_list.iter_s
      (fun level ->
        let*@ (_ : int) = Rpc.produce_block sandbox in
        Evm_node.wait_for_blueprint_applied observer level)
      (List.init nb_blocks (fun i -> i + 1))
  in
  wait_for_shadow_execution observer ~level:nb_blocks

let shadow_arguments ~shadow_kernel =
  ["--shadow-kernel"; Uses.path shadow_kernel]

let register_shadow_test ~title ~tags ~sequencer_kernel ~shadow_kernel body =
  register_sandbox_with_observer
    ~__FILE__
    ~kernel:sequencer_kernel
    ~title
    ~tags:("shadow" :: "observer" :: tags)
    ~extra_uses:[shadow_kernel]
    ~observer_extra_arguments:(fun () -> shadow_arguments ~shadow_kernel)
    body

(* Levels on which the shadow kernel of [observer] failed, in the order the
   failures are logged. *)
let record_shadow_failures observer =
  let failures = ref [] in
  Evm_node.on_event observer (fun {name; value; _} ->
      let level =
        match name with
        | "shadow_kernel_no_block.v0" -> Some JSON.(value |> as_string)
        | "shadow_kernel_failed.v0" ->
            Some JSON.(value |-> "level" |> as_string)
        | _ -> None
      in
      Option.iter
        (fun level -> failures := int_of_string level :: !failures)
        level) ;
  fun () -> List.rev !failures

let nb_blocks = 3

let test_same_kernel_does_not_fail () =
  register_shadow_test
    ~title:"Shadow kernel: the same kernel applies every blueprint"
    ~tags:["same_kernel"]
    ~sequencer_kernel:Kernel.Latest
    ~shadow_kernel:(Kernel.to_uses Kernel.Latest)
  @@ fun setup ->
  let failures = record_shadow_failures setup.observer in
  let* () = produce_blocks setup ~nb_blocks in
  Check.(
    (failures () = [])
      (list int)
      ~error_msg:"The shadow kernel should not fail, it failed at levels %L") ;
  unit

(* The sink kernel does nothing, hence produces no block: the shadow
   execution fails on each blueprint, which is logged, whereas the node
   applies the blueprints as usual. *)
let test_shadow_kernel_without_block () =
  register_shadow_test
    ~title:"Shadow kernel: a kernel producing no block does not fail the node"
    ~tags:["failure"]
    ~sequencer_kernel:Kernel.Latest
    ~shadow_kernel:Constant.WASM.sink
  @@ fun setup ->
  let last_failure =
    Evm_node.wait_for setup.observer "shadow_kernel_no_block.v0" (fun json ->
        if JSON.as_string json = string_of_int nb_blocks then Some () else None)
  in
  let* () = produce_blocks setup ~nb_blocks in
  last_failure

(* With the preconfirmation stream, the observer assembles its blocks from the
   transactions it already executed instead of applying their blueprint: there
   is nothing the shadow kernel could apply. *)
let test_shadow_kernel_refused_with_preconfirmations () =
  let shadow_kernel = Kernel.to_uses Kernel.Latest in
  register_shadow_test
    ~title:"Shadow kernel: refused with the preconfirmation stream"
    ~tags:["preconfirmation"]
    ~sequencer_kernel:Kernel.Latest
    ~shadow_kernel
  @@ fun {observer; _} ->
  let* () = Evm_node.terminate observer in
  let* () =
    Evm_node.Config_file.update
      observer
      (Evm_node.patch_config_with_experimental_feature
         ~preconfirmation_stream_enabled:true
         ())
  in
  Process.check_error
    ~msg:(rex "cannot be used with the preconfirmation stream")
    (Evm_node.spawn_run
       ~extra_arguments:(shadow_arguments ~shadow_kernel)
       observer)

let () =
  test_same_kernel_does_not_fail () ;
  test_shadow_kernel_without_block () ;
  test_shadow_kernel_refused_with_preconfirmations ()
