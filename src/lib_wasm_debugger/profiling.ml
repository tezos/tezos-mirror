(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Tezos_lazy_containers
open Tezos_webassembly_interpreter
module Vector = Lazy_vector.Int32Vector

(** Call stack representation and construction. *)

(** The call stack computation algorithm is the following:

    There are two components: the current node (or stack frame) and the
    continuation (a list of stack frames). There's a "toplevel node" describing
    the execution at the toplevel of the interpreter.
    A node contains:
    - [id]: a function call representation (an identifier)
    - [t]: the ticks elapsed during the call
    - [time]: the time elapsed during the call
    - [sub]: the subcalls.

    Note that for the rest of the algorithm, `time` will be eluded as its
    computation is equivalent to the ticks.

    The algorithm starts with an empty toplevel and an empty continuation.
    - on function call (id, current_tick, current_node, continuation):
    1. create a node N_id: (id, t: current_tick, sub:[])
    2. update current_node N_curr with t:(current_tick - t)
      => the number of ticks is now the diff between the moment the call started
          and the subcall started.
    3. push N_curr on the continuation
    4. return N_id, continuation

    - on function end (current_tick, current_node, continuation):
    1. update current_node N_curr with t:(current_tick - t)
    2. pop N_prev from the continuation
    3. update N_prev: t:(current_tick - t) sub:(sub + N_curr)
    4. return N_prev, continuation

    Let's take an example:
    call: f () \{ ...... g () \{ .... h () \{ ...... \} .......... \} ......... \}
    tick: 0 ----------> 10 -------> 30 ---------> 60 --------> 100 -----> 160
          [  10 ticks  ] [ 20 ticks ] [ 30 ticks ] [ 40 ticks ] [ 60 ticks ]

    - `f` takes 10 + 60 = 70 ticks
    - `g` takes 20 + 40 = 60 ticks
    - `h` takes 30 ticks

    T ([nodes]) : toplevel
    K : continuation (list)
    N : current node (N(id) means it hasn't changed from previous step)

    N, K |- exec

    Start:
    T, [] |- f () \{ g () \{ h () \{ \} \} \}
    ==> at tick 0
    N (f, 0, []), [T] |- g () \{ h () \{ \} \} \}
    ==> at tick 10
    N (g, 10, []), [N (f, 10 - 0 = 10, []); T] |- h () \{ \} \} \}
    ==> at tick 30
    N (h, 30, []), [N (g, 30 - 10 = 20, []); N(f); T] |- \} \} \}
    ==> at tick 60
    N (g, 60 - 20 = 40, [N (h, 60 - 30 = 30, [])]), [N(f); T] |- \} \}
    ==> at tick 100
    N (f, 100 - 10 = 90, [N (g, 100 - 40 = 60, [N(h)])]), [T] |- \}
    ==> at tick 160
    T [N (f, 160 - 90 = 70, [N(g, 60, [N(h, 30, [])])])], [] |- _

*)

type 'function_call call_stack =
  | Node of
      'function_call * Z.t * Ptime.span option * 'function_call call_stack list
  | Toplevel of 'function_call call_stack list

let rec fold_call_stack f acc = function
  | Node (call, ticks, time, substacks) ->
      List.fold_left (fold_call_stack f) (f acc call ticks time) substacks
  | Toplevel substacks -> List.fold_left (fold_call_stack f) acc substacks

(* [sub_opt_time t1 t2] returns `Some (t1 - t2)` if both are not `None`, and returns
   `None` otherwise. This function propagates the non usage of time if
   `--without-time` is specified by the profiler. *)
let sub_opt_times t1 t2 =
  match (t1, t2) with
  | Some t1, Some t2 -> Some (Ptime.Span.sub t1 t2)
  | _, _ -> None

(* Same semantics as [sub_opt_times] but returns [Some (t1 + t2)] instead. *)
let add_opt_times t1 t2 =
  match (t1, t2) with
  | Some t1, Some t2 -> Some (Ptime.Span.add t1 t2)
  | _, _ -> None

(** [end_function_call current_tick current_function call_stack] implements an
    ending call. Please refer to the prelude of the file. *)
let end_function_call current_tick current_time current_function call_stack =
  let current_time = current_time () in
  match current_function with
  | Node (call, starting_tick, starting_time, subcalls) -> (
      let tick = Z.sub current_tick starting_tick in
      let time = sub_opt_times current_time starting_time in
      let final_node = Node (call, tick, time, List.rev subcalls) in
      match call_stack with
      | [] -> assert false
      | Toplevel finalized :: stack ->
          (Toplevel (final_node :: finalized), stack)
      | Node (call, ticks, time, subcalls) :: stack ->
          ( Node
              ( call,
                Z.sub current_tick ticks,
                sub_opt_times current_time time,
                final_node :: subcalls ),
            stack ))
  (* A toplevel call cannot reduce. *)
  | Toplevel _ -> (current_function, call_stack)

(** [call_function called_function current_tick current_function call_stack]
    implements a function start. Please refere to the prelude of the module. *)
let call_function called_function current_tick current_time current_function
    call_stack =
  let current_time = current_time () in
  match current_function with
  | Toplevel _ as top ->
      let func = Node (called_function, current_tick, current_time, []) in
      (func, top :: call_stack)
  | Node (current_call, ticks, time, subcalls) ->
      let stack =
        Node
          ( current_call,
            Z.sub current_tick ticks,
            sub_opt_times current_time time,
            subcalls )
        :: call_stack
      in
      let func = Node (called_function, current_tick, current_time, []) in
      (func, stack)

(** Profiling the execution of the PVM *)

(** A function call can be either a direct call, a call through a reference or
    an internal step of the PVM. *)
type function_call =
  | Function of string
  | CallDirect of int32
  | CallRef of int32
  | Internal of string

let pp_call ppf = function
  | Function f -> Format.fprintf ppf "%s" f
  | CallDirect i -> Format.fprintf ppf "function[%ld]" i
  | CallRef i -> Format.fprintf ppf "function_ref[%ld]" i
  | Internal s -> Format.fprintf ppf "%%interpreter(%s)" s

(** [initial_eval_call] is `kernel_run` function call. *)
let initial_eval_call = Function Constants.wasm_entrypoint

(** [update_on_decode current_tick current_call_state] starts and stop
    `internal` calls related to the {b Decode} step of the PVM. *)
let update_on_decode current_tick current_time (current_node, call_stack) =
  let open Lwt_syntax in
  function
  | Decode.MKStart ->
      return_some
      @@ call_function
           (Internal "decode")
           current_tick
           current_time
           current_node
           call_stack
  | Decode.MKStop _ ->
      let current_node, call_stack =
        end_function_call current_tick current_time current_node call_stack
      in
      return_some
      @@ call_function
           (Internal "link")
           current_tick
           current_time
           current_node
           call_stack
  | _ -> return_none

(** [update_on_link current_tick current_call_state] starts and stop
    `internal` call to the {b Link} step of the PVM. *)
let update_on_link current_tick current_time (current_node, call_stack) module_
    imports_offset =
  let open Lwt_syntax in
  if imports_offset >= Vector.num_elements module_.Source.it.Ast.imports then
    let current_node, call_stack =
      end_function_call current_tick current_time current_node call_stack
    in
    return_some
    @@ call_function
         (Internal "init")
         current_tick
         current_time
         current_node
         call_stack
  else return_none

(** [update_on_init current_tick current_call_state] starts and stop
    `internal` call to the {b Init} step of the PVM. *)
let update_on_init current_tick current_time (current_node, call_stack) =
  let open Lwt_syntax in
  function
  | Eval.IK_Stop ->
      let current_node, call_stack =
        end_function_call current_tick current_time current_node call_stack
      in
      return_some
      @@ call_function
           initial_eval_call
           current_tick
           current_time
           current_node
           call_stack
  | _ -> return_none

(** [update_on_instr current_tick current_node call_stack] handle function calls
    during the evaluation. *)
let update_on_instr current_tick current_time current_node call_stack symbols =
  function
  | Eval.Plain (Ast.Call f) ->
      let id =
        match Custom_section.FuncMap.find f.Source.it symbols with
        | None -> CallDirect f.Source.it
        | Some f -> Function f
      in
      Lwt.return_some
        (call_function id current_tick current_time current_node call_stack)
  | Eval.Plain (CallIndirect (f, _)) ->
      Lwt.return_some
        (call_function
           (CallRef f.Source.it)
           current_tick
           current_time
           current_node
           call_stack)
  | _ -> Lwt.return_none

(** [update_on_eval current_tick current_call_state] handle function calls and
    end during the evaluation. *)
let update_on_eval current_tick current_time (current_node, call_stack) symbols
    =
  let open Lwt_syntax in
  function
  (* Instruction evaluation step *)
  | Eval.(SK_Next (_, _, LS_Start (Label_stack (label, _)))) ->
      let _, es = label.Eval.label_code in
      if 0l < Vector.num_elements es then
        let* e = Vector.get 0l es in
        update_on_instr
          current_tick
          current_time
          current_node
          call_stack
          symbols
          e.Source.it
      else return_none
  (* Labels `result` or `trapped` implies the end of a function call and the pop of
     the current stack frame, this can be interpreted as an end of the current
     function. *)
  | SK_Start ({frame_label_kont = Label_trapped _ | Label_result _; _}, _) ->
      return_some
      @@ end_function_call current_tick current_time current_node call_stack
  (* An invocation of function that doesn't return a new stack frame implies the
     current function is an host function, and it is the end of its call. *)
  | SK_Next
      ( _,
        _,
        LS_Craft_frame
          (Label_stack (_, _), Inv_stop {fresh_frame = None; remaining_ticks; _})
      )
    when Z.equal Z.zero remaining_ticks ->
      return_some
      @@ end_function_call current_tick current_time current_node call_stack
  | _ -> return_none

(** [update_call_stack current_tick current_state_call symbols state] returns
    the call state changes for any state. Returns [None] if no change
    happened. *)
let update_call_stack current_tick current_time (current_node, call_stack)
    symbols state =
  let open Lwt_syntax in
  match state with
  | Wasm_pvm_state.Internal_state.Decode {Decode.module_kont; _} ->
      update_on_decode
        current_tick
        current_time
        (current_node, call_stack)
        module_kont
  | Link {ast_module; imports_offset; _} ->
      update_on_link
        current_tick
        current_time
        (current_node, call_stack)
        ast_module
        imports_offset
  | Init {init_kont; _} ->
      update_on_init
        current_tick
        current_time
        (current_node, call_stack)
        init_kont
  | Eval {config = {step_kont; _}; _} ->
      update_on_eval
        current_tick
        current_time
        (current_node, call_stack)
        symbols
        step_kont
  | _ -> return_none

module State : sig
  (** Kinds of special `write_debug` messages the profiler can handle. Messages
     are of the form `__wasm_debugger__::<debug_call>(<data>)` *)
  type debug_call = Start_section of string | End_section of string

  (** The profiling state is the internal state built by the profiler. It is
      defined as mutable as it is updated by side effects during the
      profiling. *)
  type t = private {
    mutable call_stack :
      function_call call_stack * function_call call_stack list;
    mutable kernel_runs : function_call call_stack option list;
    mutable sections : (Z.t * string) list;
    mutable debug_call : debug_call option;
  }

  type should_compute := Wasm_pvm_state.Internal_state.pvm_state -> bool Lwt.t

  (** [init ~symbols ~current_time] initializes the profiler state and returns
      the instrumented `should_compute` function expected by the WASM PVM and
      the instrumented `write_debug` backend. This function updates the profiler
      state by side effects during the compilation. *)
  val init :
    symbols:string Custom_section.FuncMap.t ->
    with_time:bool ->
    reveal_builtins:Builtins.reveals option ->
    write_debug:Builtins.write_debug option ->
    t * should_compute * Builtins.write_debug option

  val finalized_runs : t -> function_call call_stack option list
end = struct
  type debug_call = Start_section of string | End_section of string

  type t = {
    mutable call_stack :
      function_call call_stack * function_call call_stack trace;
    mutable kernel_runs : function_call call_stack option trace;
    mutable sections : (Z.t * string) list;
    mutable debug_call : debug_call option;
  }

  let init_profiling_state () =
    {
      call_stack = (Toplevel [], []);
      kernel_runs = [];
      sections = [];
      debug_call = None;
    }

  (* Successive kernel runs are pushed on the stack. If one of the unresolved
     function calls stack is not empty at the end of a kernel run,
     its result might be inconsistent and the profiling has failed. *)
  let push_kernel_run profiler_state =
    match profiler_state.call_stack with
    | (Toplevel _ as run), [] ->
        profiler_state.kernel_runs <- Some run :: profiler_state.kernel_runs ;
        profiler_state.call_stack <- (Toplevel [], [])
    | _ ->
        profiler_state.kernel_runs <- None :: profiler_state.kernel_runs ;
        profiler_state.call_stack <- (Toplevel [], [])

  let finalized_runs profiler_state =
    (List.fold_left
       (fun runs graph ->
         match graph with
         | Some (Toplevel l) -> Some (Toplevel (List.rev l)) :: runs
         | n -> n :: runs)
       [])
      profiler_state.kernel_runs

  (** Specific debug calls semantics.

      The profiler adds some semantics to `write_debug` calls starting with
      `__wasm_debugger__`. These calls can be interpreted to print information
      that are specific to the profiling, such as the current tick or ticks
      elapsed between two given points. *)

  let parse_debug_call call =
    let open Option_syntax in
    let* fct = Re.Group.get_opt call 1 in
    let* params = Re.Group.get_opt call 2 in
    match fct with
    | "start_section" -> Some (Start_section params)
    | "end_section" -> Some (End_section params)
    | _ -> None

  let debugger_calls =
    Re.compile (Re.Perl.re "__wasm_debugger__::(\\w*)\\(([\x00-\xff]*)\\)")

  let is_debug_call s =
    Option.bind (Re.exec_opt debugger_calls s) parse_debug_call

  let reset_debug_call state = state.debug_call <- None

  let start_section pvm_state profiler_state data =
    profiler_state.sections <-
      (pvm_state.Wasm_pvm_state.Internal_state.current_tick, data)
      :: profiler_state.sections

  let close_section pvm_state profiler_state end_data =
    match profiler_state.sections with
    | (start_tick, start_data) :: sections ->
        let tick =
          Z.sub pvm_state.Wasm_pvm_state.Internal_state.current_tick start_tick
        in
        Format.printf
          "__wasm_debugger__::Section{ticks:%s;data:(0x%s,0x%s)}\n%!"
          (Z.to_string tick)
          (Hex.of_string start_data |> Hex.show)
          (Hex.of_string end_data |> Hex.show) ;
        profiler_state.sections <- sections
    | [] -> ()

  let handle_debug_call pvm_state profiler_state =
    match profiler_state.debug_call with
    | Some (Start_section data) ->
        start_section pvm_state profiler_state data ;
        reset_debug_call profiler_state
    | Some (End_section end_data) ->
        close_section pvm_state profiler_state end_data ;
        reset_debug_call profiler_state
    | None -> ()

  (* Instruments the given `write_debug` backend to handle the specific debug
     commands. *)
  let build_write_debug write_debug profiler_state =
    match write_debug with
    | Some (Builtins.Printer f) ->
        Some
          (Builtins.Printer
             (fun s ->
               match is_debug_call s with
               | Some c ->
                   profiler_state.debug_call <- Some c ;
                   Lwt.return_unit
               | None -> f s))
    | w -> w

  (* [update_state_on_step profiler_state pvm_state symbols current_time] updates the
     callstack according to the current instruction. *)
  let update_state_on_step profiler_state pvm_state symbols current_time =
    let open Lwt_syntax in
    let+ updated_stack =
      update_call_stack
        pvm_state.Wasm_pvm_state.Internal_state.current_tick
        current_time
        profiler_state.call_stack
        symbols
        pvm_state.tick_state
    in
    handle_debug_call pvm_state profiler_state ;
    Option.iter
      (fun (current_node, current_call_stack) ->
        profiler_state.call_stack <- (current_node, current_call_stack))
      updated_stack ;
    if pvm_state.tick_state = Snapshot || pvm_state.tick_state = Collect then
      push_kernel_run profiler_state

  let instrument_should_compute symbols current_time reveal_builtins
      profiler_state pvm_state =
    let open Lwt_syntax in
    let* () =
      update_state_on_step profiler_state pvm_state symbols current_time
    in
    let* input_request_val = Wasm_vm.get_info pvm_state in
    match input_request_val.input_request with
    | Reveal_required _ when reveal_builtins <> None -> return_true
    | Input_required | Reveal_required _ -> return_false
    | _ -> return_true

  let init ~symbols ~with_time ~reveal_builtins ~write_debug =
    let profiler_state = init_profiling_state () in
    (* [current_time] is defined as a closure instead as a direct call to avoid
       calling it at each tick and avoid a non necessary system call. *)
    let current_time () =
      if with_time then Some (Time.System.now () |> Ptime.to_span) else None
    in
    ( profiler_state,
      instrument_should_compute
        symbols
        current_time
        reveal_builtins
        profiler_state,
      build_write_debug write_debug profiler_state )
end

module Make (Wasm_utils : Wasm_utils_intf.S) = struct
  (** [eval_and_profile ?write_debug ?reveal_builtins symbols tree] profiles a
    kernel up to the next result, and returns the call stack. *)
  let eval_and_profile ?write_debug ?reveal_builtins ~with_time ~no_reboot
      symbols tree =
    let open Lwt_syntax in
    (* Initialize the state and the instrumented `should_compute` function. *)
    let profiler_state, instrumented_should_compute, instrumented_write_debug =
      State.init ~symbols ~with_time ~reveal_builtins ~write_debug
    in

    let rec eval_until_input_requested accumulated_ticks tree =
      let* pvm_state =
        Wasm_utils.Tree_encoding_runner.decode Wasm_pvm.pvm_state_encoding tree
      in
      let* info = Wasm_utils.Wasm.get_info tree in
      let run () =
        let* tree, ticks =
          Wasm_utils.Wasm.Internal_for_tests.compute_step_many_until
            ~wasm_entrypoint:Constants.wasm_entrypoint
            ?write_debug:instrumented_write_debug
            ?reveal_builtins
            ~max_steps:(Z.to_int64 pvm_state.max_nb_ticks)
            instrumented_should_compute
            tree
        in
        let* pvm_state =
          Wasm_utils.Tree_encoding_runner.decode
            Wasm_pvm.pvm_state_encoding
            tree
        in
        let accumulated_ticks = Z.add accumulated_ticks @@ Z.of_int64 ticks in
        if no_reboot && pvm_state.tick_state = Snapshot then
          return (tree, accumulated_ticks)
        else eval_until_input_requested accumulated_ticks tree
      in
      match info.Wasm_pvm_state.input_request with
      | No_input_required -> run ()
      | Reveal_required _ when reveal_builtins <> None -> run ()
      | Input_required | Reveal_required _ -> return (tree, accumulated_ticks)
    in
    let+ tree, ticks = eval_until_input_requested Z.zero tree in
    let kernel_runs = State.finalized_runs profiler_state in
    (tree, ticks, kernel_runs)
end

(** Flamegraph building

    Flamegraph are an aggregation of all the same callstacks, thus there is no
    longer a notion of time. We can easily collapse all nodes into a single one.
*)

module StringMap = Map.Make (String)

(** [collapse_stack ~max_depth pp_call call_stack] collapses a call stack into a
    valid flamegraph. Node deeper than [max_depth] are not considered. [pp_call]
    is used to print the identifiers. *)
let collapse_stack ~max_depth pp_call call_stack =
  let rec handle_node ~prefix ~depth ~max_depth accumulated_nodes = function
    | Node (name, ticks, _time, subnodes) ->
        let prefix =
          if prefix = "" then Format.asprintf "%a" pp_call name
          else Format.asprintf "%s;%a" prefix pp_call name
        in
        let map =
          StringMap.update
            prefix
            (function
              | None -> Some ticks | Some prev -> Some (Z.add prev ticks))
            accumulated_nodes
        in
        handle_nodes ~prefix ~depth:(succ depth) ~max_depth map subnodes
    | Toplevel nodes ->
        handle_nodes ~prefix ~depth ~max_depth accumulated_nodes nodes
  and handle_nodes ~prefix ~depth ~max_depth accumulated_nodes nodes =
    if depth > max_depth then accumulated_nodes
    else
      List.fold_left
        (fun acc node -> handle_node ~prefix ~depth ~max_depth acc node)
        accumulated_nodes
        nodes
  in
  handle_node ~prefix:"" ~depth:0 ~max_depth StringMap.empty call_stack
  |> StringMap.bindings

(** Pretty printing and flamegraph output *)

(** [pp_indent ppf depth] prints an indentation corresponding to the given
    [depth]. *)
let pp_indent ppf depth = Format.fprintf ppf "%s" (String.make (depth * 2) ' ')

let pp_time_opt ppf = function
  | None -> ()
  | Some time -> Format.fprintf ppf " (%a)" Ptime.Span.pp time

let rec pp_nodes ?(max_depth = 10) depth pp_call ppf nodes =
  if depth > max_depth then ()
  else
    Format.fprintf
      ppf
      "\n%a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
         (pp_stack (depth + 1) pp_call))
      nodes

and pp_stack ?max_depth depth pp_call ppf = function
  | Node (call, ticks, time, sub_nodes) ->
      Format.fprintf
        ppf
        "%a- %a : %a ticks%a%a"
        pp_indent
        depth
        pp_call
        call
        Z.pp_print
        ticks
        pp_time_opt
        time
        (pp_nodes ?max_depth depth pp_call)
        sub_nodes
  | Toplevel nodes -> pp_nodes ?max_depth depth pp_call ppf nodes

(** [pp_stack ~max_depth ppf stack] pretty prints the stack. It should be used
    for debug only. *)
let pp_stack ?max_depth = pp_stack ?max_depth 0

let rec pp_flame_callstack_node ~prefix ~depth ~max_depth pp_call ppf = function
  | Node (call, ticks, _time, subnodes) ->
      let prefix =
        if prefix = "" then Format.asprintf "%a" pp_call call
        else Format.asprintf "%s;%a" prefix pp_call call
      in
      Format.fprintf
        ppf
        "%s %a\n%a"
        prefix
        Z.pp_print
        ticks
        (pp_flame_callstack_nodes
           ~prefix
           ~depth:(succ depth)
           ~max_depth
           pp_call)
        subnodes
  | Toplevel nodes ->
      pp_flame_callstack_nodes ~prefix ~depth ~max_depth pp_call ppf nodes

and pp_flame_callstack_nodes ~prefix ~depth ~max_depth pp_call ppf nodes =
  if depth > max_depth then ()
  else
    (Format.pp_print_list
       ~pp_sep:(fun _ () -> ())
       (pp_flame_callstack_node ~prefix ~depth ~max_depth pp_call))
      ppf
      nodes

(** [pp_callstack_as_flamegraph] if [pp_stack] with the syntax of flamegraphs. *)
let pp_callstack_as_flamegraph ~max_depth pp_call =
  (* `pp_call` is repeated to enforce generalization, it leads to a typechecking
     error otherwise. *)
  pp_flame_callstack_node ~prefix:"" ~depth:0 ~max_depth pp_call

(** [pp_flamegraph] collapses the stack and print it as a valid flamegraph. *)
let pp_collapsed_flamegraph ~max_depth pp_call ppf call_stack =
  let nodes = collapse_stack ~max_depth pp_call call_stack in
  Format.fprintf
    ppf
    "%a"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
       (fun ppf (name, ticks) ->
         Format.fprintf ppf "%s %a" name Z.pp_print ticks))
    nodes

(** [pp_flamegraph ~collapsed ~max_depth pp_call ppf call_stack] outputs the
    given [call_stack] with its `flamegraph` representation. If [collapse =
    true], the stacks are collapsed. This can be useful to output smaller files,
    but the stack cannot be analyzed on a time basis (i.e. as a flamechart). *)
let pp_flamegraph ~collapse ~max_depth pp_call ppf call_stack =
  if collapse then pp_collapsed_flamegraph ~max_depth pp_call ppf call_stack
  else pp_callstack_as_flamegraph ~max_depth pp_call ppf call_stack

(** [aggregate_toplevel_time_and_ticks ~call_stack] counts the time and ticks
    spent in each toplevel phases during an execution. *)
let aggregate_toplevel_time_and_ticks = function
  | Node _ -> []
  | Toplevel nodes ->
      let aggregate = function
        | Toplevel _ -> assert false
        | Node (call, _, _, _) as node ->
            let ticks, time =
              fold_call_stack
                (fun (acc_ticks, acc_time) _ ticks time ->
                  (Z.add ticks acc_ticks, add_opt_times acc_time time))
                (Z.zero, Some Ptime.Span.zero)
                node
            in
            (call, ticks, time)
      in
      List.map aggregate nodes

let full_ticks_and_time toplevel_result =
  List.fold_left
    (fun (acc_ticks, acc_time) (_, ticks, time) ->
      (Z.add ticks acc_ticks, add_opt_times acc_time time))
    (Z.zero, Some Ptime.Span.zero)
    toplevel_result

let pp_ticks_and_time ppf (call, ticks, time) =
  Format.fprintf
    ppf
    "%a: %a ticks%a"
    pp_call
    call
    Z.pp_print
    ticks
    pp_time_opt
    time
