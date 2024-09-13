(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
(* Copyright (c) 2023  Marigold <contact@marigold.dev>                       *)
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

let ns = Interpreter_model.ns

let fv = Interpreter_model.fv

module Timelock_samplers = Tezos_crypto.Timelock
open Protocol

(* ------------------------------------------------------------------------- *)

type ex_stack_and_kinstr =
  | Ex_stack_and_kinstr : {
      stack : 'a * 'b;
      stack_type : ('a, 'b) Script_typed_ir.stack_ty;
      kinstr : ('a, 'b, 'c, 'd) Script_typed_ir.kinstr;
    }
      -> ex_stack_and_kinstr

type ex_stack_and_continuation =
  | Ex_stack_and_cont : {
      stack : 'a * 'b;
      stack_type : ('a, 'b) Script_typed_ir.stack_ty;
      cont : ('a, 'b, 'c, 'd) Script_typed_ir.continuation;
    }
      -> ex_stack_and_continuation

type ex_value =
  | Ex_value : {value : 'a; ty : ('a, _) Script_typed_ir.ty} -> ex_value

type benchmark_type = Registration_helpers.benchmark_type = Time | Alloc

let gas_unit_per_allocation_word = 4

(* ------------------------------------------------------------------------- *)

let sf = Printf.sprintf

(* End of Stack *)
let eos = Script_typed_ir.(EmptyCell, EmptyCell)

let info_and_name ~benchmark_type ~intercept ?(salt = "") s =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6072
     Time benchmarks should be qualified with "time" when we switch to time-alloc cost model
  *)
  let s = s ^ salt in

  let info =
    let type_str =
      match benchmark_type with
      | Time -> "Time benchmark"
      | Alloc -> "Allocation benchmark"
    in
    let intercept_str = if intercept then " (intercept case)" else "" in
    sf "%s %s%s" type_str s intercept_str
  in

  let benchmark_type =
    match benchmark_type with Time -> [] | Alloc -> ["alloc"]
  in
  let intercept = if intercept then ["intercept"] else [] in
  let name = s :: (benchmark_type @ intercept) in

  let rec make_name ns = function
    | [] -> assert false
    | [s] -> ns s
    | s :: ss -> make_name (Namespace.make ns s) ss
  in

  (info, make_name ns name)

module Default_boilerplate = struct
  type workload = Interpreter_workload.t

  let workload_encoding = Interpreter_workload.encoding

  let workload_to_vector = Interpreter_workload.trace_to_sparse_vec

  let tags = [Tags.interpreter]
end

module Default_config = struct
  (* Configuration specific to sapling benchmarks *)
  type sapling_config = {sapling_txs_file : string; seed : int option}

  (* Configuration specific to benchmarking Dign/Dipn/Dupn/Dropn/Combs *)
  type comb_config = {max_depth : int}

  (* Configuration specific to benchmarking ICompare *)
  type compare_config = {type_size : Base_samplers.range}

  type config = {
    sampler : Michelson_samplers.parameters;
    sapling : sapling_config;
    comb : comb_config;
    compare : compare_config;
  }

  let default_config =
    let open Michelson_samplers in
    let open Michelson_samplers_base in
    let sampler =
      {
        base_parameters =
          {
            int_size = {min = 8; max = 100_000};
            string_size = {min = 1 lsl 10; max = 1 lsl 17};
            bytes_size = {min = 1 lsl 10; max = 1 lsl 17};
          };
        list_size = {min = 10; max = 1000};
        set_size = {min = 10; max = 1000};
        map_size = {min = 10; max = 1000};
      }
    in
    {
      sampler;
      sapling = {sapling_txs_file = {|/no/such/file|}; seed = None};
      comb = {max_depth = 1000};
      compare = {type_size = {min = 1; max = 15}};
    }

  let sapling_config_encoding =
    let open Data_encoding in
    conv
      (fun {sapling_txs_file; seed} -> (sapling_txs_file, seed))
      (fun (sapling_txs_file, seed) -> {sapling_txs_file; seed})
      (obj2 (req "sapling_txs_file" string) (req "seed" (option int31)))

  let comb_config_encoding =
    let open Data_encoding in
    conv
      (fun {max_depth} -> max_depth)
      (fun max_depth -> {max_depth})
      (obj1 (req "max_depth" int31))

  let compare_config_encoding =
    let open Data_encoding in
    conv
      (fun {type_size} -> type_size)
      (fun type_size -> {type_size})
      (obj1 (req "type_size" Base_samplers.range_encoding))

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {sampler; sapling; comb; compare} ->
        (sampler, sapling, comb, compare))
      (fun (sampler, sapling, comb, compare) ->
        {sampler; sapling; comb; compare})
      (obj4
         (req "sampler" Michelson_samplers.parameters_encoding)
         (req "sapling" sapling_config_encoding)
         (req "comb" comb_config_encoding)
         (req "compare" compare_config_encoding))
end

let make_default_samplers ?(algo = `Default) cfg :
    (module Crypto_samplers.Finite_key_pool_S) * (module Michelson_samplers.S) =
  let module Crypto_samplers = Crypto_samplers.Make_finite_key_pool (struct
    let size = 16

    let algo = algo
  end) in
  let module Michelson_samplers =
    Michelson_samplers.Make
      (struct
        let parameters = cfg
      end)
      (Crypto_samplers)
  in
  ((module Crypto_samplers), (module Michelson_samplers))

(* ------------------------------------------------------------------------- *)
(* Helpers for creating benchmarks for the interpreter *)

(* Register only a model for code generation *)
let register_model_for_code_generation model =
  let model = Model.make ~conv:Fun.id model in
  Registration.register_model_for_code_generation
    "interpreter"
    model
    ~destination:"michelson_v1_gas"

let register_time_alloc_codegen_model instr_or_cont_name =
  let (Model.Model codegen_model) =
    Interpreter_model.make_time_alloc_codegen_model instr_or_cont_name
  in
  register_model_for_code_generation codegen_model

let prepare_workload ?amplification ctxt step_constants stack_type kinstr stack
    =
  let workload =
    Interpreter_workload.extract_deps
      ctxt
      step_constants
      stack_type
      kinstr
      stack
  in
  match amplification with
  | None -> workload
  | Some n -> List.repeat n workload |> List.flatten

let time_benchmark_from_kinstr_and_stack :
    ?amplification:int ->
    Alpha_context.context ->
    Protocol.Script_interpreter.step_constants ->
    ex_stack_and_kinstr ->
    Interpreter_workload.ir_sized_step list Generator.benchmark =
 fun ?amplification ctxt step_constants stack_kinstr ->
  let ctxt = Gas_helpers.set_limit ctxt in
  let (Ex_stack_and_kinstr {stack = bef_top, bef; stack_type; kinstr}) =
    stack_kinstr
  in
  let workload =
    prepare_workload
      ?amplification
      ctxt
      step_constants
      stack_type
      kinstr
      (bef_top, bef)
  in
  let _gas_counter, outdated_ctxt =
    Local_gas_counter.local_gas_counter_and_outdated_context ctxt
  in

  let closure =
    match amplification with
    | None ->
        let closure () =
          (* Lwt_main.run *)
          ignore
            (Script_interpreter.Internals.step
               (outdated_ctxt, step_constants)
               (Local_gas_counter 9_999_999_999)
               kinstr
               bef_top
               bef)
        in
        closure
    | Some amplification_factor ->
        assert (amplification_factor > 0) ;
        let closure () =
          for _i = 1 to amplification_factor do
            ignore
              (* Lwt_main.run *)
              (Script_interpreter.Internals.step
                 (outdated_ctxt, step_constants)
                 (Local_gas_counter 9_999_999_999)
                 kinstr
                 bef_top
                 bef)
          done
        in
        closure
  in
  Generator.Plain {workload; closure}

let alloc_benchmark_from_kinstr_and_stack :
    Alpha_context.context ->
    Protocol.Script_interpreter.step_constants ->
    ex_stack_and_kinstr ->
    Interpreter_workload.ir_sized_step list Generator.benchmark =
 fun ctxt step_constants stack_kinstr ->
  let ctxt = Gas_helpers.set_limit ctxt in
  let (Ex_stack_and_kinstr {stack = bef_top, bef; stack_type; kinstr}) =
    stack_kinstr
  in
  let workload =
    prepare_workload ctxt step_constants stack_type kinstr (bef_top, bef)
  in
  let _gas_counter, outdated_ctxt =
    Local_gas_counter.local_gas_counter_and_outdated_context ctxt
  in

  let measure () =
    let result =
      Lwt_main.run
      @@ Script_interpreter.Internals.step
           (outdated_ctxt, step_constants)
           (Local_gas_counter 9_999_999_999)
           kinstr
           bef_top
           bef
    in
    Result.fold
      ~error:(fun _ -> 0.0)
      ~ok:(fun (stack_top, stack, _, _) ->
        let size_after =
          Obj.reachable_words (Obj.repr (stack_top, stack, bef_top, bef))
        in
        let size_before =
          Obj.reachable_words (Obj.repr (bef_top, bef, bef_top, bef))
        in

        float_of_int ((size_after - size_before) * gas_unit_per_allocation_word))
      result
  in

  Generator.Calculated {workload; measure}

let benchmark_from_kinstr_and_stack :
    ?amplification:int ->
    benchmark_type ->
    Alpha_context.context ->
    Protocol.Script_interpreter.step_constants ->
    ex_stack_and_kinstr ->
    Interpreter_workload.ir_sized_step list Generator.benchmark =
 fun ?amplification benchmark_type ->
  match benchmark_type with
  | Time -> time_benchmark_from_kinstr_and_stack ?amplification
  | Alloc ->
      (* amplification wouldn't make sense,
         because the measurement resolution doesn't matter for the allocation *)
      assert (amplification = None) ;
      alloc_benchmark_from_kinstr_and_stack

let time_and_alloc (f : benchmark_type -> unit) =
  f Time ;
  f Alloc

let make_benchmark :
    ?amplification:int ->
    ?intercept:bool ->
    ?salt:string ->
    ?more_tags:string list ->
    ?check:(unit -> unit) ->
    name:Interpreter_workload.instruction_name ->
    benchmark_type:benchmark_type ->
    kinstr_and_stack_sampler:
      (Default_config.config -> Random.State.t -> unit -> ex_stack_and_kinstr) ->
    unit ->
    Benchmark.t =
 fun ?amplification
     ?(intercept = false)
     ?salt
     ?(more_tags = [])
     ?(check = fun () -> ())
     ~name
     ~benchmark_type
     ~kinstr_and_stack_sampler
     () ->
  let module B : Benchmark.S = struct
    include Default_config
    include Default_boilerplate

    let tags = tags @ more_tags

    let models =
      (* [intercept = true] implies there's a benchmark with [intercept = false].
         No need to register the model twice. *)
      Interpreter_model.make_model
        benchmark_type
        ?amplification
        (Instr_name name)

    let info, name =
      info_and_name
        ~benchmark_type
        ~intercept
        ?salt
        (Interpreter_workload.string_of_instruction_name name)

    let module_filename = __FILE__

    let purpose = Benchmark.Generate_code "michelson_v1_gas"

    let benchmark kinstr_and_stack_sampler ctxt step_constants () =
      let stack_instr = kinstr_and_stack_sampler () in
      benchmark_from_kinstr_and_stack
        ?amplification
        benchmark_type
        ctxt
        step_constants
        stack_instr

    let create_benchmarks ~rng_state ~bench_num (config : config) =
      check () ;
      match Lwt_main.run (Execution_context.make ~rng_state ()) with
      | Error _errs -> assert false
      | Ok (ctxt, step_constants) ->
          let kinstr_and_stack_sampler =
            kinstr_and_stack_sampler config rng_state
          in
          List.repeat
            bench_num
            (benchmark kinstr_and_stack_sampler ctxt step_constants)
  end in
  (module B : Benchmark.S)

let make_simple_benchmark :
    type bef_top bef res_top res.
    ?amplification:int ->
    ?intercept:bool ->
    ?more_tags:string list ->
    ?salt:string ->
    ?check:(unit -> unit) ->
    name:Interpreter_workload.instruction_name ->
    benchmark_type:benchmark_type ->
    stack_type:(bef_top, bef) Script_typed_ir.stack_ty ->
    kinstr:(bef_top, bef, res_top, res) Script_typed_ir.kinstr ->
    unit ->
    Benchmark.t =
 fun ?amplification
     ?intercept
     ?more_tags
     ?salt
     ?check
     ~name
     ~benchmark_type
     ~stack_type
     ~kinstr
     () ->
  let kinstr_and_stack_sampler config rng_state =
    let _, (module Samplers) =
      make_default_samplers config.Default_config.sampler
    in
    fun () ->
      Ex_stack_and_kinstr
        {
          stack = Samplers.Random_value.stack stack_type rng_state;
          stack_type;
          kinstr;
        }
  in
  make_benchmark
    ?amplification
    ?intercept
    ?more_tags
    ?salt
    ?check
    ~name
    ~benchmark_type
    ~kinstr_and_stack_sampler
    ()

let benchmark ?(benchmark_type = Time) ?amplification ?intercept ?more_tags
    ?salt ?check ~name ~kinstr_and_stack_sampler () =
  let bench =
    make_benchmark
      ?amplification
      ?intercept
      ?more_tags
      ?salt
      ?check
      ~name
      ~benchmark_type
      ~kinstr_and_stack_sampler
      ()
  in
  Registration_helpers.register ~benchmark_type bench

let time_alloc_benchmark ?amplification ?intercept ?more_tags ?salt ?check ~name
    ~kinstr_and_stack_sampler () =
  benchmark
    ~benchmark_type:Time
    ?amplification
    ?intercept
    ?more_tags
    ?salt
    ?check
    ~name
    ~kinstr_and_stack_sampler
    () ;
  benchmark
    ~benchmark_type:Alloc
    ?intercept
    ?more_tags
    ?salt
    ?check
    ~name
    ~kinstr_and_stack_sampler
    () ;
  register_time_alloc_codegen_model (Instr_name name)

let benchmark_with_stack_sampler ?(benchmark_type = Time) ?amplification
    ?intercept ?more_tags ?salt ?check ~stack_type ~name ~kinstr ~stack_sampler
    () =
  let kinstr_and_stack_sampler config rng_state =
    let stack_sampler = stack_sampler config rng_state in
    fun () -> Ex_stack_and_kinstr {stack = stack_sampler (); stack_type; kinstr}
  in
  let bench =
    make_benchmark
      ?amplification
      ?intercept
      ?more_tags
      ?salt
      ?check
      ~name
      ~benchmark_type
      ~kinstr_and_stack_sampler
      ()
  in
  Registration_helpers.register ~benchmark_type bench

let benchmark_with_fixed_stack ?(benchmark_type = Time) ?amplification
    ?intercept ?more_tags ?salt ?check ~name ~stack_type ~stack ~kinstr () =
  benchmark_with_stack_sampler
    ?amplification
    ?intercept
    ?more_tags
    ?salt
    ?check
    ~name
    ~stack_type
    ~benchmark_type
    ~kinstr
    ~stack_sampler:(fun _cfg _rng_state () -> stack)
    ()

let time_alloc_benchmark_with_fixed_stack ?amplification ?intercept ?more_tags
    ?salt ?check ~name ~stack_type ~stack ~kinstr () =
  benchmark_with_fixed_stack
    ~benchmark_type:Time
    ?amplification
    ?intercept
    ?more_tags
    ?salt
    ?check
    ~name
    ~stack_type
    ~stack
    ~kinstr
    () ;
  benchmark_with_fixed_stack
    ~benchmark_type:Alloc
    ?intercept
    ?more_tags
    ?salt
    ?check
    ~name
    ~stack_type
    ~stack
    ~kinstr
    () ;
  register_time_alloc_codegen_model (Instr_name name)

let simple_benchmark_with_stack_sampler ?(benchmark_type = Time) ?amplification
    ?intercept_stack ?salt ?more_tags ?check ~name ~stack_type ~kinstr
    ~stack_sampler () =
  benchmark_with_stack_sampler
    ?amplification
    ~intercept:false
    ?salt
    ?more_tags
    ?check
    ~name
    ~benchmark_type
    ~stack_type
    ~kinstr
    ~stack_sampler
    () ;
  Option.iter
    (fun stack ->
      benchmark_with_fixed_stack
        ?amplification
        ~intercept:true
        ?more_tags
        ?salt
        ?check
        ~name
        ~benchmark_type
        ~stack_type
        ~stack
        ~kinstr
        ())
    intercept_stack

let simple_time_alloc_benchmark_with_stack_sampler ?amplification
    ?intercept_stack ?(alloc_intercept = true) ?salt ?more_tags ?check ~name
    ~stack_type ~kinstr ~stack_sampler () =
  simple_benchmark_with_stack_sampler
    ~benchmark_type:Time
    ?amplification
    ?intercept_stack
    ?salt
    ?more_tags
    ?check
    ~name
    ~stack_type
    ~kinstr
    ~stack_sampler
    () ;
  simple_benchmark_with_stack_sampler
    ~benchmark_type:Alloc
    ?intercept_stack:(if alloc_intercept then intercept_stack else None)
    ?salt
    ?more_tags
    ?check
    ~name
    ~stack_type
    ~kinstr
    ~stack_sampler
    () ;
  register_time_alloc_codegen_model (Instr_name name)

let simple_benchmark ?(benchmark_type = Time) ?amplification ?intercept_stack
    ?more_tags ?salt ?check ~name ~stack_type ~kinstr () =
  let bench =
    make_simple_benchmark
      ?amplification
      ~intercept:false
      ?more_tags
      ?salt
      ?check
      ~name
      ~benchmark_type
      ~stack_type
      ~kinstr
      ()
  in
  Registration_helpers.register ~benchmark_type bench ;
  Option.iter
    (fun stack ->
      benchmark_with_fixed_stack
        ?amplification
        ~intercept:true
        ?more_tags
        ?salt
        ?check
        ~name
        ~benchmark_type
        ~stack_type
        ~stack
        ~kinstr
        ())
    intercept_stack

let simple_time_alloc_benchmark ?amplification ?intercept_stack ?more_tags ?salt
    ?check ~name ~stack_type ~kinstr () =
  simple_benchmark
    ~benchmark_type:Time
    ?amplification
    ?intercept_stack
    ?more_tags
    ?salt
    ?check
    ~name
    ~stack_type
    ~kinstr
    () ;
  simple_benchmark
    ~benchmark_type:Alloc
    ?intercept_stack
    ?more_tags
    ?salt
    ?check
    ~name
    ~stack_type
    ~kinstr
    () ;
  register_time_alloc_codegen_model (Instr_name name)

(* ------------------------------------------------------------------------- *)
(* Helpers for creating benchmarks for [Script_interpreter.next] *)

let time_benchmark_from_continuation :
    ?amplification:int ->
    Alpha_context.context ->
    Protocol.Script_interpreter.step_constants ->
    ex_stack_and_continuation ->
    Interpreter_workload.ir_sized_step list Generator.benchmark =
 fun ?amplification ctxt step_constants stack_cont ->
  let ctxt = Gas_helpers.set_limit ctxt in
  match stack_cont with
  | Ex_stack_and_cont {stack = bef_top, bef; cont; stack_type} ->
      let workload, closure =
        match amplification with
        | None ->
            let workload =
              Interpreter_workload.extract_deps_continuation
                ctxt
                step_constants
                stack_type
                cont
                (bef_top, bef)
            in
            let _gas_counter, outdated_ctxt =
              Local_gas_counter.local_gas_counter_and_outdated_context ctxt
            in
            let closure () =
              ignore
                (* Lwt_main.run *)
                (Script_interpreter.Internals.next
                   None
                   (outdated_ctxt, step_constants)
                   (Local_gas_counter 9_999_999_999)
                   stack_type
                   cont
                   bef_top
                   bef)
            in
            (workload, closure)
        | Some amplification_factor ->
            assert (amplification_factor > 0) ;
            let workload =
              Interpreter_workload.extract_deps_continuation
                ctxt
                step_constants
                stack_type
                cont
                (bef_top, bef)
            in
            let workload =
              List.repeat amplification_factor workload |> List.flatten
            in
            let _gas_counter, outdated_ctxt =
              Local_gas_counter.local_gas_counter_and_outdated_context ctxt
            in
            let closure () =
              for _i = 1 to amplification_factor do
                ignore
                  (* Lwt_main.run *)
                  (Script_interpreter.Internals.next
                     None
                     (outdated_ctxt, step_constants)
                     (Local_gas_counter 9_999_999_999)
                     stack_type
                     cont
                     bef_top
                     bef)
              done
            in
            (workload, closure)
      in
      Generator.Plain {workload; closure}

let alloc_benchmark_from_continuation :
    Alpha_context.context ->
    Protocol.Script_interpreter.step_constants ->
    ex_stack_and_continuation ->
    Interpreter_workload.ir_sized_step list Generator.benchmark =
 fun ctxt step_constants stack_cont ->
  let ctxt = Gas_helpers.set_limit ctxt in
  let (Ex_stack_and_cont {stack = bef_top, bef; cont; stack_type}) =
    stack_cont
  in
  let workload =
    Interpreter_workload.extract_deps_continuation
      ctxt
      step_constants
      stack_type
      cont
      (bef_top, bef)
  in
  let _gas_counter, outdated_ctxt =
    Local_gas_counter.local_gas_counter_and_outdated_context ctxt
  in
  let measure () =
    let result =
      Lwt_main.run
      @@ Script_interpreter.Internals.next
           None
           (outdated_ctxt, step_constants)
           (Local_gas_counter 9_999_999_999)
           stack_type
           cont
           bef_top
           bef
    in
    Result.fold
      ~error:(fun _ -> 0.0)
      ~ok:(fun (stack_top, stack, _, _) ->
        let size_after =
          Obj.reachable_words (Obj.repr (stack_top, stack, bef_top, bef, cont))
        in
        let size_before =
          Obj.reachable_words (Obj.repr (bef_top, bef, bef_top, bef, cont))
        in

        float_of_int ((size_after - size_before) * gas_unit_per_allocation_word))
      result
  in
  Generator.Calculated {workload; measure}

let benchmark_from_continuation :
    ?amplification:int ->
    benchmark_type ->
    Alpha_context.context ->
    Protocol.Script_interpreter.step_constants ->
    ex_stack_and_continuation ->
    Interpreter_workload.ir_sized_step list Generator.benchmark =
 fun ?amplification benchmark_type ->
  match benchmark_type with
  | Time -> time_benchmark_from_continuation ?amplification
  | Alloc ->
      (* amplification wouldn't make sense,
         because the measurement resolution doesn't matter for the allocation *)
      assert (amplification = None) ;
      alloc_benchmark_from_continuation

let make_continuation_benchmark :
    ?amplification:int ->
    ?intercept:bool ->
    ?salt:string ->
    ?more_tags:string list ->
    ?check:(unit -> unit) ->
    name:Interpreter_workload.continuation_name ->
    benchmark_type:benchmark_type ->
    cont_and_stack_sampler:
      (Default_config.config ->
      Random.State.t ->
      unit ->
      ex_stack_and_continuation) ->
    unit ->
    Benchmark.t =
 fun ?amplification
     ?(intercept = false)
     ?salt
     ?(more_tags = [])
     ?(check = fun () -> ())
     ~name
     ~benchmark_type
     ~cont_and_stack_sampler
     () ->
  let module B : Benchmark.S = struct
    include Default_config
    include Default_boilerplate

    let tags = tags @ more_tags

    let models =
      Interpreter_model.make_model
        benchmark_type
        ?amplification
        (Cont_name name)

    let info, name =
      info_and_name
        ~benchmark_type
        ~intercept
        ?salt
        (Interpreter_workload.string_of_continuation_name name)

    let module_filename = __FILE__

    let purpose = Benchmark.Generate_code "michelson_v1_gas"

    let benchmark cont_and_stack_sampler ctxt step_constants () =
      let stack_instr = cont_and_stack_sampler () in
      benchmark_from_continuation
        ?amplification
        benchmark_type
        ctxt
        step_constants
        stack_instr

    let create_benchmarks ~rng_state ~bench_num (config : config) =
      check () ;
      match Lwt_main.run (Execution_context.make ~rng_state ()) with
      | Error _errs -> assert false
      | Ok (ctxt, step_constants) ->
          let cont_and_stack_sampler =
            cont_and_stack_sampler config rng_state
          in
          List.repeat
            bench_num
            (benchmark cont_and_stack_sampler ctxt step_constants)
  end in
  (module B : Benchmark.S)

let continuation_benchmark ?(benchmark_type = Time) ?amplification ?intercept
    ?salt ?more_tags ?check ~name ~cont_and_stack_sampler () =
  let bench =
    make_continuation_benchmark
      ?amplification
      ?intercept
      ?salt
      ?more_tags
      ?check
      ~name
      ~benchmark_type
      ~cont_and_stack_sampler
      ()
  in
  Registration_helpers.register ~benchmark_type bench

let continuation_time_alloc_benchmark ?amplification ?intercept ?salt ?more_tags
    ?check ~name ~cont_and_stack_sampler () =
  continuation_benchmark
    ?amplification
    ?intercept
    ?salt
    ?more_tags
    ?check
    ~name
    ~benchmark_type:Time
    ~cont_and_stack_sampler
    () ;
  continuation_benchmark
    ?intercept
    ?salt
    ?more_tags
    ?check
    ~name
    ~benchmark_type:Alloc
    ~cont_and_stack_sampler
    () ;
  register_time_alloc_codegen_model (Cont_name name)

(* ------------------------------------------------------------------------- *)
(* Sampling helpers *)

let nat_of_positive_int (i : int) =
  let open Script_int in
  match is_nat (of_int i) with None -> assert false | Some x -> x

let adversarial_ints rng_state (cfg : Default_config.config) n =
  let _common_prefix, ls =
    Base_samplers.Adversarial.integers
      ~prefix_size:cfg.sampler.base_parameters.int_size
      ~card:n
      rng_state
  in
  List.map Script_int.of_zint ls

(* ------------------------------------------------------------------------- *)
(* Error helpers *)

let raise_if_error = function
  | Ok x -> x
  | Error e ->
      Format.eprintf "%a@." (Error_monad.TzTrace.pp_print Error_monad.pp) e ;
      Stdlib.failwith "raise_if_error"

(* ------------------------------------------------------------------------- *)

(** [Registration_section] contains all interpreter benchmarks. The goal of
    a benchmark is to gather enough data to reliably estimate the parameters
    of the cost model associated to each instruction. In general, it can
    take several distinct benchmarks to properly cover all the execution
    paths.

    In particular, for affine cost model, it is often worth estimating the
    intercept separately from the size-dependent coefficients.
 *)

module Registration_section = struct
  open Script_typed_ir
  open Michelson_types

  let sf = Printf.sprintf

  let dummy_loc = 0

  let halt = IHalt dummy_loc

  let () =
    (* KHalt *)
    simple_time_alloc_benchmark
      ~amplification:100
      ~name:Interpreter_workload.N_IHalt
      ~stack_type:(unit @$ bot)
      ~kinstr:halt
      ()

  module Amplification = struct
    module Loop : Benchmark.S = struct
      let name = ns "amplification_loop"

      let info = "Benchmarking the cost of an empty loop"

      let module_filename = __FILE__

      let purpose = Benchmark.Generate_code "michelson_v1_gas"

      let tags = [Tags.interpreter]

      type config = {max_iterations : int}

      let config_encoding =
        let open Data_encoding in
        conv
          (fun {max_iterations} -> max_iterations)
          (fun max_iterations -> {max_iterations})
          (obj1 (req "max_iterations" int31))

      let default_config = {max_iterations = 100000}

      type workload = int

      let workload_encoding = Data_encoding.int31

      let workload_to_vector n =
        Sparse_vec.String.of_list [("iterations", float_of_int n)]

      let models = [("interpreter", Interpreter_model.amplification_loop_model)]

      let benchmark rng_state config () =
        let workload = Random.State.int rng_state config.max_iterations in
        let closure () =
          for _i = 1 to workload do
            Sys.opaque_identity ()
          done
        in
        Generator.Plain {workload; closure}

      let create_benchmarks ~rng_state ~bench_num (config : config) =
        List.repeat bench_num (benchmark rng_state config)
    end
  end

  let () = Registration_helpers.register (module Amplification.Loop)

  module Stack = struct
    let () =
      (* KDrop ; KHalt *)
      simple_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_IDrop
        ~stack_type:(unit @$ unit @$ bot)
        ~kinstr:(IDrop (dummy_loc, halt))
        ()

    let () =
      (* IDup ; IHalt *)
      simple_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_IDup
        ~stack_type:(unit @$ unit @$ bot)
        ~kinstr:(IDup (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_ISwap
        ~stack_type:(unit @$ unit @$ bot)
        ~kinstr:(ISwap (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_IPush
        ~stack_type:(unit @$ unit @$ bot)
        ~kinstr:(IPush (dummy_loc, unit, (), halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_IUnit
        ~stack_type:(unit @$ unit @$ bot)
        ~kinstr:(IUnit (dummy_loc, halt))
        ()

    (* deep stack manipulation *)

    (* Constructing these instructions is made especially painful by the
       fact that they include "stack preservation witnesses", which are not
       exposed in Script_ir_translator.
       We must go through [Script_ir_translator.parse_instr] to construct
       the corresponding terms. *)
    type ex_stack =
      | Ex_stack : ('a, 'b) Script_typed_ir.stack_ty * ('a * 'b) -> ex_stack

    let rec make_stack (depth : int) =
      if depth = 0 then assert false
      else if depth = 1 then Ex_stack (unit @$ Script_typed_ir.Bot_t, ((), eos))
      else
        let stack = make_stack (depth - 1) in
        match stack with
        | Ex_stack (stack_ty, stack) -> Ex_stack (unit @$ stack_ty, ((), stack))

    let parse_instr rng_state node stack =
      let open Lwt_result_syntax in
      match stack with
      | Ex_stack (stack_ty, stack) ->
          raise_if_error
            (Lwt_main.run
               (let* ctxt, _step_constants =
                  Execution_context.make ~rng_state ()
                in
                let* judgement, _ =
                  let*! result =
                    Script_ir_translator.parse_instr
                      Script_tc_context.data
                      ctxt
                      ~elab_conf:
                        (Script_ir_translator_config.make ~legacy:false ())
                      node
                      stack_ty
                  in
                  Lwt.return (Environment.wrap_tzresult result)
                in
                match judgement with
                | Script_ir_translator.Typed descr ->
                    let kinstr = descr.instr.apply (IHalt dummy_loc) in
                    return
                      (Ex_stack_and_kinstr
                         {stack; kinstr; stack_type = descr.bef})
                | Script_ir_translator.Failed _ -> assert false))

    open Protocol.Michelson_v1_primitives

    (* The size parameter of a deep stack instruction must fit on 10 bits. See
       [Script_ir_translator.parse_uint10]. *)
    let stack_size = 1023

    let long_stack = make_stack stack_size

    let sample_depth rng_state =
      Base_samplers.(
        sample_in_interval rng_state ~range:{min = 0; max = stack_size - 2})

    let () =
      let dig = Micheline.(Prim (0, I_DIG, [Int (0, Z.of_int 0)], [])) in
      benchmark
        ~amplification:100
        ~intercept:true
        ~name:Interpreter_workload.N_IDig
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dig in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let dig n = Micheline.(Prim (0, I_DIG, [Int (0, Z.of_int n)], [])) in
      time_alloc_benchmark
        ~name:Interpreter_workload.N_IDig
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dig (sample_depth rng_state) in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let dug = Micheline.(Prim (0, I_DUG, [Int (0, Z.of_int 0)], [])) in
      benchmark
        ~intercept:true
        ~name:Interpreter_workload.N_IDug
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dug in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let dug n = Micheline.(Prim (0, I_DUG, [Int (0, Z.of_int n)], [])) in
      time_alloc_benchmark
        ~name:Interpreter_workload.N_IDug
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dug (sample_depth rng_state) in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let nop = Micheline.Seq (0, []) in
      let dip = Micheline.(Prim (0, I_DIP, [Int (0, Z.of_int 0); nop], [])) in
      benchmark
        ~intercept:true
        ~name:Interpreter_workload.N_IDipN
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dip in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let nop = Micheline.Seq (0, []) in
      let dip n = Micheline.(Prim (0, I_DIP, [Int (0, Z.of_int n); nop], [])) in
      time_alloc_benchmark
        ~name:Interpreter_workload.N_IDipN
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dip (sample_depth rng_state) in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let drop = Micheline.(Prim (0, I_DROP, [Int (0, Z.of_int 0)], [])) in
      benchmark
        ~intercept:true
        ~name:Interpreter_workload.N_IDropN
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = drop in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let drop n = Micheline.(Prim (0, I_DROP, [Int (0, Z.of_int n)], [])) in
      time_alloc_benchmark
        ~name:Interpreter_workload.N_IDropN
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = drop (sample_depth rng_state) in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let pair n = Micheline.(Prim (0, I_PAIR, [Int (0, Z.of_int n)], [])) in
      ( time_and_alloc @@ fun benchmark_type ->
        benchmark
          ~benchmark_type
          ~name:Interpreter_workload.N_IComb
          ~kinstr_and_stack_sampler:(fun cfg rng_state () ->
            let width =
              Base_samplers.(
                sample_in_interval
                  rng_state
                  ~range:{min = 2; max = cfg.comb.max_depth})
            in
            let node = pair width in
            parse_instr rng_state node long_stack)
          () ) ;
      benchmark
        ~name:Interpreter_workload.N_IComb
        ~intercept:true
        ~kinstr_and_stack_sampler:(fun _ rng_state () ->
          let node = pair 2 in
          parse_instr rng_state node long_stack)
        () ;
      register_time_alloc_codegen_model
        (Instr_name Interpreter_workload.N_IComb)

    let rec make_comb_stack (comb_width : int) (depth : int) acc =
      if depth = 0 then
        match acc with
        | Ex_stack (stack_ty, stack) -> (
            match make_comb comb_width (Ex_value {value = (); ty = unit}) with
            | Ex_value {value; ty} -> Ex_stack (ty @$ stack_ty, (value, stack)))
      else
        match acc with
        | Ex_stack (stack_ty, stack) ->
            make_comb_stack
              comb_width
              (depth - 1)
              (Ex_stack (unit @$ stack_ty, ((), stack)))

    and make_comb comb_width comb_acc =
      if comb_width = 0 then assert false
      else if comb_width = 1 then comb_acc
      else
        match comb_acc with
        | Ex_value {value; ty} ->
            let (Ty_ex_c ty) = pair unit ty in
            make_comb (comb_width - 1) (Ex_value {value = ((), value); ty})

    let () =
      let unpair n =
        Micheline.(Prim (0, I_UNPAIR, [Int (0, Z.of_int n)], []))
      in
      ( time_and_alloc @@ fun benchmark_type ->
        benchmark
          ~benchmark_type
          ~name:Interpreter_workload.N_IUncomb
          ~kinstr_and_stack_sampler:(fun cfg rng_state () ->
            let width =
              Base_samplers.(
                sample_in_interval
                  rng_state
                  ~range:{min = 2; max = cfg.comb.max_depth - 2})
            in
            let node = unpair width in
            let stack =
              make_comb_stack width 1 (Ex_stack (unit @$ bot, ((), eos)))
            in
            parse_instr rng_state node stack)
          () ) ;
      benchmark
        ~name:Interpreter_workload.N_IUncomb
        ~intercept:true
        ~kinstr_and_stack_sampler:(fun _ rng_state () ->
          let width = 2 in
          let node = unpair width in
          let stack =
            make_comb_stack width 1 (Ex_stack (unit @$ bot, ((), eos)))
          in
          parse_instr rng_state node stack)
        () ;
      register_time_alloc_codegen_model
        (Instr_name Interpreter_workload.N_IUncomb)

    let () =
      let comb_get n = Micheline.(Prim (0, I_GET, [Int (0, Z.of_int n)], [])) in
      ( time_and_alloc @@ fun benchmark_type ->
        benchmark
          ~benchmark_type
          ~name:Interpreter_workload.N_IComb_get
          ~kinstr_and_stack_sampler:(fun cfg rng_state () ->
            let width =
              Base_samplers.(
                sample_in_interval
                  rng_state
                  ~range:{min = 2; max = cfg.comb.max_depth - 2})
            in
            let index =
              Base_samplers.(
                sample_in_interval rng_state ~range:{min = 0; max = width})
            in
            let node = comb_get index in
            let stack =
              make_comb_stack width 1 (Ex_stack (unit @$ bot, ((), eos)))
            in
            parse_instr rng_state node stack)
          () ) ;
      benchmark
        ~name:Interpreter_workload.N_IComb_get
        ~intercept:true
        ~kinstr_and_stack_sampler:(fun _ rng_state () ->
          let node = comb_get 0 in
          let stack = make_comb_stack 2 1 (Ex_stack (unit @$ bot, ((), eos))) in
          parse_instr rng_state node stack)
        () ;

      register_time_alloc_codegen_model
        (Instr_name Interpreter_workload.N_IComb_get)

    let () =
      let comb_set n =
        Micheline.(Prim (0, I_UPDATE, [Int (0, Z.of_int n)], []))
      in
      ( time_and_alloc @@ fun benchmark_type ->
        benchmark
          ~benchmark_type
          ~name:Interpreter_workload.N_IComb_set
          ~kinstr_and_stack_sampler:(fun cfg rng_state () ->
            let width =
              Base_samplers.(
                sample_in_interval
                  rng_state
                  ~range:{min = 2; max = cfg.comb.max_depth - 2})
            in
            let index =
              Base_samplers.(
                sample_in_interval rng_state ~range:{min = 0; max = width})
            in
            let node = comb_set index in
            let stack =
              let (Ex_stack (stack_ty, stack)) =
                make_comb_stack width 1 (Ex_stack (unit @$ bot, ((), eos)))
              in
              Ex_stack (unit @$ stack_ty, ((), stack))
            in
            parse_instr rng_state node stack)
          () ;
        benchmark
          ~benchmark_type
          ~name:Interpreter_workload.N_IComb_set
          ~intercept:true
          ~kinstr_and_stack_sampler:(fun _ rng_state () ->
            let node = comb_set 0 in
            let stack =
              let (Ex_stack (stack_ty, stack)) =
                make_comb_stack 2 1 (Ex_stack (unit @$ bot, ((), eos)))
              in
              Ex_stack (unit @$ stack_ty, ((), stack))
            in
            parse_instr rng_state node stack)
          () ) ;

      register_time_alloc_codegen_model
        (Instr_name Interpreter_workload.N_IComb_set)

    let () =
      let dup n = Micheline.(Prim (0, I_DUP, [Int (0, Z.of_int n)], [])) in
      time_alloc_benchmark
        ~name:Interpreter_workload.N_IDupN
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dup (1 + sample_depth rng_state) in
          parse_instr rng_state node long_stack)
        () ;
      time_alloc_benchmark
        ~name:Interpreter_workload.N_IDupN
        ~intercept:true
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dup 1 in
          parse_instr rng_state node long_stack)
        ()
  end

  module Pairs = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ICons_pair
        ~stack_type:(unit @$ unit @$ bot)
        ~kinstr:(ICons_pair (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ICar
        ~stack_type:(cpair unit unit @$ bot)
        ~kinstr:(ICar (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ICdr
        ~stack_type:(cpair unit unit @$ bot)
        ~kinstr:(ICdr (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IUnpair
        ~stack_type:(cpair unit unit @$ bot)
        ~kinstr:(IUnpair (dummy_loc, halt))
        ()
  end

  module Options = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ICons_some
        ~stack_type:(unit @$ bot)
        ~kinstr:(ICons_some (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ICons_none
        ~stack_type:(unit @$ bot)
        ~kinstr:(ICons_none (dummy_loc, unit, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IIf_none
        ~stack_type:(option unit @$ bot)
        ~kinstr:
          (IIf_none
             {
               loc = dummy_loc;
               branch_if_none = halt;
               branch_if_some = IDrop (dummy_loc, halt);
               k = halt;
             })
        ()

    let () =
      time_alloc_benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_IOpt_map_none
        ~stack:(None, ((), eos))
        ~stack_type:(option unit @$ unit @$ bot)
        ~kinstr:(IOpt_map {loc = dummy_loc; body = halt; k = halt})
        ()

    let () =
      time_alloc_benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_IOpt_map_some
        ~stack:(Some (), ((), eos))
        ~stack_type:(option unit @$ unit @$ bot)
        ~kinstr:(IOpt_map {loc = dummy_loc; body = halt; k = halt})
        ()

    let () =
      let model =
        Interpreter_model.Models.max_branching_model
          ~case_0:"none_const"
          ~case_1:"some_const"
          "N_IOpt_map"
      in
      register_model_for_code_generation model
  end

  module Ors = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ILeft
        ~stack_type:(unit @$ bot)
        ~kinstr:(ICons_left (dummy_loc, unit, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IRight
        ~stack_type:(unit @$ bot)
        ~kinstr:(ICons_right (dummy_loc, unit, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IIf_left
        ~stack_type:(cor unit unit @$ bot)
        ~kinstr:
          (IIf_left
             {
               loc = dummy_loc;
               branch_if_left = halt;
               branch_if_right = halt;
               k = halt;
             })
        ()
  end

  module Lists = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ICons_list
        ~stack_type:(unit @$ list unit @$ bot)
        ~kinstr:(ICons_list (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_INil
        ~stack_type:(unit @$ bot)
        ~kinstr:(INil (dummy_loc, unit, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IIf_cons
        ~stack_type:(list unit @$ unit @$ bot)
        ~kinstr:
          (IIf_cons
             {
               loc = dummy_loc;
               branch_if_cons = IDrop (dummy_loc, IDrop (dummy_loc, halt));
               branch_if_nil = halt;
               k = halt;
             })
        ()

    module Mapping = struct
      let () =
        (*
          IList_map ->
          IList_enter_body (empty case) ->
          IHalt
         *)
        time_alloc_benchmark_with_fixed_stack
          ~name:Interpreter_workload.N_IList_map
          ~stack:(Script_list.empty, ((), eos))
          ~stack_type:(list unit @$ unit @$ bot)
          ~kinstr:(IList_map (dummy_loc, halt, Some (list unit), halt))
          ()
    end

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IList_size
        ~stack_type:(list unit @$ bot)
        ~kinstr:(IList_size (dummy_loc, halt))
        ()

    let () =
      (*
        IList_iter ->
        IIter (empty case) ->
        IHalt
       *)
      time_alloc_benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_IList_iter
        ~stack:(Script_list.empty, ((), eos))
        ~stack_type:(list unit @$ unit @$ bot)
        ~kinstr:
          (IList_iter (dummy_loc, Some unit, IDrop (dummy_loc, halt), halt))
        ()
  end

  module Sets = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IEmpty_set
        ~stack_type:(unit @$ bot)
        ~kinstr:(IEmpty_set (dummy_loc, unit, halt))
        ()

    let set_iter_code =
      ISet_iter (dummy_loc, Some int, IDrop (dummy_loc, halt), halt)

    let () =
      (*
        ISet_iter ->
        (List.rev (set_fold)) ->
        {
        IIter ->
        IDrop ->
        ICons ->
        ...
        }
       *)
      simple_benchmark
        ~name:Interpreter_workload.N_ISet_iter
        ~intercept_stack:(Script_set.empty int, ((), eos))
        ~stack_type:(set int @$ unit @$ bot)
        ~kinstr:set_iter_code
        () ;
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_ISet_iter
        ~stack_type:(set int @$ unit @$ bot)
        ~kinstr:set_iter_code
        () ;
      register_time_alloc_codegen_model (Instr_name N_ISet_iter)

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISet_mem
        ~stack_type:(int @$ set int @$ unit @$ bot)
        ~kinstr:(ISet_mem (dummy_loc, halt))
        ~intercept_stack:(Script_int.zero, (Script_set.empty int, ((), eos)))
        ~alloc_intercept:false
        ~stack_sampler:(fun cfg rng_state () ->
          assert (cfg.sampler.set_size.min >= 1) ;
          let n =
            Base_samplers.sample_in_interval
              rng_state
              ~range:cfg.sampler.set_size
          in
          let elts = adversarial_ints rng_state cfg n in
          let set =
            List.fold_left
              (fun set elt -> Script_set.update elt true set)
              (Script_set.empty int)
              elts
          in
          let elt =
            List.nth_opt elts (Random.State.int rng_state n)
            |> WithExceptions.Option.get ~loc:__LOC__
          in
          (elt, (set, ((), eos))))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISet_update
        ~stack_type:(int @$ bool @$ set int @$ bot)
        ~kinstr:(ISet_update (dummy_loc, halt))
        ~intercept_stack:(Script_int.zero, (false, (Script_set.empty int, eos)))
        ~stack_sampler:(fun cfg rng_state () ->
          assert (cfg.sampler.set_size.min >= 2) ;
          let n =
            Base_samplers.sample_in_interval
              rng_state
              ~range:cfg.sampler.set_size
          in
          let elts = adversarial_ints rng_state cfg (n + 1) in
          let out_of_set, in_set =
            match elts with [] -> assert false | hd :: tl -> (hd, tl)
          in
          let set =
            List.fold_left
              (fun set elt -> Script_set.update elt true set)
              (Script_set.empty int)
              in_set
          in
          let stack =
            let flip = Random.State.bool rng_state in
            if flip then
              (* add an element not in the set *)
              (out_of_set, (true, (set, eos)))
            else
              (* remove an element in the set *)
              let elt = out_of_set in
              let set = Script_set.update elt true set in
              (elt, (flip, (set, eos)))
          in
          stack)
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ISet_size
        ~stack_type:(set unit @$ bot)
        ~kinstr:(ISet_size (dummy_loc, halt))
        ()
  end

  module Maps = struct
    let generate_map_and_key_in_map (cfg : Default_config.config) rng_state =
      let n =
        Base_samplers.sample_in_interval rng_state ~range:cfg.sampler.set_size
      in
      let keys = adversarial_ints rng_state cfg n in
      let map =
        List.fold_left
          (fun map i -> Script_map.update i (Some ()) map)
          (Script_map.empty int)
          keys
      in
      let (module M) = Script_map.get_module map in
      let key =
        M.OPS.fold (fun k _ -> function None -> Some k | x -> x) M.boxed None
        |> WithExceptions.Option.get ~loc:__LOC__
      in
      (key, map)

    let generate_map_and_key_not_in_map (cfg : Default_config.config) rng_state
        =
      (* [adversarial_ints] could return a non-distinct list,
         so the returned [key] may be bound in the [map] in a low probability. *)
      let n =
        Base_samplers.sample_in_interval rng_state ~range:cfg.sampler.set_size
      in
      let keys = adversarial_ints rng_state cfg n in
      let key, keys =
        match keys with [] -> assert false | x :: xs -> (x, xs)
      in
      let map =
        List.fold_left
          (fun map i -> Script_map.update i (Some ()) map)
          (Script_map.empty int)
          keys
      in
      (key, map)

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IEmpty_map
        ~stack_type:(unit @$ bot)
        ~kinstr:(IEmpty_map (dummy_loc, unit, Some unit, halt))
        ()

    (*
    let map_map_code =
      IMap_map
        ( dummy_loc,
          ICdr (dummy_loc, halt_unitunit),
          halt )
     *)

    let map_map_code () =
      IMap_map
        ( dummy_loc,
          Some (map int unit),
          IFailwith (dummy_loc, cpair int unit),
          halt )

    let () =
      (*
        Map_map (nonempty case) ->
        (List.rev (map_fold nonempty_map)) ->
        KMap_enter_body (nonempty case) ->
        fail (early interruption)
       *)
      simple_benchmark
        ~name:Interpreter_workload.N_IMap_map
        ~intercept_stack:
          (let map = Script_map.empty int in
           (map, ((), eos)))
        ~stack_type:(map int unit @$ unit @$ bot)
        ~kinstr:(map_map_code ())
        () ;
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_IMap_map
        ~stack_type:(map int unit @$ unit @$ bot)
        ~kinstr:(map_map_code ())
        () ;
      register_time_alloc_codegen_model (Instr_name N_IMap_map)

    let kmap_iter_code =
      IMap_iter (dummy_loc, Some (cpair int unit), IDrop (dummy_loc, halt), halt)

    let () =
      (*
        IMap_iter (nonempty case) ->
        (List.rev (map_fold (nonempty))) ->
        IIter (nonempty case) ->
        ...
       *)
      simple_benchmark
        ~name:Interpreter_workload.N_IMap_iter
        ~intercept_stack:
          (let map = Script_map.empty int in
           (map, ((), eos)))
        ~stack_type:(map int unit @$ unit @$ bot)
        ~kinstr:kmap_iter_code
        () ;
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_IMap_iter
        ~stack_type:(map int unit @$ unit @$ bot)
        ~kinstr:kmap_iter_code
        () ;
      register_time_alloc_codegen_model (Instr_name N_IMap_iter)

    let () =
      (*
        IMap_mem ->
        (map_mem) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_mem
        ~stack_type:(int @$ map int unit @$ unit @$ bot)
        ~kinstr:(IMap_mem (dummy_loc, halt))
        ~intercept_stack:
          (let map = Script_map.empty int in
           (Script_int.zero, (map, ((), eos))))
        ~alloc_intercept:false
        ~stack_sampler:(fun cfg rng_state () ->
          let key, map = generate_map_and_key_in_map cfg rng_state in
          (key, (map, ((), eos))))
        ()

    let () =
      (*
        IMap_get ->
        (map_get) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_get
        ~stack_type:(int @$ map int unit @$ unit @$ bot)
        ~kinstr:(IMap_get (dummy_loc, halt))
        ~intercept_stack:
          (let map = Script_map.empty int in
           (Script_int.zero, (map, ((), eos))))
        ~alloc_intercept:false
        ~stack_sampler:(fun cfg rng_state () ->
          let key, map = generate_map_and_key_in_map cfg rng_state in
          (key, (map, ((), eos))))
        ()

    let () =
      (*
        IMap_update ->
        (map_update) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_update
        ~stack_type:(int @$ option unit @$ map int unit @$ bot)
        ~kinstr:(IMap_update (dummy_loc, halt))
        ~intercept_stack:
          (let map = Script_map.empty int in
           (Script_int.zero, (None, (map, eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let key, map = generate_map_and_key_not_in_map cfg rng_state in
          (key, (Some (), (map, eos))))
        ()

    let () =
      (*
        IMap_get_and_update ->
        (map_update) ->
        (map_get) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_get_and_update
        ~stack_type:(int @$ option unit @$ map int unit @$ bot)
        ~kinstr:(IMap_get_and_update (dummy_loc, halt))
        ~intercept_stack:
          (let map = Script_map.empty int in
           (Script_int.zero, (None, (map, eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let key, map = generate_map_and_key_not_in_map cfg rng_state in
          (key, (Some (), (map, eos))))
        ()

    let () =
      (*
        IMap_size ->
        (map_update) ->
        (map_get) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_size
        ~stack_type:(map int unit @$ bot)
        ~kinstr:(IMap_size (dummy_loc, halt))
        ~stack_sampler:(fun _cfg _rng_state ->
          let map = Script_map.empty int in
          fun () -> (map, eos))
        ()
  end

  module Big_maps = struct
    let generate_big_map_and_key_in_map (cfg : Default_config.config) rng_state
        =
      let open Lwt_result_syntax in
      let n =
        Base_samplers.sample_in_interval rng_state ~range:cfg.sampler.set_size
      in
      let keys = adversarial_ints rng_state cfg n in
      let key, keys =
        match keys with [] -> assert false | hd :: tl -> (hd, tl)
      in
      let big_map =
        raise_if_error
          (Lwt_main.run
             (let* ctxt, _ = Execution_context.make ~rng_state () in
              let big_map = Script_big_map.empty int unit_t in
              let* big_map, _ =
                let*! result =
                  List.fold_left_es
                    (fun (bm, ctxt_acc) k ->
                      Script_big_map.update ctxt_acc k (Some ()) bm)
                    (big_map, ctxt)
                    keys
                in
                Lwt.return (Environment.wrap_tzresult result)
              in
              return big_map))
      in
      (key, big_map)

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IEmpty_big_map
        ~stack_type:(unit @$ bot)
        ~kinstr:(IEmpty_big_map (dummy_loc, unit, unit, halt))
        ()

    let () =
      (*
        IBig_map_mem ->
        (update context with gas)
        (big_map_mem) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBig_map_mem
        ~stack_type:(int @$ big_map int unit @$ unit @$ bot)
        ~kinstr:(IBig_map_mem (dummy_loc, halt))
        ~intercept_stack:
          (let map = Script_big_map.empty int unit in
           (Script_int.zero, (map, ((), eos))))
        ~alloc_intercept:false
        ~stack_sampler:(fun cfg rng_state () ->
          let key, map = generate_big_map_and_key_in_map cfg rng_state in
          (key, (map, ((), eos))))
        ()

    let () =
      (*
        IBig_map_get ->
        (big_map_get) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBig_map_get
        ~stack_type:(int @$ big_map int unit @$ unit @$ bot)
        ~kinstr:(IBig_map_get (dummy_loc, halt))
        ~intercept_stack:
          (let map = Script_big_map.empty int unit in
           (Script_int.zero, (map, ((), eos))))
        ~alloc_intercept:false
        ~stack_sampler:(fun cfg rng_state () ->
          let key, map = generate_big_map_and_key_in_map cfg rng_state in
          (key, (map, ((), eos))))
        ()

    let () =
      (*
        IBig_map_update ->
        (big_map_update) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBig_map_update
        ~stack_type:(int @$ option unit @$ big_map int unit @$ bot)
        ~kinstr:(IBig_map_update (dummy_loc, halt))
        ~intercept_stack:
          (let map = Script_big_map.empty int unit in
           (Script_int.zero, (None, (map, eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let key, map = generate_big_map_and_key_in_map cfg rng_state in
          (key, (Some (), (map, eos))))
        ()

    let () =
      (*
        IBig_map_get_and_update ->
        (big_map_update) ->
        (big_map_get) ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBig_map_get_and_update
        ~stack_type:(int @$ option unit @$ big_map int unit @$ bot)
        ~kinstr:(IBig_map_get_and_update (dummy_loc, halt))
        ~intercept_stack:
          (let map = Script_big_map.empty int unit in
           (Script_int.zero, (None, (map, eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let key, map = generate_big_map_and_key_in_map cfg rng_state in
          (key, (Some (), (map, eos))))
        ()
  end

  module Strings = struct
    open Script_string

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IConcat_string
        ~intercept_stack:(Script_list.empty, eos)
        ~stack_type:(list string @$ bot)
        ~kinstr:(IConcat_string (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IConcat_string_pair
        ~intercept_stack:(empty, (empty, eos))
        ~stack_type:(string @$ string @$ bot)
        ~kinstr:(IConcat_string_pair (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISlice_string
        ~stack_type:(nat @$ nat @$ string @$ bot)
        ~kinstr:(ISlice_string (dummy_loc, halt))
        ~intercept_stack:
          (let z = Script_int.zero_n in
           (z, (z, (empty, eos))))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let string =
              Samplers.Random_value.value Script_typed_ir.string_t rng_state
            in
            let len = nat_of_positive_int (length string) in
            (* worst case: offset = 0 *)
            (nat_of_positive_int 0, (len, (string, eos))))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IString_size
        ~stack_type:(string @$ bot)
        ~kinstr:(IString_size (dummy_loc, halt))
        ()
  end

  module Bytes = struct
    (* Copy of [String] modulo renaming string to bytes. *)

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IConcat_bytes
        ~intercept_stack:(Script_list.empty, eos)
        ~stack_type:(list bytes @$ bot)
        ~kinstr:(IConcat_bytes (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IConcat_bytes_pair
        ~intercept_stack:(Bytes.empty, (Bytes.empty, eos))
        ~stack_type:(bytes @$ bytes @$ bot)
        ~kinstr:(IConcat_bytes_pair (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISlice_bytes
        ~stack_type:(nat @$ nat @$ bytes @$ bot)
        ~kinstr:(ISlice_bytes (dummy_loc, halt))
        ~intercept_stack:
          (let z = Script_int.zero_n in
           (z, (z, (Bytes.empty, eos))))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let bytes =
              Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
            in
            let len = nat_of_positive_int (Bytes.length bytes) in
            (* worst case: offset = 0 *)
            (nat_of_positive_int 0, (len, (bytes, eos))))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IBytes_size
        ~stack_type:(bytes @$ bot)
        ~kinstr:(IBytes_size (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAnd_bytes
        ~intercept_stack:(Bytes.empty, (Bytes.empty, eos))
        ~stack_type:(bytes @$ bytes @$ bot)
        ~kinstr:(IAnd_bytes (dummy_loc, halt))
        ()

    let stack_sampler_for_or_and_xor_on_bytes cfg rng_state =
      let _, (module Samplers) =
        make_default_samplers cfg.Default_config.sampler
      in
      fun () ->
        (* We benchmark the worst cases: when the two bytes have
               the same length *)
        let bytes1 =
          Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
        in
        let bytes2 =
          Bytes.init (Bytes.length bytes1) (fun _ ->
              Char.chr (Random.State.int rng_state 256))
        in
        (bytes1, (bytes2, eos))

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IOr_bytes
        ~intercept_stack:(Bytes.empty, (Bytes.empty, eos))
        ~stack_type:(bytes @$ bytes @$ bot)
        ~kinstr:(IOr_bytes (dummy_loc, halt))
        ~stack_sampler:stack_sampler_for_or_and_xor_on_bytes
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IXor_bytes
        ~intercept_stack:(Bytes.empty, (Bytes.empty, eos))
        ~stack_type:(bytes @$ bytes @$ bot)
        ~kinstr:(IXor_bytes (dummy_loc, halt))
        ~stack_sampler:stack_sampler_for_or_and_xor_on_bytes
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_INot_bytes
        ~intercept_stack:(Bytes.empty, eos)
        ~stack_type:(bytes @$ bot)
        ~kinstr:(INot_bytes (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ILsl_bytes
        ~intercept_stack:(Bytes.empty, (Script_int.one_n, eos))
        ~stack_type:(bytes @$ nat @$ bot)
        ~kinstr:(ILsl_bytes (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let bytes =
              Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
            in
            (* Avoid [n mod 8 = 0] which runs faster than the others. *)
            let n =
              (* 0-63999 without multiples of 8 *)
              let n = Random.State.int rng_state 56000 in
              (n / 7 * 8) + (n mod 7) + 1
            in
            let shift = Script_int.(abs (of_int n)) in
            (bytes, (shift, eos)))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ILsr_bytes
        ~intercept_stack:(Bytes.empty, (Script_int.one_n, eos))
        ~stack_type:(bytes @$ nat @$ bot)
        ~kinstr:(ILsr_bytes (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let bytes =
              Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
            in
            (* No need of samples of shift > bytes * 8 which are equivalent with
               the case of shift = bytes * 8 where LSR returns empty bytes immediately *)
            (* Avoid [n mod 8 = 0] which runs faster than the others. *)
            let n =
              let n =
                Random.State.int rng_state ((Bytes.length bytes * 7) + 1)
              in
              (n / 7 * 8) + (n mod 7) + 1
            in
            let shift = Script_int.(abs (of_int n)) in
            (bytes, (shift, eos)))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBytes_nat
        ~stack_type:(nat @$ bot)
        ~kinstr:(IBytes_nat (dummy_loc, halt))
        ~intercept_stack:(Script_int.one_n, eos)
          (* Avoid the optimized case of 0 *)
        ~stack_sampler:(fun cfg rng_state ->
          let base_parameters =
            {cfg.sampler.base_parameters with int_size = {min = 0; max = 4096}}
          in
          let sampler = {cfg.sampler with base_parameters} in
          let _, (module Samplers) = make_default_samplers sampler in
          fun () ->
            let nat =
              Samplers.Random_value.value Script_typed_ir.nat_t rng_state
            in
            (nat, eos))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_INat_bytes
        ~stack_type:(bytes @$ bot)
        ~kinstr:(INat_bytes (dummy_loc, halt))
        ~intercept_stack:(Bytes.empty, eos)
        ~stack_sampler:(fun cfg rng_state ->
          let base_parameters =
            {
              cfg.sampler.base_parameters with
              bytes_size = {min = 0; max = 4096};
            }
          in
          let sampler = {cfg.sampler with base_parameters} in
          let _, (module Samplers) = make_default_samplers sampler in
          fun () ->
            let bytes =
              Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
            in
            (bytes, eos))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBytes_int
        ~stack_type:(int @$ bot)
        ~kinstr:(IBytes_int (dummy_loc, halt))
        ~intercept_stack:(Script_int.one, eos)
          (* Avoid the optimized case of 0 *)
        ~stack_sampler:(fun cfg rng_state ->
          let base_parameters =
            {cfg.sampler.base_parameters with int_size = {min = 0; max = 4096}}
          in
          let sampler = {cfg.sampler with base_parameters} in
          let _, (module Samplers) = make_default_samplers sampler in
          fun () ->
            let int =
              Samplers.Random_value.value Script_typed_ir.int_t rng_state
            in
            (int, eos))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IInt_bytes
        ~stack_type:(bytes @$ bot)
        ~kinstr:(IInt_bytes (dummy_loc, halt))
        ~intercept_stack:(Bytes.empty, eos)
        ~stack_sampler:(fun cfg rng_state ->
          let base_parameters =
            {
              cfg.sampler.base_parameters with
              bytes_size = {min = 0; max = 4096};
            }
          in
          let sampler = {cfg.sampler with base_parameters} in
          let _, (module Samplers) = make_default_samplers sampler in
          fun () ->
            let bytes =
              Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
            in
            (bytes, eos))
        ()
  end

  module Timestamps = struct
    let zero_timestamp = Script_timestamp.of_zint Z.zero

    let zero_int = Script_int.zero

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAdd_seconds_to_timestamp
        ~intercept_stack:(zero_int, (zero_timestamp, eos))
        ~stack_type:(int @$ timestamp @$ bot)
        ~kinstr:(IAdd_seconds_to_timestamp (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAdd_timestamp_to_seconds
        ~intercept_stack:(zero_timestamp, (zero_int, eos))
        ~stack_type:(timestamp @$ int @$ bot)
        ~kinstr:(IAdd_timestamp_to_seconds (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ISub_timestamp_seconds
        ~intercept_stack:(zero_timestamp, (zero_int, eos))
        ~stack_type:(timestamp @$ int @$ bot)
        ~kinstr:(ISub_timestamp_seconds (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IDiff_timestamps
        ~intercept_stack:(zero_timestamp, (zero_timestamp, eos))
        ~stack_type:(timestamp @$ timestamp @$ bot)
        ~kinstr:(IDiff_timestamps (dummy_loc, halt))
        ()
  end

  module Tez = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAdd_tez
        ~stack_type:(mutez @$ mutez @$ bot)
        ~kinstr:(IAdd_tez (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISub_tez
        ~stack_type:(mutez @$ mutez @$ bot)
        ~kinstr:(ISub_tez (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) =
            make_default_samplers cfg.Default_config.sampler
          in
          fun () ->
            let a = Samplers.Random_value.value mutez rng_state in
            let b =
              match Alpha_context.Tez.(a /? 2L) with
              | Error _ -> assert false
              | Ok x -> x
            in
            (a, (b, eos)))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISub_tez_legacy
        ~stack_type:(mutez @$ mutez @$ bot)
        ~kinstr:(ISub_tez_legacy (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) =
            make_default_samplers cfg.Default_config.sampler
          in
          fun () ->
            let a = Samplers.Random_value.value mutez rng_state in
            let b =
              match Alpha_context.Tez.(a /? 2L) with
              | Error _ -> assert false
              | Ok x -> x
            in
            (a, (b, eos)))
        ()

    let sample_tez_nat (module Samplers : Michelson_samplers.S) rng_state =
      let mutez = Samplers.Random_value.value mutez rng_state in
      let mutez_int64 = Alpha_context.Tez.to_mutez mutez in
      let int64 = Int64.(div max_int (mul mutez_int64 2L)) in
      let nat =
        match Script_int.(is_nat (of_int64 int64)) with
        | None -> assert false
        | Some nat -> nat
      in
      (mutez, nat)

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMul_teznat
        ~stack_type:(mutez @$ nat @$ bot)
        ~kinstr:(IMul_teznat (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, samplers = make_default_samplers cfg.sampler in
          fun () ->
            let mutez, nat = sample_tez_nat samplers rng_state in
            (mutez, (nat, eos)))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMul_nattez
        ~stack_type:(nat @$ mutez @$ bot)
        ~kinstr:(IMul_nattez (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, samplers = make_default_samplers cfg.sampler in
          fun () ->
            let mutez, nat = sample_tez_nat samplers rng_state in
            (nat, (mutez, eos)))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IEdiv_teznat
        ~stack_type:(mutez @$ nat @$ bot)
        ~kinstr:(IEdiv_teznat (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, samplers = make_default_samplers cfg.sampler in
          fun () ->
            let mutez, nat = sample_tez_nat samplers rng_state in
            (mutez, (nat, eos)))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IEdiv_tez
        ~stack_type:(mutez @$ mutez @$ bot)
        ~kinstr:(IEdiv_tez (dummy_loc, halt))
        ()
  end

  module Booleans = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IOr
        ~stack_type:(bool @$ bool @$ bot)
        ~kinstr:(IOr (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAnd
        ~stack_type:(bool @$ bool @$ bot)
        ~kinstr:(IAnd (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IXor
        ~stack_type:(bool @$ bool @$ bot)
        ~kinstr:(IXor (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_INot
        ~stack_type:(bool @$ bot)
        ~kinstr:(INot (dummy_loc, halt))
        ()
  end

  module Integers = struct
    let zero = Script_int.zero

    let zero_n = Script_int.zero_n

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IIs_nat
        ~stack_type:(int @$ bot)
        ~kinstr:(IIs_nat (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_INeg
        ~intercept_stack:(zero, eos)
        ~stack_type:(int @$ bot)
        ~kinstr:(INeg (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IAbs_int
        ~stack_type:(int @$ bot)
        ~kinstr:(IAbs_int (dummy_loc, halt))
        ~intercept_stack:(zero, eos)
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let x = Samplers.Michelson_base.nat rng_state in
            let neg_x = Script_int.neg x in
            (neg_x, eos))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IInt_nat
        ~stack_type:(nat @$ bot)
        ~kinstr:(IInt_nat (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAdd_int
        ~intercept_stack:(zero, (zero, eos))
        ~stack_type:(int @$ int @$ bot)
        ~kinstr:(IAdd_int (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAdd_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~stack_type:(nat @$ nat @$ bot)
        ~kinstr:(IAdd_nat (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ISub_int
        ~intercept_stack:(zero, (zero, eos))
        ~stack_type:(int @$ int @$ bot)
        ~kinstr:(ISub_int (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IMul_int
        ~intercept_stack:(zero, (zero, eos))
        ~stack_type:(int @$ int @$ bot)
        ~kinstr:(IMul_int (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IMul_nat
        ~intercept_stack:(zero_n, (zero, eos))
        ~stack_type:(nat @$ int @$ bot)
        ~kinstr:(IMul_nat (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IEdiv_int
        ~intercept_stack:(zero, (zero, eos))
        ~stack_type:(int @$ int @$ bot)
        ~kinstr:(IEdiv_int (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            (* When x > 0 and size(x) < size(y), EDIV x y evaluates to (0,x) and nothing is allocated.
               The following sampler avoids such cases that lead to underestimation. *)
            let x = Script_int.neg @@ Samplers.Michelson_base.nat rng_state in
            let y = Samplers.Michelson_base.int rng_state in
            (x, (y, eos)))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IEdiv_nat
        ~intercept_stack:(zero_n, (zero, eos))
        ~stack_type:(nat @$ int @$ bot)
        ~kinstr:(IEdiv_nat (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ILsl_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~stack_type:(nat @$ nat @$ bot)
        ~kinstr:(ILsl_nat (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let x = Samplers.Michelson_base.nat rng_state in
            (* shift must be in [0;256]: 1 byte max *)
            let shift =
              Script_int.(abs (of_int (Random.State.int rng_state 256)))
            in
            (x, (shift, eos)))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ILsr_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~stack_type:(nat @$ nat @$ bot)
        ~kinstr:(ILsr_nat (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let x = Samplers.Michelson_base.nat rng_state in
            (* shift must be in [0;256]: 1 byte max *)
            let shift =
              Script_int.(abs (of_int (Random.State.int rng_state 256)))
            in
            (x, (shift, eos)))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IOr_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~stack_type:(nat @$ nat @$ bot)
        ~kinstr:(IOr_nat (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAnd_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~stack_type:(nat @$ nat @$ bot)
        ~kinstr:(IAnd_nat (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IAnd_int_nat
        ~intercept_stack:(zero, (zero_n, eos))
        ~stack_type:(int @$ nat @$ bot)
        ~kinstr:(IAnd_int_nat (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            (* The worst case is size(and(x,y)) = size(y) when x < 0. *)
            let x = Script_int.neg @@ Samplers.Michelson_base.nat rng_state in
            let y = Samplers.Michelson_base.nat rng_state in
            (x, (y, eos)))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IXor_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~stack_type:(nat @$ nat @$ bot)
        ~kinstr:(IXor_nat (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_INot_int
        ~intercept_stack:(zero, eos)
        ~stack_type:(int @$ bot)
        ~kinstr:(INot_int (dummy_loc, halt))
        ()
  end

  module Control = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IIf
        ~stack_type:(bool @$ unit @$ bot)
        ~kinstr:
          (IIf
             {
               loc = dummy_loc;
               branch_if_true = halt;
               branch_if_false = halt;
               k = halt;
             })
        ()

    let () =
      let push_false = IPush (dummy_loc, bool, false, halt) in
      (* The case true is on top of stack
         ILoop ->
         IPush false ->
         IHalt
      *)
      time_alloc_benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_ILoop_in
        ~stack:(true, eos)
        ~stack_type:(bool @$ bot)
        ~kinstr:(ILoop (dummy_loc, push_false, halt))
        () ;
      (* The case false is on top of stack
         ILoop ->
         IHalt
      *)
      time_alloc_benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_ILoop_out
        ~stack:(false, eos)
        ~stack_type:(bool @$ bot)
        ~kinstr:(ILoop (dummy_loc, push_false, halt))
        ()

    let () =
      let model =
        Interpreter_model.Models.max_branching_model
          ~case_0:"in_const"
          ~case_1:"out_const"
          "N_ILoop"
      in
      register_model_for_code_generation model

    let () =
      let cons_r = ICons_right (dummy_loc, unit, halt) in
      (*
          ILoop_left ->
          ICons_right ->
          IHalt
       *)
      time_alloc_benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_ILoop_left_in
        ~stack:(L (), eos)
        ~stack_type:(cor unit unit @$ bot)
        ~kinstr:(ILoop_left (dummy_loc, cons_r, halt))
        () ;
      (*
        ILoop_left ->
        IHalt
       *)
      time_alloc_benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_ILoop_left_out
        ~stack:(R (), eos)
        ~stack_type:(cor unit unit @$ bot)
        ~kinstr:(ILoop_left (dummy_loc, cons_r, halt))
        ()

    let () =
      let model =
        Interpreter_model.Models.max_branching_model
          ~case_0:"in_const"
          ~case_1:"out_const"
          "N_ILoop_left"
      in
      register_model_for_code_generation model

    let () =
      (*
        IDip ->
        IHalt ->
        IPush ->
        IHalt
       *)
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IDip
        ~stack_type:(unit @$ unit @$ bot)
        ~kinstr:(IDip (dummy_loc, halt, Some unit, halt))
        ()

    let dummy_lambda =
      let open Script_typed_ir in
      let descr =
        {
          kloc = dummy_loc;
          kbef = unit @$ bot;
          kaft = unit @$ bot;
          kinstr = halt;
        }
      in
      Lam (descr, Micheline.Int (dummy_loc, Z.zero))

    let dummy_lambda_rec =
      let open Script_typed_ir in
      let descr =
        {
          kloc = dummy_loc;
          kbef = unit @$ lambda unit unit @$ bot;
          kaft = unit @$ bot;
          kinstr =
            IDrop
              (dummy_loc, IDrop (dummy_loc, IPush (dummy_loc, unit, (), halt)));
        }
      in
      LamRec (descr, Micheline.Int (dummy_loc, Z.zero))

    let () =
      (*
        IExec ->
        (switch to in-context gas-counting) ->
        interp lambda code ->
        IHalt
       *)
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IExec
        ~stack_type:(unit @$ lambda unit unit @$ bot)
        ~kinstr:(IExec (dummy_loc, Some (unit @$ bot), halt))
        ~stack_sampler:(fun _cfg rng_state () ->
          if Base_samplers.uniform_bool rng_state then ((), (dummy_lambda, eos))
          else ((), (dummy_lambda_rec, eos)))
        ()

    let () =
      (*
        IApply ->
        unparse unit ->
        unparse unit_ty ->
        construct term ->
        IHalt
       *)
      let dummy_lambda_pair =
        let open Script_typed_ir in
        let descr =
          {
            kloc = dummy_loc;
            kbef = cpair unit unit @$ bot;
            kaft = unit @$ bot;
            kinstr = ICdr (dummy_loc, halt);
          }
        in
        Lam (descr, Micheline.Int (dummy_loc, Z.zero))
      in
      let dummy_lambda_pair_rec =
        let open Script_typed_ir in
        let descr =
          {
            kloc = dummy_loc;
            kbef = cpair unit unit @$ lambda (cpair unit unit) unit @$ bot;
            kaft = unit @$ bot;
            kinstr =
              IDrop
                (dummy_loc, IDrop (dummy_loc, IPush (dummy_loc, unit, (), halt)));
          }
        in
        LamRec (descr, Micheline.Int (dummy_loc, Z.zero))
      in
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IApply
        ~stack_type:(unit @$ lambda (cpair unit unit) unit @$ bot)
        ~kinstr:(IApply (dummy_loc, unit, halt))
        ~stack_sampler:(fun _cfg rng_state () ->
          if Base_samplers.uniform_bool rng_state then
            ((), (dummy_lambda_pair, eos))
          else ((), (dummy_lambda_pair_rec, eos)))
        ()

    let () =
      (*
        ILambda ->
        IHalt
       *)
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ILambda_lam
        ~stack_type:(unit @$ bot)
        ~kinstr:(ILambda (dummy_loc, dummy_lambda, halt))
        ()

    let () =
      (*
        ILambda (rec) ->
        IHalt
       *)
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ILambda_lamrec
        ~stack_type:(unit @$ bot)
        ~kinstr:(ILambda (dummy_loc, dummy_lambda_rec, halt))
        ()

    let () =
      let model =
        Interpreter_model.Models.max_branching_model
          ~case_0:"lam_const"
          ~case_1:"lamrec_const"
          "N_ILambda"
      in
      register_model_for_code_generation model

    let () =
      (*
        IFailwith ->
        (unparse_data Unit) ->
        (strip_locations) ->
        fail
       *)
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IFailwith
        ~amplification:100
        ~stack_type:(unit @$ bot)
        ~kinstr:(IFailwith (dummy_loc, unit))
        ()
  end

  module Comparison = struct
    let () =
      benchmark
        ~name:Interpreter_workload.N_ICompare
        ~kinstr_and_stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let size =
              Base_samplers.sample_in_interval
                rng_state
                ~range:cfg.compare.type_size
            in
            let (Script_ir_translator.Ex_comparable_ty ty) =
              Samplers.Random_type.m_comparable_type ~size rng_state
            in
            let value = Samplers.Random_value.comparable ty rng_state in
            let kinstr = ICompare (dummy_loc, ty, halt) in
            Ex_stack_and_kinstr
              {
                stack = (value, (value, eos));
                stack_type = ty @$ ty @$ bot;
                kinstr;
              })
        ()

    let () =
      benchmark
        ~name:Interpreter_workload.N_ICompare
        ~salt:"_intercepts"
        ~kinstr_and_stack_sampler:(fun cfg rng_state ->
          (* Sample from values whose [size_of_comparable_value] is 1. *)
          (* [unit] and [bool] are excluded since they are relatively quick. *)
          let sampler =
            {
              cfg.sampler with
              base_parameters =
                {
                  int_size = {min = 1; max = 1};
                  string_size = {min = 1; max = 1};
                  bytes_size = {min = 1; max = 1};
                };
            }
          in
          let _, (module Samplers) = make_default_samplers sampler in
          fun () ->
            let ty : Script_ir_translator.ex_comparable_ty =
              match Random.State.int rng_state 4 with
              | 0 -> Ex_comparable_ty String_t
              | 1 -> Ex_comparable_ty Bytes_t
              | 2 -> Ex_comparable_ty Int_t
              | 3 -> Ex_comparable_ty Nat_t
              | _ -> assert false
            in
            let (Ex_comparable_ty ty) = ty in
            let value = Samplers.Random_value.comparable ty rng_state in
            let kinstr = ICompare (dummy_loc, ty, halt) in
            Ex_stack_and_kinstr
              {
                stack = (value, (value, eos));
                stack_type = ty @$ ty @$ bot;
                kinstr;
              })
        ()

    let () =
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_ICompare
        ~stack_type:(string @$ string @$ bot)
        ~kinstr:(ICompare (dummy_loc, String_t, halt))
        ()

    let () = register_time_alloc_codegen_model (Instr_name N_ICompare)
  end

  module Comparators = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IEq
        ~stack_type:(int @$ bot)
        ~kinstr:(IEq (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_INeq
        ~stack_type:(int @$ bot)
        ~kinstr:(INeq (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ILt
        ~stack_type:(int @$ bot)
        ~kinstr:(ILt (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IGt
        ~stack_type:(int @$ bot)
        ~kinstr:(IGt (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ILe
        ~stack_type:(int @$ bot)
        ~kinstr:(ILe (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IGe
        ~stack_type:(int @$ bot)
        ~kinstr:(IGe (dummy_loc, halt))
        ()
  end

  module Proto = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAddress
        ~stack_type:(contract unit @$ bot)
        ~kinstr:(IAddress (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IContract
        ~stack_type:(address @$ bot)
        ~kinstr:
          (IContract (dummy_loc, unit, Alpha_context.Entrypoint.default, halt))
        ()

    let () =
      simple_time_alloc_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ITransfer_tokens
        ~stack_type:(unit @$ mutez @$ contract unit @$ bot)
        ~kinstr:(ITransfer_tokens (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let contract =
              Samplers.Random_value.value (contract unit) rng_state
            in
            let amount =
              match contract with
              | Typed_implicit _ | Typed_originated _ ->
                  Samplers.Random_value.value mutez rng_state
              | Typed_sc_rollup _ -> Alpha_context.Tez.zero
            in
            ((), (amount, (contract, eos))))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IImplicit_account
        ~stack_type:(key_hash @$ bot)
        ~kinstr:(IImplicit_account (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ICreate_contract
        ~stack_type:(option key_hash @$ mutez @$ unit @$ bot)
        ~kinstr:
          (ICreate_contract
             {
               loc = dummy_loc;
               storage_type = unit;
               code = Micheline.(strip_locations @@ Seq (0, []));
               k = halt;
             })
        ()

    let () =
      let name =
        match Protocol.Script_string.of_string "view" with
        | Ok s -> s
        | Error _ -> assert false
      in
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IView
        ~stack_type:(unit @$ address @$ bot)
        ~kinstr:
          (IView
             ( dummy_loc,
               View_signature {name; input_ty = unit; output_ty = unit},
               Some bot,
               halt ))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ISet_delegate
        ~stack_type:(option key_hash @$ bot)
        ~kinstr:(ISet_delegate (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_INow
        ~stack_type:(unit @$ bot)
        ~kinstr:(INow (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IMin_block_time
        ~stack_type:bot
        ~kinstr:(IMin_block_time (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IBalance
        ~stack_type:(unit @$ bot)
        ~kinstr:(IBalance (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ILevel
        ~stack_type:(unit @$ bot)
        ~kinstr:(ILevel (dummy_loc, halt))
        ()

    let name_of_algo = function
      | Signature.Ed25519 -> Interpreter_workload.N_ICheck_signature_ed25519
      | Signature.Secp256k1 -> Interpreter_workload.N_ICheck_signature_secp256k1
      | Signature.P256 -> Interpreter_workload.N_ICheck_signature_p256
      | Signature.Bls -> Interpreter_workload.N_ICheck_signature_bls

    let check_signature (algo : Signature.algo) ~benchmark_type ~for_intercept =
      benchmark_with_stack_sampler
        ~benchmark_type
        ~intercept:for_intercept
        ~name:(name_of_algo algo)
        ~stack_type:(public_key @$ signature @$ bytes @$ bot)
        ~kinstr:(ICheck_signature (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let (module Crypto_samplers), (module Samplers) =
            make_default_samplers ~algo:(`Algo algo) cfg.Default_config.sampler
          in
          fun () ->
            let _pkh, pk, sk = Crypto_samplers.all rng_state in
            let unsigned_message =
              if for_intercept then Environment.Bytes.empty
              else Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
            in
            let signed_message = Signature.sign sk unsigned_message in
            let signed_message = Script_signature.make signed_message in
            (pk, (signed_message, (unsigned_message, eos))))
        ()

    let check_signature algo =
      check_signature algo ~benchmark_type:Time ~for_intercept:true ;
      check_signature algo ~benchmark_type:Time ~for_intercept:false ;
      check_signature algo ~benchmark_type:Alloc ~for_intercept:false ;
      register_time_alloc_codegen_model (Instr_name (name_of_algo algo))

    let () = check_signature Signature.Ed25519

    let () = check_signature Signature.Secp256k1

    let () = check_signature Signature.P256

    let () = check_signature Signature.Bls

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IHash_key
        ~stack_type:(public_key @$ bot)
        ~kinstr:(IHash_key (dummy_loc, halt))
        ()

    let () =
      time_alloc_benchmark
        ~name:Interpreter_workload.N_IPack
        ~kinstr_and_stack_sampler:(fun _cfg _rng_state ->
          let kinstr = IPack (dummy_loc, unit, halt) in
          fun () ->
            Ex_stack_and_kinstr
              {stack = ((), eos); stack_type = unit @$ bot; kinstr})
        ()

    let () =
      let open Lwt_result_syntax in
      time_alloc_benchmark
        ~name:Interpreter_workload.N_IUnpack
        ~kinstr_and_stack_sampler:(fun _cfg rng_state ->
          let b =
            raise_if_error
              (Lwt_main.run
                 (let* ctxt, _ = Execution_context.make ~rng_state () in
                  let* bytes, _ =
                    let*! result =
                      Script_ir_translator.pack_data ctxt unit ()
                    in
                    Lwt.return (Environment.wrap_tzresult result)
                  in
                  return bytes))
          in
          let kinstr = IUnpack (dummy_loc, unit, halt) in
          fun () ->
            Ex_stack_and_kinstr
              {stack = (b, eos); stack_type = bytes @$ bot; kinstr})
        ()

    let () =
      simple_benchmark
        ~benchmark_type:Time
        ~name:Interpreter_workload.N_IBlake2b
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~stack_type:(bytes @$ bot)
        ~kinstr:(IBlake2b (dummy_loc, halt))
        () ;
      (* no intercept for alloc *)
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_IBlake2b
        ~stack_type:(bytes @$ bot)
        ~kinstr:(IBlake2b (dummy_loc, halt))
        () ;
      register_time_alloc_codegen_model (Instr_name N_IBlake2b)

    let () =
      simple_benchmark
        ~benchmark_type:Time
        ~name:Interpreter_workload.N_ISha256
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~stack_type:(bytes @$ bot)
        ~kinstr:(ISha256 (dummy_loc, halt))
        () ;
      (* no intercept for alloc *)
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_ISha256
        ~stack_type:(bytes @$ bot)
        ~kinstr:(ISha256 (dummy_loc, halt))
        () ;
      register_time_alloc_codegen_model (Instr_name N_ISha256)

    let () =
      simple_benchmark
        ~benchmark_type:Time
        ~name:Interpreter_workload.N_ISha512
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~stack_type:(bytes @$ bot)
        ~kinstr:(ISha512 (dummy_loc, halt))
        () ;
      (* no intercept for alloc *)
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_ISha512
        ~stack_type:(bytes @$ bot)
        ~kinstr:(ISha512 (dummy_loc, halt))
        () ;
      register_time_alloc_codegen_model (Instr_name N_ISha512)

    let () =
      simple_benchmark
        ~benchmark_type:Time
        ~name:Interpreter_workload.N_IKeccak
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~stack_type:(bytes @$ bot)
        ~kinstr:(IKeccak (dummy_loc, halt))
        () ;
      (* no intercept for alloc *)
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_IKeccak
        ~stack_type:(bytes @$ bot)
        ~kinstr:(IKeccak (dummy_loc, halt))
        () ;
      register_time_alloc_codegen_model (Instr_name N_IKeccak)

    let () =
      simple_benchmark
        ~benchmark_type:Time
        ~name:Interpreter_workload.N_ISha3
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~stack_type:(bytes @$ bot)
        ~kinstr:(ISha3 (dummy_loc, halt))
        () ;
      (* no intercept for alloc *)
      simple_benchmark
        ~benchmark_type:Alloc
        ~name:Interpreter_workload.N_ISha3
        ~stack_type:(bytes @$ bot)
        ~kinstr:(ISha3 (dummy_loc, halt))
        () ;
      register_time_alloc_codegen_model (Instr_name N_ISha3)

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ISource
        ~stack_type:(unit @$ bot)
        ~kinstr:(ISource (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ISender
        ~stack_type:(unit @$ bot)
        ~kinstr:(ISender (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ISelf
        ~stack_type:(unit @$ bot)
        ~kinstr:
          (ISelf (dummy_loc, unit, Alpha_context.Entrypoint.default, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ISelf_address
        ~stack_type:(unit @$ bot)
        ~kinstr:(ISelf_address (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IAmount
        ~stack_type:(unit @$ bot)
        ~kinstr:(IAmount (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IChainId
        ~stack_type:(unit @$ bot)
        ~kinstr:(IChainId (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IVoting_power
        ~stack_type:(key_hash @$ bot)
        ~kinstr:(IVoting_power (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ITotal_voting_power
        ~stack_type:(unit @$ bot)
        ~kinstr:(ITotal_voting_power (dummy_loc, halt))
        ()
  end

  let () =
    let memo_size =
      match Alpha_context.Sapling.Memo_size.parse_z Z.zero with
      | Error _ -> assert false
      | Ok sz -> sz
    in
    simple_time_alloc_benchmark
      ~name:Interpreter_workload.N_ISapling_empty_state
      ~stack_type:(unit @$ bot)
      ~kinstr:(ISapling_empty_state (dummy_loc, memo_size, halt))
      ()

  module type Type_transaction = sig
    val type_transaction : Sapling_generation.type_transaction

    val suffix : string
  end

  module Register_Sapling_benchmark (Type_transaction : Type_transaction) =
  struct
    let is_empty =
      match Type_transaction.type_transaction with Empty -> true | _ -> false

    let () =
      (* Note that memo_size is hardcoded to 0 in module [Sapling_generation]. *)
      let memo_size =
        match Alpha_context.Sapling.Memo_size.parse_z Z.zero with
        | Error _ -> assert false
        | Ok sz -> sz
      in
      let module B (BenchmarkType : sig
        val value : benchmark_type
      end) : Benchmark.S = struct
        let info, name =
          info_and_name
            ~benchmark_type:BenchmarkType.value
            ~intercept:is_empty
            ("ISapling_verify_update_" ^ Type_transaction.suffix)

        let module_filename = __FILE__

        let purpose = Benchmark.Generate_code "michelson_v1_gas"

        include Default_config
        include Default_boilerplate

        let models =
          Interpreter_model.make_model
            BenchmarkType.value
            (Instr_name Interpreter_workload.N_ISapling_verify_update)

        let stack_type =
          let spl_state = sapling_state memo_size in
          let spl_tx = sapling_transaction memo_size in
          spl_tx @$ spl_state @$ bot

        let kinstr = ISapling_verify_update (dummy_loc, halt)

        let prepare_sapling_execution_environment sapling_forge_rng_seed
            sapling_transition =
          let open Lwt_result_syntax in
          let sapling_forge_rng_state =
            Random.State.make
            @@ Option.fold
                 ~none:Sapling_generation.shared_seed
                 ~some:(fun seed -> [|seed|])
                 sapling_forge_rng_seed
          in
          (* Prepare context. We _must_ reuse the same seed as the one used for
               the context when generating the transactions. This ensures that the
               bootstrap account match and that the transactions can be replayed. *)
          let result =
            Lwt_main.run
              (let* ctxt, step_constants =
                 Execution_context.make ~rng_state:sapling_forge_rng_state ()
               in
               (* Prepare a sapling state able to replay the transition. *)
               let* _, _, _, _, ctxt, state_id =
                 Sapling_generation.prepare_seeded_state sapling_transition ctxt
               in
               let* state, ctxt =
                 let*! result =
                   Alpha_context.Sapling.(
                     state_from_id ctxt (Id.parse_z state_id))
                 in
                 Lwt.return (Environment.wrap_tzresult result)
               in
               return (ctxt, state, step_constants))
          in
          match result with
          | Ok r -> r
          | Error _ ->
              Format.eprintf
                "Error in prepare_sapling_execution_environment, aborting@." ;
              Stdlib.failwith "prepare_sapling_execution_environment"

        let create_benchmarks ~rng_state ~bench_num (config : config) =
          ignore rng_state ;
          match config.sapling with
          | {sapling_txs_file; seed} ->
              let transitions =
                Sapling_generation.load
                  ~filename:sapling_txs_file
                  Type_transaction.type_transaction
              in
              let length = List.length transitions in
              if length < bench_num && not is_empty then
                Format.eprintf
                  "ISapling_verify_update: warning, only %d available \
                   transactions (requested %d)@."
                  length
                  bench_num ;
              let transitions =
                List.take_n (min bench_num length) transitions
              in
              List.map
                (fun (_, transition) () ->
                  let ctxt, state, step_constants =
                    prepare_sapling_execution_environment seed transition
                  in
                  let address =
                    Alpha_context.Contract.(
                      to_b58check (Originated step_constants.self))
                  in
                  let chain_id =
                    Environment.Chain_id.to_b58check step_constants.chain_id
                  in
                  let anti_replay = address ^ chain_id in
                  (* Checks that the transaction is correct*)
                  let () =
                    match
                      Sapling_validator.verify_update
                        (Sapling_generation.alpha_to_raw ctxt)
                        (Obj.magic state)
                        transition.sapling_tx
                        anti_replay
                      |> Lwt_main.run
                    with
                    | Ok (_, Some _) -> ()
                    | Ok (_, None) ->
                        Stdlib.failwith "benchmarked transaction is incorrect"
                    | _ -> assert false
                  in
                  let stack_instr =
                    Ex_stack_and_kinstr
                      {
                        stack = (transition.sapling_tx, (state, eos));
                        stack_type;
                        kinstr;
                      }
                  in
                  benchmark_from_kinstr_and_stack
                    BenchmarkType.value
                    ctxt
                    step_constants
                    stack_instr)
                transitions
      end in
      Registration_helpers.register
        (module B (struct
          let value = Time
        end)) ;
      Registration_helpers.register
        (module B (struct
          let value = Alloc
        end)) ;
      register_time_alloc_codegen_model
        (Instr_name Interpreter_workload.N_ISapling_verify_update)
  end

  module Sapling_empty = struct
    let module A = Register_Sapling_benchmark (struct
      let type_transaction = Sapling_generation.Empty

      let suffix = "empty"
    end) in
    ()
  end

  module Sapling_non_empty = struct
    let module A = Register_Sapling_benchmark (struct
      let type_transaction = Sapling_generation.Non_empty

      let suffix = "non_empty"
    end) in
    ()
  end

  (* when benchmarking, compile bls12-381-unix without ADX, see
     https://gitlab.com/dannywillems/ocaml-bls12-381/-/blob/71d0b4d467fbfaa6452d702fcc408d7a70916a80/README.md#install
  *)
  module Bls12_381 = struct
    let check () =
      if not Bls12_381.built_with_blst_portable then (
        Format.eprintf
          "BLS must be built without ADX to run the BLS benchmarks. Try \
           compiling again after setting the environment variable \
           BLST_PORTABLE. Aborting.@." ;
        Stdlib.failwith "bls_not_built_with_blst_portable")

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IAdd_bls12_381_g1
        ~stack_type:(bls12_381_g1 @$ bls12_381_g1 @$ bot)
        ~kinstr:(IAdd_bls12_381_g1 (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IAdd_bls12_381_g2
        ~stack_type:(bls12_381_g2 @$ bls12_381_g2 @$ bot)
        ~kinstr:(IAdd_bls12_381_g2 (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IAdd_bls12_381_fr
        ~stack_type:(bls12_381_fr @$ bls12_381_fr @$ bot)
        ~kinstr:(IAdd_bls12_381_fr (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IMul_bls12_381_g1
        ~stack_type:(bls12_381_g1 @$ bls12_381_fr @$ bot)
        ~kinstr:(IMul_bls12_381_g1 (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IMul_bls12_381_g2
        ~stack_type:(bls12_381_g2 @$ bls12_381_fr @$ bot)
        ~kinstr:(IMul_bls12_381_g2 (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IMul_bls12_381_fr
        ~stack_type:(bls12_381_fr @$ bls12_381_fr @$ bot)
        ~kinstr:(IMul_bls12_381_fr (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IMul_bls12_381_z_fr
        ~stack_type:(bls12_381_fr @$ int @$ bot)
        ~kinstr:(IMul_bls12_381_z_fr (dummy_loc, halt))
        ()

    let () =
      benchmark_with_stack_sampler
        ~check
        ~name:Interpreter_workload.N_IMul_bls12_381_z_fr
        ~intercept:true
        ~stack_type:(bls12_381_fr @$ int @$ bot)
        ~kinstr:(IMul_bls12_381_z_fr (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          let fr_sampler = Samplers.Random_value.value bls12_381_fr in
          let zero = Script_int.zero in
          fun () -> (fr_sampler rng_state, (zero, eos)))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IMul_bls12_381_fr_z
        ~stack_type:(int @$ bls12_381_fr @$ bot)
        ~kinstr:(IMul_bls12_381_fr_z (dummy_loc, halt))
        ()

    let () =
      benchmark_with_stack_sampler
        ~check
        ~name:Interpreter_workload.N_IMul_bls12_381_fr_z
        ~intercept:true
        ~stack_type:(int @$ bls12_381_fr @$ bot)
        ~kinstr:(IMul_bls12_381_fr_z (dummy_loc, halt))
        ~stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          let fr_sampler = Samplers.Random_value.value bls12_381_fr in
          let zero = Script_int.zero in
          fun () -> (zero, (fr_sampler rng_state, eos)))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_IInt_bls12_381_z_fr
        ~stack_type:(bls12_381_fr @$ bot)
        ~kinstr:(IInt_bls12_381_fr (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_INeg_bls12_381_g1
        ~stack_type:(bls12_381_g1 @$ bot)
        ~kinstr:(INeg_bls12_381_g1 (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_INeg_bls12_381_g2
        ~stack_type:(bls12_381_g2 @$ bot)
        ~kinstr:(INeg_bls12_381_g2 (dummy_loc, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~check
        ~name:Interpreter_workload.N_INeg_bls12_381_fr
        ~stack_type:(bls12_381_fr @$ bot)
        ~kinstr:(INeg_bls12_381_fr (dummy_loc, halt))
        ()

    let () =
      let (Ty_ex_c p) = pair bls12_381_g1 bls12_381_g2 in
      simple_benchmark
        ~benchmark_type:Time
        ~check
        ~name:Interpreter_workload.N_IPairing_check_bls12_381
        ~intercept_stack:(Script_list.empty, eos)
        ~stack_type:(list p @$ bot)
        ~kinstr:(IPairing_check_bls12_381 (dummy_loc, halt))
        () ;
      (* no intercept for alloc *)
      simple_benchmark
        ~benchmark_type:Alloc
        ~check
        ~name:Interpreter_workload.N_IPairing_check_bls12_381
        ~stack_type:(list p @$ bot)
        ~kinstr:(IPairing_check_bls12_381 (dummy_loc, halt))
        () ;
      register_time_alloc_codegen_model (Instr_name N_IPairing_check_bls12_381)
  end

  module Tickets = struct
    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_ITicket
        ~stack_type:(unit @$ nat @$ bot)
        ~kinstr:(ITicket (dummy_loc, Some unit, halt))
        ()

    let () =
      simple_time_alloc_benchmark
        ~name:Interpreter_workload.N_IRead_ticket
        ~stack_type:(ticket unit @$ bot)
        ~kinstr:(IRead_ticket (dummy_loc, Some unit, halt))
        ()

    let split_ticket_instr = ISplit_ticket (dummy_loc, halt)

    let stack_type = ticket unit @$ cpair nat nat @$ bot

    let () =
      let one = Script_int.one_n in
      let ticket =
        {
          ticketer =
            Alpha_context.Contract.Implicit
              Environment.Signature.Public_key_hash.zero;
          contents = ();
          amount = Ticket_amount.(add one one);
        }
      in
      benchmark_with_fixed_stack
        ~intercept:true
        ~name:Interpreter_workload.N_ISplit_ticket
        ~stack_type
        ~stack:(ticket, ((one, one), eos))
        ~kinstr:split_ticket_instr
        ()

    let () =
      time_and_alloc (fun benchmark_type ->
          benchmark
            ~benchmark_type
            ~name:Interpreter_workload.N_ISplit_ticket
            ~kinstr_and_stack_sampler:(fun config rng_state ->
              let _, (module Samplers) =
                make_default_samplers config.Default_config.sampler
              in
              fun () ->
                let x_amount =
                  Script_int.succ_n @@ Samplers.Random_value.value nat rng_state
                in
                let y_amount =
                  Script_int.succ_n @@ Samplers.Random_value.value nat rng_state
                in
                let amount = Script_int.add_n x_amount y_amount in
                let amount =
                  (* this is safe because x_amount > 0 and y_amount > 0 *)
                  WithExceptions.Option.get ~loc:__LOC__
                  @@ Ticket_amount.of_n amount
                in
                let ticket =
                  Samplers.Random_value.value (ticket unit) rng_state
                in
                let ticket = {ticket with amount} in
                Ex_stack_and_kinstr
                  {
                    stack = (ticket, ((x_amount, y_amount), eos));
                    stack_type;
                    kinstr = split_ticket_instr;
                  })
            ()) ;
      register_time_alloc_codegen_model (Instr_name N_ISplit_ticket)

    let join_tickets_instr = IJoin_tickets (dummy_loc, string, halt)

    let ticket_str = ticket string

    let stack_type =
      let (Ty_ex_c p) = pair ticket_str ticket_str in
      p @$ bot

    let () =
      benchmark
        ~intercept:true
        ~name:Interpreter_workload.N_IJoin_tickets
        ~kinstr_and_stack_sampler:(fun config rng_state ->
          let _, (module Samplers) =
            make_default_samplers config.Default_config.sampler
          in
          fun () ->
            let ticket =
              Samplers.Random_value.value (ticket string) rng_state
            in
            let ticket =
              {
                ticket with
                contents = Script_string.empty;
                amount = Ticket_amount.one;
              }
            in
            Ex_stack_and_kinstr
              {
                stack = ((ticket, ticket), eos);
                stack_type;
                kinstr = join_tickets_instr;
              })
        ()

    let () =
      time_and_alloc (fun benchmark_type ->
          benchmark
            ~benchmark_type
            ~name:Interpreter_workload.N_IJoin_tickets
            ~kinstr_and_stack_sampler:(fun config rng_state ->
              let _, (module Samplers) =
                make_default_samplers config.Default_config.sampler
              in
              fun () ->
                let ticket =
                  Samplers.Random_value.value (ticket string) rng_state
                in
                let alt_amount =
                  let amount = Samplers.Random_value.value nat rng_state in
                  let open Ticket_amount in
                  match of_n amount with
                  | Some amount -> add amount one
                  | None -> one
                in
                let ticket' = {ticket with amount = alt_amount} in
                Ex_stack_and_kinstr
                  {
                    stack = ((ticket, ticket'), eos);
                    stack_type;
                    kinstr = join_tickets_instr;
                  })
            ()) ;
      register_time_alloc_codegen_model (Instr_name N_IJoin_tickets)
  end

  module Timelock = struct
    let name = Interpreter_workload.N_IOpen_chest

    let stack_type =
      Michelson_types.chest_key @$ Michelson_types.chest @$ nat @$ bot

    let kinstr = IOpen_chest (dummy_loc, halt)

    let resulting_stack chest chest_key time =
      let chest = Script_timelock.make_chest chest in
      let chest_key = Script_timelock.make_chest_key chest_key in
      ( chest_key,
        ( chest,
          ( Script_int.is_nat (Script_int.of_int time)
            |> WithExceptions.Option.get ~loc:"Timelock:gas benchmarks",
            eos ) ) )

    let () =
      benchmark_with_stack_sampler
        ~intercept:true
        ~name
        ~kinstr
        ~stack_type
        ~stack_sampler:(fun _ rng_state () ->
          let time = 1 in
          let chest, chest_key =
            Timelock_samplers.chest_sampler ~plaintext_size:1 ~time ~rng_state
          in
          resulting_stack chest chest_key time)
        ()

    let () =
      time_and_alloc @@ fun benchmark_type ->
      benchmark_with_stack_sampler
        ~benchmark_type
        ~name
        ~kinstr
        ~stack_type
        ~stack_sampler:(fun _ rng_state () ->
          let log_time =
            (* Chest generation takes too long time for [log_time > 20] *)
            Base_samplers.sample_in_interval
              ~range:{min = 0; max = 20}
              rng_state
          in
          let time = Int.shift_left 1 log_time in
          let plaintext_size =
            Base_samplers.sample_in_interval
              ~range:{min = 1; max = 10000}
              rng_state
          in

          let chest, chest_key =
            Timelock_samplers.chest_sampler ~plaintext_size ~time ~rng_state
          in
          resulting_stack chest chest_key time)
        ()

    let () = register_time_alloc_codegen_model (Instr_name name)
  end

  module Continuations = struct
    let () =
      (*
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KNil
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KNil in
          let stack = eos in
          let stack_type = bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      (*
        KCons -> step
        KHalt -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KCons
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KCons (halt, KNil) in
          let stack = ((), eos) in
          let stack_type = unit @$ bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      (*
        KReturn -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KReturn
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KReturn (eos, Some (unit @$ bot), KNil) in
          let stack = ((), eos) in
          let stack_type = unit @$ bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      (*
        KView_exit -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KView_exit
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let open Script_typed_ir in
          let open Alpha_context in
          let step_constants =
            {
              sender = Contract (Implicit Signature.Public_key_hash.zero);
              payer = Signature.Public_key_hash.zero;
              self = Contract_hash.zero;
              amount = Tez.zero;
              balance = Tez.zero;
              chain_id = Chain_id.zero;
              now = Script_timestamp.of_zint Z.zero;
              level = Script_int.zero_n;
            }
          in
          let cont = KView_exit (step_constants, KNil) in
          let stack = ((), eos) in
          let stack_type = unit @$ bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      (*
        KLoop_in -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KLoop_in
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KLoop_in (IPush (dummy_loc, bool, false, halt), KNil) in
          let stack = (false, ((), eos)) in
          let stack_type = bool @$ unit @$ bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      (*
        KLoop_in_left -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KLoop_in_left
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont =
            KLoop_in_left (ICons_right (dummy_loc, unit, halt), KNil)
          in
          let stack = (R (), eos) in
          let stack_type = cor unit unit @$ bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      (*
        KUndip -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KUndip
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KUndip ((), Some unit, KNil) in
          let stack = eos in
          let stack_type = bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      (*
        KIter (empty case) -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KIter_empty
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KIter (IDrop (dummy_loc, halt), Some unit, [], KNil) in
          let stack = ((), eos) in
          let stack_type = unit @$ bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      (*
        KIter (nonempty case) -> step
        KDrop -> step
        KHalt -> next
        KIter (empty case) -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KIter_nonempty
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KIter (IDrop (dummy_loc, halt), Some unit, [()], KNil) in
          let stack = ((), eos) in
          let stack_type = unit @$ bot in
          fun () -> Ex_stack_and_cont {stack; cont; stack_type})
        ()

    let () =
      let model =
        Interpreter_model.Models.max_branching_model
          ~case_0:"empty_const"
          ~case_1:"nonempty_const"
          "N_KIter"
      in
      register_model_for_code_generation model

    let () =
      (*
        KList_enter_body ([()], bot accumulator case) -> step
        KHalt -> next
        KList_exit_body ([], []) ->
        KList_enter_body ([], [()] ->
        List.rev singleton
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KList_enter_body
        ~salt:"_singleton_list"
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let kbody = halt in
          fun () ->
            let cont =
              KList_enter_body
                (kbody, [()], Script_list.empty, Some (list unit), 1, KNil)
            in
            Ex_stack_and_cont
              {stack = ((), eos); stack_type = unit @$ bot; cont})
        ()

    let () =
      (*
        KList_enter_body (empty list, nonempty accumulator case) ->
        {List.rev n elements} -> next
        KNil
      *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KList_enter_body
        ~salt:"_terminal"
        ~cont_and_stack_sampler:(fun cfg rng_state ->
          let _, (module Samplers) = make_default_samplers cfg.sampler in
          let kbody = halt in
          fun () ->
            let ys = Samplers.Random_value.value (list unit) rng_state in
            let cont =
              KList_enter_body (kbody, [], ys, Some (list unit), ys.length, KNil)
            in
            Ex_stack_and_cont
              {stack = ((), eos); stack_type = unit @$ bot; cont})
        ()

    let () =
      (*
        KList_enter_body (empty list, bot accumulator case) ->
        {List.rev singleton} -> next
        KNil
      *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~intercept:true
        ~name:Interpreter_workload.N_KList_enter_body
        ~salt:"_terminal"
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let kbody = halt in
          fun () ->
            let cont =
              KList_enter_body
                (kbody, [], Script_list.empty, Some (list unit), 1, KNil)
            in
            Ex_stack_and_cont
              {stack = ((), eos); stack_type = unit @$ bot; cont})
        ()

    let () =
      (*
        KList_exit_body (empty list) -> next
        KList_enter_body ->
        {List.rev 1 element} -> next
        KNil
      *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~intercept:true
        ~name:Interpreter_workload.N_KList_exit_body
        ~salt:"_terminal"
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let kbody = halt in
          let cont =
            KList_exit_body
              (kbody, [], Script_list.empty, Some (list unit), 1, KNil)
          in
          fun () ->
            Ex_stack_and_cont
              {stack = ((), ((), eos)); stack_type = unit @$ unit @$ bot; cont})
        ()

    let stack_type = cpair int unit @$ unit @$ bot

    let map_enter_body_code =
      let kbody = ICdr (dummy_loc, halt) in
      fun accu ->
        KMap_enter_body
          (kbody, accu, Script_map.empty int, Some (map int unit), KNil)

    let () =
      (*
        KMap_enter_body (empty case) -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KMap_enter_body
        ~salt:"_empty"
        ~cont_and_stack_sampler:(fun _cfg _rng_state () ->
          Ex_stack_and_cont
            {
              stack = ((), eos);
              stack_type = unit @$ bot;
              cont = map_enter_body_code [];
            })
        ()

    let () =
      (*
        KMap_enter_body (singleton case) -> step
        KCdr -> step
        KHalt -> next
        KMap_exit_body -> next
        (map_update)
        KMap_enter_body (empty case) -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KMap_enter_body
        ~salt:"_singleton"
        ~cont_and_stack_sampler:(fun _cfg _rng_state () ->
          Ex_stack_and_cont
            {
              stack = ((), eos);
              stack_type = unit @$ bot;
              cont = map_enter_body_code [(Script_int.zero, ())];
            })
        ()

    let () =
      (*
        KMap_exit_body ->
        (map_update) -> next
        KMap_enter_body (empty case) -> next
        KNil
       *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KMap_exit_body
        ~cont_and_stack_sampler:(fun cfg rng_state ->
          let kbody = ICdr (dummy_loc, halt) in
          fun () ->
            let ty = map int unit in
            let key, map = Maps.generate_map_and_key_not_in_map cfg rng_state in
            let cont = KMap_exit_body (kbody, [], map, key, Some ty, KNil) in
            Ex_stack_and_cont
              {stack = ((), ((), eos)); stack_type = unit @$ unit @$ bot; cont})
        ()

    let () =
      (* KMap_head -> KNil *)
      continuation_time_alloc_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KMap_head
        ~cont_and_stack_sampler:(fun _cfg _rng_state () ->
          let cont = KMap_head (Option.some, KNil) in
          Ex_stack_and_cont
            {stack = ((), ((), eos)); stack_type = unit @$ unit @$ bot; cont})
        ()
  end

  let () =
    simple_time_alloc_benchmark
      ~name:Interpreter_workload.N_IEmit
      ~stack_type:(unit_t @$ bot)
      ~kinstr:
        (IEmit
           {
             ty = unit_t;
             k = halt;
             loc = dummy_loc;
             tag = Entrypoint_repr.default;
             unparsed_ty = Script_repr.unit;
           })
      ()
end
