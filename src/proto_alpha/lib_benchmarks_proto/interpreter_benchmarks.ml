(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

module Timelock_samplers = Timelock
open Protocol

(* ------------------------------------------------------------------------- *)

type ex_stack_and_kinstr =
  | Ex_stack_and_kinstr : {
      stack : 'a * 'b;
      kinstr : ('a, 'b, 'c, 'd) Script_typed_ir.kinstr;
    }
      -> ex_stack_and_kinstr

type ex_stack_and_continuation =
  | Ex_stack_and_cont : {
      stack : 'a * 'b;
      cont : ('a, 'b, 'c, 'd) Script_typed_ir.continuation;
    }
      -> ex_stack_and_continuation

type ex_value =
  | Ex_value : {value : 'a; ty : 'a Script_typed_ir.ty} -> ex_value

(* ------------------------------------------------------------------------- *)

let sf = Printf.sprintf

(* End of Stack *)
let eos = Script_typed_ir.(EmptyCell, EmptyCell)

let info_and_name ~intercept ?(salt = "") s =
  let s = s ^ salt in
  if intercept then (sf "Benchmark %s (intercept case)" s, s ^ "_intercept")
  else (sf "Benchmark %s" s, s)

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

let benchmark_from_kinstr_and_stack :
    ?amplification:int ->
    Alpha_context.context ->
    Protocol.Script_interpreter.step_constants ->
    ex_stack_and_kinstr ->
    Interpreter_workload.ir_sized_step list Generator.benchmark =
 fun ?amplification ctxt step_constants stack_kinstr ->
  let ctxt = Gas_helpers.set_limit ctxt in
  match stack_kinstr with
  | Ex_stack_and_kinstr {stack = (bef_top, bef); kinstr} ->
      let (workload, closure) =
        match amplification with
        | None ->
            let workload =
              Interpreter_workload.extract_deps
                ctxt
                step_constants
                kinstr
                (bef_top, bef)
            in
            let (_gas_counter, outdated_ctxt) =
              Local_gas_counter.local_gas_counter_and_outdated_context ctxt
            in
            let closure () =
              ignore
                (* Lwt_main.run *)
                (Script_interpreter.Internals.step
                   (outdated_ctxt, step_constants)
                   (Local_gas_counter 9_999_999_999)
                   kinstr
                   bef_top
                   bef)
            in
            (workload, closure)
        | Some amplification_factor ->
            assert (amplification_factor > 0) ;
            let workload =
              Interpreter_workload.extract_deps
                ctxt
                step_constants
                kinstr
                (bef_top, bef)
            in
            let workload =
              List.repeat amplification_factor workload |> List.flatten
            in
            let (_gas_counter, outdated_ctxt) =
              Local_gas_counter.local_gas_counter_and_outdated_context ctxt
            in
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
            (workload, closure)
      in
      Generator.Plain {workload; closure}

let make_benchmark :
    ?amplification:int ->
    ?intercept:bool ->
    ?salt:string ->
    ?more_tags:string list ->
    name:Interpreter_workload.instruction_name ->
    kinstr_and_stack_sampler:
      (Default_config.config -> Random.State.t -> unit -> ex_stack_and_kinstr) ->
    unit ->
    Benchmark.t =
 fun ?amplification
     ?(intercept = false)
     ?salt
     ?(more_tags = [])
     ~name
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
        ?amplification
        (if intercept then None else Some (Instr_name name))

    let (info, name) =
      info_and_name
        ~intercept
        ?salt
        (Interpreter_workload.string_of_instruction_name name)

    let benchmark kinstr_and_stack_sampler ctxt step_constants () =
      let stack_instr = kinstr_and_stack_sampler () in
      benchmark_from_kinstr_and_stack
        ?amplification
        ctxt
        step_constants
        stack_instr

    let create_benchmarks ~rng_state ~bench_num (config : config) =
      match Lwt_main.run (Execution_context.make ~rng_state) with
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
    name:Interpreter_workload.instruction_name ->
    kinstr:(bef_top, bef, res_top, res) Script_typed_ir.kinstr ->
    unit ->
    Benchmark.t =
 fun ?amplification ?intercept ?more_tags ?salt ~name ~kinstr () ->
  let kinfo = Script_typed_ir.kinfo_of_kinstr kinstr in
  let stack_ty = kinfo.kstack_ty in
  let kinstr_and_stack_sampler config rng_state =
    let (_, (module Samplers)) =
      make_default_samplers config.Default_config.sampler
    in
    fun () ->
      Ex_stack_and_kinstr
        {stack = Samplers.Random_value.stack stack_ty rng_state; kinstr}
  in
  make_benchmark
    ?amplification
    ?intercept
    ?more_tags
    ?salt
    ~name
    ~kinstr_and_stack_sampler
    ()

let benchmark ?amplification ?intercept ?more_tags ?salt ~name
    ~kinstr_and_stack_sampler () =
  let bench =
    make_benchmark
      ?amplification
      ?intercept
      ?more_tags
      ?salt
      ~name
      ~kinstr_and_stack_sampler
      ()
  in
  Registration_helpers.register bench

let benchmark_with_stack_sampler ?amplification ?intercept ?more_tags ?salt
    ~name ~kinstr ~stack_sampler () =
  let kinstr_and_stack_sampler config rng_state =
    let stack_sampler = stack_sampler config rng_state in
    fun () -> Ex_stack_and_kinstr {stack = stack_sampler (); kinstr}
  in
  let bench =
    make_benchmark
      ?amplification
      ?intercept
      ?more_tags
      ?salt
      ~name
      ~kinstr_and_stack_sampler
      ()
  in
  Registration_helpers.register bench

let benchmark_with_fixed_stack ?amplification ?intercept ?more_tags ?salt ~name
    ~stack ~kinstr () =
  benchmark_with_stack_sampler
    ?amplification
    ?intercept
    ?more_tags
    ?salt
    ~name
    ~kinstr
    ~stack_sampler:(fun _cfg _rng_state () -> stack)
    ()

let simple_benchmark_with_stack_sampler ?amplification ?intercept_stack ?salt
    ?more_tags ~name ~kinstr ~stack_sampler () =
  benchmark_with_stack_sampler
    ?amplification
    ~intercept:false
    ?salt
    ?more_tags
    ~name
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
        ~name
        ~stack
        ~kinstr
        ())
    intercept_stack

let simple_benchmark ?amplification ?intercept_stack ?more_tags ?salt ~name
    ~kinstr () =
  let bench =
    make_simple_benchmark
      ?amplification
      ~intercept:false
      ?more_tags
      ?salt
      ~name
      ~kinstr
      ()
  in
  Registration_helpers.register bench ;
  Option.iter
    (fun stack ->
      benchmark_with_fixed_stack
        ?amplification
        ~intercept:true
        ?more_tags
        ?salt
        ~name
        ~stack
        ~kinstr
        ())
    intercept_stack

(* ------------------------------------------------------------------------- *)
(* Helpers for creating benchmarks for [Script_interpreter.next] *)

let benchmark_from_continuation :
    ?amplification:int ->
    Alpha_context.context ->
    Protocol.Script_interpreter.step_constants ->
    ex_stack_and_continuation ->
    Interpreter_workload.ir_sized_step list Generator.benchmark =
 fun ?amplification ctxt step_constants stack_cont ->
  let ctxt = Gas_helpers.set_limit ctxt in
  match stack_cont with
  | Ex_stack_and_cont {stack = (bef_top, bef); cont} ->
      let (workload, closure) =
        match amplification with
        | None ->
            let workload =
              Interpreter_workload.extract_deps_continuation
                ctxt
                step_constants
                cont
                (bef_top, bef)
            in
            let (_gas_counter, outdated_ctxt) =
              Local_gas_counter.local_gas_counter_and_outdated_context ctxt
            in
            let closure () =
              ignore
                (* Lwt_main.run *)
                (Script_interpreter.Internals.next
                   None
                   (outdated_ctxt, step_constants)
                   (Local_gas_counter 9_999_999_999)
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
                cont
                (bef_top, bef)
            in
            let workload =
              List.repeat amplification_factor workload |> List.flatten
            in
            let (_gas_counter, outdated_ctxt) =
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
                     cont
                     bef_top
                     bef)
              done
            in
            (workload, closure)
      in
      Generator.Plain {workload; closure}

let make_continuation_benchmark :
    ?amplification:int ->
    ?intercept:bool ->
    ?salt:string ->
    ?more_tags:string list ->
    name:Interpreter_workload.continuation_name ->
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
     ~name
     ~cont_and_stack_sampler
     () ->
  let module B : Benchmark.S = struct
    include Default_config
    include Default_boilerplate

    let tags = tags @ more_tags

    let models =
      Interpreter_model.make_model
        ?amplification
        (if intercept then None else Some (Cont_name name))

    let (info, name) =
      info_and_name
        ~intercept
        ?salt
        (Interpreter_workload.string_of_continuation_name name)

    let benchmark cont_and_stack_sampler ctxt step_constants () =
      let stack_instr = cont_and_stack_sampler () in
      benchmark_from_continuation ?amplification ctxt step_constants stack_instr

    let create_benchmarks ~rng_state ~bench_num (config : config) =
      match Lwt_main.run (Execution_context.make ~rng_state) with
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

let continuation_benchmark ?amplification ?intercept ?salt ?more_tags ~name
    ~cont_and_stack_sampler () =
  let bench =
    make_continuation_benchmark
      ?amplification
      ?intercept
      ?salt
      ?more_tags
      ~name
      ~cont_and_stack_sampler
      ()
  in
  Registration_helpers.register bench

(* ------------------------------------------------------------------------- *)
(* Sampling helpers *)

let nat_of_positive_int (i : int) =
  let open Alpha_context.Script_int in
  match is_nat (of_int i) with None -> assert false | Some x -> x

let adversarial_ints rng_state (cfg : Default_config.config) n =
  let (_common_prefix, ls) =
    Base_samplers.Adversarial.integers
      ~prefix_size:cfg.sampler.base_parameters.int_size
      ~card:n
      rng_state
  in
  List.map Script_int_repr.of_zint ls

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

  let kinfo kstack_ty = {iloc = 0; kstack_ty}

  let halt stack_ty = IHalt (kinfo stack_ty)

  let halt_unit = halt (unit @$ bot)

  let halt_unitunit = halt (unit @$ unit @$ bot)

  let kinfo_unit = kinfo (unit @$ bot)

  let kinfo_unitunit = kinfo (unit @$ unit @$ bot)

  let () =
    (* KHalt *)
    simple_benchmark
      ~amplification:100
      ~name:Interpreter_workload.N_IHalt
      ~kinstr:halt_unit
      ()

  module Amplification = struct
    module Loop : Benchmark.S = struct
      let name = "amplification_loop"

      let info = "Benchmarking the cost of an empty loop"

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
      simple_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_IDrop
        ~kinstr:(IDrop (kinfo_unitunit, halt_unit))
        ()

    let () =
      (* IDup ; IHalt *)
      simple_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_IDup
        ~kinstr:(IDup (kinfo_unit, halt_unitunit))
        ()

    let () =
      simple_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_ISwap
        ~kinstr:(ISwap (kinfo_unitunit, halt_unitunit))
        ()

    let () =
      simple_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_IConst
        ~kinstr:(IConst (kinfo_unit, (), halt_unitunit))
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
      match stack with
      | Ex_stack (stack_ty, stack) ->
          raise_if_error
            (Lwt_main.run
               ( Execution_context.make ~rng_state
               >>=? fun (ctxt, _step_constants) ->
                 Script_ir_translator.parse_instr
                   Script_tc_context.data
                   ctxt
                   ~legacy:false
                   node
                   stack_ty
                 >|= Environment.wrap_tzresult
                 >>=? fun (judgement, _) ->
                 match judgement with
                 | Script_ir_translator.Typed descr ->
                     let kinfo = {iloc = 0; kstack_ty = descr.bef} in
                     let kinfo' = {iloc = 0; kstack_ty = descr.aft} in
                     let kinstr = descr.instr.apply kinfo (IHalt kinfo') in
                     return (Ex_stack_and_kinstr {stack; kinstr})
                 | Script_ir_translator.Failed _ -> assert false ))

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
      benchmark
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
      benchmark
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
      benchmark
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
      benchmark
        ~name:Interpreter_workload.N_IDropN
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = drop (sample_depth rng_state) in
          parse_instr rng_state node long_stack)
        ()

    let () =
      let pair n = Micheline.(Prim (0, I_PAIR, [Int (0, Z.of_int n)], [])) in
      benchmark
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
        ()

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
            make_comb
              (comb_width - 1)
              (Ex_value {value = ((), value); ty = pair unit ty})

    let () =
      let unpair n =
        Micheline.(Prim (0, I_UNPAIR, [Int (0, Z.of_int n)], []))
      in
      benchmark
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
        ()

    let () =
      let comb_get n = Micheline.(Prim (0, I_GET, [Int (0, Z.of_int n)], [])) in
      benchmark
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
        ()

    let () =
      let comb_set n =
        Micheline.(Prim (0, I_UPDATE, [Int (0, Z.of_int n)], []))
      in
      benchmark
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
        ()

    let () =
      let dup n = Micheline.(Prim (0, I_DUP, [Int (0, Z.of_int n)], [])) in
      benchmark
        ~name:Interpreter_workload.N_IDupN
        ~kinstr_and_stack_sampler:(fun _cfg rng_state () ->
          let node = dup (1 + sample_depth rng_state) in
          parse_instr rng_state node long_stack)
        ()
  end

  module Pairs = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ICons_pair
        ~kinstr:(ICons_pair (kinfo_unitunit, halt (pair unit unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ICar
        ~kinstr:(ICar (kinfo (pair unit unit @$ bot), halt_unit))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ICdr
        ~kinstr:(ICdr (kinfo (pair unit unit @$ bot), halt_unit))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IUnpair
        ~kinstr:(IUnpair (kinfo (pair unit unit @$ bot), halt_unitunit))
        ()
  end

  module Options = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ICons_some
        ~kinstr:(ICons_some (kinfo_unit, halt (option unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ICons_none
        ~kinstr:(ICons_none (kinfo_unit, halt (option unit @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IIf_none
        ~kinstr:
          (IIf_none
             {
               kinfo = kinfo (option unit @$ unit @$ bot);
               branch_if_none = halt_unit;
               branch_if_some = IDrop (kinfo_unitunit, halt_unit);
               k = halt_unit;
             })
        ()

    let () =
      benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_IOpt_map
        ~salt:"none"
        ~stack:(None, ((), eos))
        ~kinstr:
          (IOpt_map
             {
               kinfo = kinfo (option unit @$ unit @$ bot);
               body = halt_unitunit;
               k = halt (option unit @$ unit @$ bot);
             })
        ()

    let () =
      benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_IOpt_map
        ~salt:"some"
        ~stack:(Some (), ((), eos))
        ~kinstr:
          (IOpt_map
             {
               kinfo = kinfo (option unit @$ unit @$ bot);
               body = halt_unitunit;
               k = halt (option unit @$ unit @$ bot);
             })
        ()
  end

  module Unions = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ILeft
        ~kinstr:(ICons_left (kinfo_unit, halt (union unit unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IRight
        ~kinstr:(ICons_right (kinfo_unit, halt (union unit unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IIf_left
        ~kinstr:
          (IIf_left
             {
               kinfo = kinfo (union unit unit @$ bot);
               branch_if_left = halt_unit;
               branch_if_right = halt_unit;
               k = halt_unit;
             })
        ()
  end

  module Lists = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ICons_list
        ~kinstr:
          (ICons_list (kinfo (unit @$ list unit @$ bot), halt (list unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INil
        ~kinstr:(INil (kinfo_unit, halt (list unit @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IIf_cons
        ~kinstr:
          (IIf_cons
             {
               kinfo = kinfo (list unit @$ unit @$ bot);
               branch_if_cons =
                 IDrop
                   ( kinfo (unit @$ list unit @$ unit @$ bot),
                     IDrop (kinfo (list unit @$ unit @$ bot), halt_unit) );
               branch_if_nil = halt_unit;
               k = halt_unit;
             })
        ()

    module Mapping = struct
      let kinfo_enter_body = kinfo_unit

      let kinfo_exit_body = kinfo_unitunit

      let () =
        (*
          IList_map ->
          IList_enter_body (empty case) ->
          IHalt
         *)
        benchmark_with_fixed_stack
          ~name:Interpreter_workload.N_IList_map
          ~stack:(Script_list.empty, ((), eos))
          ~kinstr:
            (IList_map
               ( kinfo (list unit @$ unit @$ bot),
                 halt_unitunit,
                 halt (list unit @$ unit @$ bot) ))
          ()
    end

    let () =
      let kinfo = kinfo (list unit @$ bot) in
      simple_benchmark
        ~name:Interpreter_workload.N_IList_size
        ~kinstr:(IList_size (kinfo, halt (nat @$ bot)))
        ()

    let () =
      (*
        IList_iter ->
        IIter (empty case) ->
        IHalt
       *)
      let kinfo1 = kinfo (list unit @$ unit @$ bot) in
      benchmark_with_fixed_stack
        ~name:Interpreter_workload.N_IList_iter
        ~stack:(Script_list.empty, ((), eos))
        ~kinstr:
          (IList_iter (kinfo1, IDrop (kinfo_unitunit, halt_unit), halt_unit))
        ()
  end

  module Sets = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IEmpty_set
        ~kinstr:
          (IEmpty_set (kinfo_unit, unit_cmp, halt (set unit_cmp @$ unit @$ bot)))
        ()

    let set_iter_code =
      ISet_iter
        ( kinfo (set int_cmp @$ unit @$ bot),
          IDrop (kinfo (int @$ unit @$ bot), halt_unit),
          halt_unit )

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
        ~intercept_stack:(Script_set.empty int_cmp, ((), eos))
        ~kinstr:set_iter_code
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISet_mem
        ~kinstr:
          (ISet_mem
             ( kinfo (int @$ set int_cmp @$ unit @$ bot),
               halt (bool @$ unit @$ bot) ))
        ~intercept_stack:
          (Alpha_context.Script_int.zero, (Script_set.empty int_cmp, ((), eos)))
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
              (Script_set.empty int_cmp)
              elts
          in
          let elt =
            List.nth_opt elts (Random.State.int rng_state n)
            |> WithExceptions.Option.get ~loc:__LOC__
          in
          (elt, (set, ((), eos))))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISet_update
        ~kinstr:
          (ISet_update
             ( kinfo (int @$ bool @$ set int_cmp @$ bot),
               halt (set int_cmp @$ bot) ))
        ~intercept_stack:
          ( Alpha_context.Script_int.zero,
            (false, (Script_set.empty int_cmp, eos)) )
        ~stack_sampler:(fun cfg rng_state () ->
          assert (cfg.sampler.set_size.min >= 2) ;
          let n =
            Base_samplers.sample_in_interval
              rng_state
              ~range:cfg.sampler.set_size
          in
          let elts = adversarial_ints rng_state cfg (n + 1) in
          let (out_of_set, in_set) =
            match elts with [] -> assert false | hd :: tl -> (hd, tl)
          in
          let set =
            List.fold_left
              (fun set elt -> Script_set.update elt true set)
              (Script_set.empty int_cmp)
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
      simple_benchmark
        ~name:Interpreter_workload.N_ISet_size
        ~kinstr:(ISet_size (kinfo (set unit_cmp @$ bot), halt (nat @$ bot)))
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
          (Script_map.empty int_cmp)
          keys
      in
      let (module M) = Script_map.get_module map in
      let key =
        M.OPS.fold (fun k _ -> function None -> Some k | x -> x) M.boxed None
        |> WithExceptions.Option.get ~loc:__LOC__
      in
      (key, map)

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IEmpty_map
        ~kinstr:
          (IEmpty_map
             (kinfo_unit, unit_cmp, halt (map unit_cmp unit @$ unit @$ bot)))
        ()

    (*
    let map_map_code =
      IMap_map
        ( kinfo (map int_cmp unit @$ unit @$ bot),
          ICdr (kinfo (pair int unit @$ unit @$ bot), halt_unitunit),
          halt (map int_cmp unit @$ unit @$ bot) )
     *)

    let map_map_code =
      IMap_map
        ( kinfo (map int_cmp unit @$ unit @$ bot),
          IFailwith (kinfo (pair int unit @$ unit @$ bot), 0, pair int unit),
          halt (map int_cmp unit @$ unit @$ bot) )

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
          (let map = Script_map.empty int_cmp in
           (map, ((), eos)))
        ~kinstr:map_map_code
        ()

    let kmap_iter_code =
      IMap_iter
        ( kinfo (map int_cmp unit @$ unit @$ bot),
          IDrop (kinfo (pair int unit @$ unit @$ bot), halt_unit),
          halt_unit )

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
          (let map = Script_map.empty int_cmp in
           (map, ((), eos)))
        ~kinstr:kmap_iter_code
        ()

    let () =
      (*
        IMap_mem ->
        (map_mem) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_mem
        ~kinstr:
          (IMap_mem
             ( kinfo (int @$ map int_cmp unit @$ unit @$ bot),
               halt (bool @$ unit @$ bot) ))
        ~intercept_stack:
          (let map = Script_map.empty int_cmp in
           (Alpha_context.Script_int.zero, (map, ((), eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let (key, map) = generate_map_and_key_in_map cfg rng_state in
          (key, (map, ((), eos))))
        ()

    let () =
      (*
        IMap_get ->
        (map_get) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_get
        ~kinstr:
          (IMap_get
             ( kinfo (int @$ map int_cmp unit @$ unit @$ bot),
               halt (option unit @$ unit @$ bot) ))
        ~intercept_stack:
          (let map = Script_map.empty int_cmp in
           (Alpha_context.Script_int.zero, (map, ((), eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let (key, map) = generate_map_and_key_in_map cfg rng_state in
          (key, (map, ((), eos))))
        ()

    let () =
      (*
        IMap_update ->
        (map_update) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_update
        ~kinstr:
          (IMap_update
             ( kinfo (int @$ option unit @$ map int_cmp unit @$ bot),
               halt (map int_cmp unit @$ bot) ))
        ~intercept_stack:
          (let map = Script_map.empty int_cmp in
           (Alpha_context.Script_int.zero, (None, (map, eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let (key, map) = generate_map_and_key_in_map cfg rng_state in
          (key, (Some (), (map, eos))))
        ()

    let () =
      (*
        IMap_get_and_update ->
        (map_update) ->
        (map_get) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_get_and_update
        ~kinstr:
          (IMap_get_and_update
             ( kinfo (int @$ option unit @$ map int_cmp unit @$ bot),
               halt (option unit @$ map int_cmp unit @$ bot) ))
        ~intercept_stack:
          (let map = Script_map.empty int_cmp in
           (Alpha_context.Script_int.zero, (None, (map, eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let (key, map) = generate_map_and_key_in_map cfg rng_state in
          (key, (Some (), (map, eos))))
        ()

    let () =
      (*
        IMap_size ->
        (map_update) ->
        (map_get) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMap_size
        ~kinstr:(IMap_size (kinfo (map int_cmp unit @$ bot), halt (nat @$ bot)))
        ~stack_sampler:(fun _cfg _rng_state ->
          let map = Script_map.empty int_cmp in
          fun () -> (map, eos))
        ()
  end

  module Big_maps = struct
    let generate_big_map_and_key_in_map (cfg : Default_config.config) rng_state
        =
      let n =
        Base_samplers.sample_in_interval rng_state ~range:cfg.sampler.set_size
      in
      let keys = adversarial_ints rng_state cfg n in
      let map =
        List.fold_left
          (fun map i -> Script_map.update i (Some (Some ())) map)
          (Script_map.empty int_cmp)
          keys
      in
      let (module M) = Script_map.get_module map in
      let key =
        M.OPS.fold (fun k _ -> function None -> Some k | x -> x) M.boxed None
        |> WithExceptions.Option.get ~loc:__LOC__
      in
      let big_map =
        raise_if_error
          (Lwt_main.run
             ( Execution_context.make ~rng_state >>=? fun (ctxt, _) ->
               let big_map =
                 Script_ir_translator.empty_big_map int_cmp unit_t
               in
               Script_map.fold
                 (fun k v acc ->
                   acc >>=? fun (bm, ctxt_acc) ->
                   Script_ir_translator.big_map_update ctxt_acc k v bm)
                 map
                 (return (big_map, ctxt))
               >|= Environment.wrap_tzresult
               >>=? fun (big_map, _) -> return big_map ))
      in
      (key, big_map)

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IEmpty_big_map
        ~kinstr:
          (IEmpty_big_map
             ( kinfo_unit,
               unit_cmp,
               unit,
               halt (big_map unit_cmp unit @$ unit @$ bot) ))
        ()

    let () =
      (*
        IBig_map_mem ->
        (update context with gas)
        (big_map_mem) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBig_map_mem
        ~kinstr:
          (IBig_map_mem
             ( kinfo (int @$ big_map int_cmp unit @$ unit @$ bot),
               halt (bool @$ unit @$ bot) ))
        ~stack_sampler:(fun cfg rng_state () ->
          let (key, map) = generate_big_map_and_key_in_map cfg rng_state in
          (key, (map, ((), eos))))
        ()

    let () =
      (*
        IBig_map_get ->
        (big_map_get) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBig_map_get
        ~kinstr:
          (IBig_map_get
             ( kinfo (int @$ big_map int_cmp unit @$ unit @$ bot),
               halt (option unit @$ unit @$ bot) ))
        ~intercept_stack:
          (let map = Script_ir_translator.empty_big_map int_cmp unit in
           (Alpha_context.Script_int.zero, (map, ((), eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let (key, map) = generate_big_map_and_key_in_map cfg rng_state in
          (key, (map, ((), eos))))
        ()

    let () =
      (*
        IBig_map_update ->
        (big_map_update) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBig_map_update
        ~kinstr:
          (IBig_map_update
             ( kinfo (int @$ option unit @$ big_map int_cmp unit @$ bot),
               halt (big_map int_cmp unit @$ bot) ))
        ~intercept_stack:
          (let map = Script_ir_translator.empty_big_map int_cmp unit in
           (Alpha_context.Script_int.zero, (None, (map, eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let (key, map) = generate_big_map_and_key_in_map cfg rng_state in
          (key, (Some (), (map, eos))))
        ()

    let () =
      (*
        IBig_map_get_and_update ->
        (big_map_update) ->
        (big_map_get) ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IBig_map_get_and_update
        ~kinstr:
          (IBig_map_get_and_update
             ( kinfo (int @$ option unit @$ big_map int_cmp unit @$ bot),
               halt (option unit @$ big_map int_cmp unit @$ bot) ))
        ~intercept_stack:
          (let map = Script_ir_translator.empty_big_map int_cmp unit in
           (Alpha_context.Script_int.zero, (None, (map, eos))))
        ~stack_sampler:(fun cfg rng_state () ->
          let (key, map) = generate_big_map_and_key_in_map cfg rng_state in
          (key, (Some (), (map, eos))))
        ()
  end

  module Strings = struct
    open Alpha_context.Script_string

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IConcat_string
        ~intercept_stack:(Script_list.empty, eos)
        ~kinstr:
          (IConcat_string (kinfo (list string @$ bot), halt (string @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IConcat_string_pair
        ~intercept_stack:(empty, (empty, eos))
        ~kinstr:
          (IConcat_string_pair
             (kinfo (string @$ string @$ bot), halt (string @$ bot)))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISlice_string
        ~kinstr:
          (ISlice_string
             (kinfo (nat @$ nat @$ string @$ bot), halt (option string @$ bot)))
        ~intercept_stack:
          (let z = Alpha_context.Script_int.zero_n in
           (z, (z, (empty, eos))))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          fun () ->
            let string =
              Samplers.Random_value.value Script_typed_ir.string_t rng_state
            in
            let len = nat_of_positive_int (length string) in
            (* worst case: offset = 0 *)
            (nat_of_positive_int 0, (len, (string, eos))))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IString_size
        ~kinstr:(IString_size (kinfo (string @$ bot), halt (nat @$ bot)))
        ()
  end

  module Bytes = struct
    (* Copy of [String] modulo renaming string to bytes. *)

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IConcat_bytes
        ~intercept_stack:(Script_list.empty, eos)
        ~kinstr:(IConcat_bytes (kinfo (list bytes @$ bot), halt (bytes @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IConcat_bytes_pair
        ~intercept_stack:(Bytes.empty, (Bytes.empty, eos))
        ~kinstr:
          (IConcat_bytes_pair
             (kinfo (bytes @$ bytes @$ bot), halt (bytes @$ bot)))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISlice_bytes
        ~kinstr:
          (ISlice_bytes
             (kinfo (nat @$ nat @$ bytes @$ bot), halt (option bytes @$ bot)))
        ~intercept_stack:
          (let z = Alpha_context.Script_int.zero_n in
           (z, (z, (Bytes.empty, eos))))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          fun () ->
            let bytes =
              Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
            in
            let len = nat_of_positive_int (Bytes.length bytes) in
            (* worst case: offset = 0 *)
            (nat_of_positive_int 0, (len, (bytes, eos))))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IBytes_size
        ~kinstr:(IBytes_size (kinfo (bytes @$ bot), halt (nat @$ bot)))
        ()
  end

  module Timestamps = struct
    let zero_timestamp = Alpha_context.Script_timestamp.of_zint Z.zero

    let zero_int = Alpha_context.Script_int.zero

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAdd_seconds_to_timestamp
        ~intercept_stack:(zero_int, (zero_timestamp, eos))
        ~kinstr:
          (IAdd_seconds_to_timestamp
             (kinfo (int @$ timestamp @$ bot), halt (timestamp @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAdd_timestamp_to_seconds
        ~intercept_stack:(zero_timestamp, (zero_int, eos))
        ~kinstr:
          (IAdd_timestamp_to_seconds
             (kinfo (timestamp @$ int @$ bot), halt (timestamp @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISub_timestamp_seconds
        ~intercept_stack:(zero_timestamp, (zero_int, eos))
        ~kinstr:
          (ISub_timestamp_seconds
             (kinfo (timestamp @$ int @$ bot), halt (timestamp @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IDiff_timestamps
        ~intercept_stack:(zero_timestamp, (zero_timestamp, eos))
        ~kinstr:
          (IDiff_timestamps
             (kinfo (timestamp @$ timestamp @$ bot), halt (int @$ bot)))
        ()
  end

  module Tez = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAdd_tez
        ~kinstr:(IAdd_tez (kinfo (mutez @$ mutez @$ bot), halt (mutez @$ bot)))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISub_tez
        ~kinstr:
          (ISub_tez (kinfo (mutez @$ mutez @$ bot), halt (option mutez @$ bot)))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) =
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
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ISub_tez_legacy
        ~kinstr:
          (ISub_tez_legacy (kinfo (mutez @$ mutez @$ bot), halt (mutez @$ bot)))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) =
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
        match Alpha_context.Script_int.(is_nat (of_int64 int64)) with
        | None -> assert false
        | Some nat -> nat
      in
      (mutez, nat)

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMul_teznat
        ~kinstr:(IMul_teznat (kinfo (mutez @$ nat @$ bot), halt (mutez @$ bot)))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let (mutez, nat) = sample_tez_nat samplers rng_state in
            (mutez, (nat, eos)))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMul_nattez
        ~kinstr:(IMul_nattez (kinfo (nat @$ mutez @$ bot), halt (mutez @$ bot)))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let (mutez, nat) = sample_tez_nat samplers rng_state in
            (nat, (mutez, eos)))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IEdiv_teznat
        ~intercept_stack:
          (Alpha_context.Tez.zero, (Alpha_context.Script_int.zero_n, eos))
        ~kinstr:
          (IEdiv_teznat
             ( kinfo (mutez @$ nat @$ bot),
               halt (option (pair mutez mutez) @$ bot) ))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, samplers) = make_default_samplers cfg.sampler in
          fun () ->
            let (mutez, nat) = sample_tez_nat samplers rng_state in
            (mutez, (nat, eos)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IEdiv_tez
        ~intercept_stack:(Alpha_context.Tez.zero, (Alpha_context.Tez.zero, eos))
        ~kinstr:
          (IEdiv_tez
             ( kinfo (mutez @$ mutez @$ bot),
               halt (option (pair nat mutez) @$ bot) ))
        ()
  end

  module Booleans = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IOr
        ~kinstr:(IOr (kinfo (bool @$ bool @$ bot), halt (bool @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAnd
        ~kinstr:(IAnd (kinfo (bool @$ bool @$ bot), halt (bool @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IXor
        ~kinstr:(IXor (kinfo (bool @$ bool @$ bot), halt (bool @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INot
        ~kinstr:(INot (kinfo (bool @$ bot), halt (bool @$ bot)))
        ()
  end

  module Integers = struct
    let zero = Alpha_context.Script_int.zero

    let zero_n = Alpha_context.Script_int.zero_n

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IIs_nat
        ~intercept_stack:(zero, eos)
        ~kinstr:(IIs_nat (kinfo (int @$ bot), halt (option nat @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INeg
        ~intercept_stack:(zero, eos)
        ~kinstr:(INeg (kinfo (int @$ bot), halt (int @$ bot)))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IAbs_int
        ~kinstr:(IAbs_int (kinfo (int @$ bot), halt (nat @$ bot)))
        ~intercept_stack:(zero, eos)
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          fun () ->
            let x = Samplers.Michelson_base.nat rng_state in
            let neg_x = Alpha_context.Script_int.neg x in
            (neg_x, eos))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IInt_nat
        ~intercept_stack:(zero_n, eos)
        ~kinstr:(IInt_nat (kinfo (nat @$ bot), halt (int @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAdd_int
        ~intercept_stack:(zero, (zero, eos))
        ~kinstr:(IAdd_int (kinfo (int @$ int @$ bot), halt (int @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAdd_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~kinstr:(IAdd_nat (kinfo (nat @$ nat @$ bot), halt (nat @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISub_int
        ~intercept_stack:(zero, (zero, eos))
        ~kinstr:(ISub_int (kinfo (int @$ int @$ bot), halt (int @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IMul_int
        ~intercept_stack:(zero, (zero, eos))
        ~kinstr:(IMul_int (kinfo (int @$ int @$ bot), halt (int @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IMul_nat
        ~intercept_stack:(zero_n, (zero, eos))
        ~kinstr:(IMul_nat (kinfo (nat @$ int @$ bot), halt (int @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IEdiv_int
        ~intercept_stack:(zero, (zero, eos))
        ~kinstr:
          (IEdiv_int
             (kinfo (int @$ int @$ bot), halt (option (pair int nat) @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IEdiv_nat
        ~intercept_stack:(zero_n, (zero, eos))
        ~kinstr:
          (IEdiv_nat
             (kinfo (nat @$ int @$ bot), halt (option (pair int nat) @$ bot)))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ILsl_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~kinstr:(ILsl_nat (kinfo (nat @$ nat @$ bot), halt (nat @$ bot)))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          fun () ->
            let x = Samplers.Michelson_base.nat rng_state in
            (* shift must be in [0;256]: 1 byte max *)
            let shift =
              Script_int_repr.(abs (of_int (Random.State.int rng_state 256)))
            in
            (x, (shift, eos)))
        ()

    let () =
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_ILsr_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~kinstr:(ILsr_nat (kinfo (nat @$ nat @$ bot), halt (nat @$ bot)))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          fun () ->
            let x = Samplers.Michelson_base.nat rng_state in
            (* shift must be in [0;256]: 1 byte max *)
            let shift =
              Script_int_repr.(abs (of_int (Random.State.int rng_state 256)))
            in
            (x, (shift, eos)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IOr_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~kinstr:(IOr_nat (kinfo (nat @$ nat @$ bot), halt (nat @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAnd_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~kinstr:(IAnd_nat (kinfo (nat @$ nat @$ bot), halt (nat @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAnd_int_nat
        ~intercept_stack:(zero, (zero_n, eos))
        ~kinstr:(IAnd_int_nat (kinfo (int @$ nat @$ bot), halt (nat @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IXor_nat
        ~intercept_stack:(zero_n, (zero_n, eos))
        ~kinstr:(IXor_nat (kinfo (nat @$ nat @$ bot), halt (nat @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INot_int
        ~intercept_stack:(zero, eos)
        ~kinstr:(INot_int (kinfo (int @$ bot), halt (int @$ bot)))
        ()
  end

  module Control = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IIf
        ~kinstr:
          (IIf
             {
               kinfo = kinfo (bool @$ unit @$ bot);
               branch_if_true = halt_unit;
               branch_if_false = halt_unit;
               k = halt_unit;
             })
        ()

    let () =
      (*
        ILoop ->
        either
        - IHalt (false on top of stack)
        - IConst false ; IHalt (true on top of stack)
       *)
      let push_false = IConst (kinfo_unit, false, halt (bool @$ unit @$ bot)) in
      simple_benchmark
        ~name:Interpreter_workload.N_ILoop
        ~kinstr:(ILoop (kinfo (bool @$ unit @$ bot), push_false, halt_unit))
        ()

    let () =
      (*
        ILoop_left ->
        ICons_right ->
        IHalt
       *)
      let cons_r = ICons_right (kinfo_unit, halt (union unit unit @$ bot)) in
      simple_benchmark
        ~name:Interpreter_workload.N_ILoop_left
        ~kinstr:(ILoop_left (kinfo (union unit unit @$ bot), cons_r, halt_unit))
        ()

    let () =
      (*
        IDip ->
        IHalt ->
        IConst ->
        IHalt
       *)
      simple_benchmark
        ~name:Interpreter_workload.N_IDip
        ~kinstr:(IDip (kinfo (unit @$ unit @$ bot), halt_unit, halt_unitunit))
        ()

    let dummy_lambda =
      let open Script_typed_ir in
      let descr =
        {kloc = 0; kbef = unit @$ bot; kaft = unit @$ bot; kinstr = halt_unit}
      in
      Lam (descr, Micheline.Int (0, Z.zero))

    let () =
      (*
        IExec ->
        (switch to in-context gas-counting) ->
        interp lambda code ->
        IHalt
       *)
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IExec
        ~kinstr:(IExec (kinfo (unit @$ lambda unit unit @$ bot), halt_unit))
        ~stack_sampler:(fun _cfg _rng_state () -> ((), (dummy_lambda, eos)))
        ()

    let () =
      (*
        IApply ->
        unparse unit ->
        unparse unit_ty ->
        construct term ->
        IHalt
       *)
      let code =
        let open Script_typed_ir in
        let descr =
          {
            kloc = 0;
            kbef = pair unit unit @$ bot;
            kaft = unit @$ bot;
            kinstr = ICdr (kinfo (pair unit unit @$ bot), halt_unit);
          }
        in
        Lam (descr, Micheline.Int (0, Z.zero))
      in
      simple_benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IApply
        ~kinstr:
          (IApply
             ( kinfo (unit @$ lambda (pair unit unit) unit @$ bot),
               unit,
               halt (lambda unit unit @$ bot) ))
        ~stack_sampler:(fun _cfg _rng_state () -> ((), (code, eos)))
        ()

    let () =
      (*
        ILambda ->
        IHalt
       *)
      simple_benchmark
        ~name:Interpreter_workload.N_ILambda
        ~kinstr:
          (ILambda
             (kinfo_unit, dummy_lambda, halt (lambda unit unit @$ unit @$ bot)))
        ()

    let () =
      (*
        IFailwith ->
        (unparse_data Unit) ->
        (strip_locations) ->
        fail
       *)
      simple_benchmark
        ~name:Interpreter_workload.N_IFailwith
        ~amplification:100
        ~kinstr:(IFailwith (kinfo_unit, 0, unit))
        ()
  end

  module Comparison = struct
    let () =
      benchmark
        ~name:Interpreter_workload.N_ICompare
        ~kinstr_and_stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          fun () ->
            let size =
              Base_samplers.sample_in_interval
                rng_state
                ~range:cfg.compare.type_size
            in
            let (Script_ir_translator.Ex_comparable_ty cmp_ty) =
              Samplers.Random_type.m_comparable_type ~size rng_state
            in
            let ty = Script_ir_translator.ty_of_comparable_ty cmp_ty in
            let value = Samplers.Random_value.comparable cmp_ty rng_state in
            let kinstr =
              ICompare (kinfo (ty @$ ty @$ bot), cmp_ty, halt (int @$ bot))
            in
            Ex_stack_and_kinstr {stack = (value, (value, eos)); kinstr})
        ()
  end

  module Comparators = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IEq
        ~kinstr:(IEq (kinfo (int @$ bot), halt (bool @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INeq
        ~kinstr:(INeq (kinfo (int @$ bot), halt (bool @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ILt
        ~kinstr:(ILt (kinfo (int @$ bot), halt (bool @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IGt
        ~kinstr:(IGt (kinfo (int @$ bot), halt (bool @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ILe
        ~kinstr:(ILe (kinfo (int @$ bot), halt (bool @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IGe
        ~kinstr:(IGe (kinfo (int @$ bot), halt (bool @$ bot)))
        ()
  end

  module Proto = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAddress
        ~kinstr:(IAddress (kinfo (contract unit @$ bot), halt (address @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IContract
        ~kinstr:
          (IContract
             ( kinfo (address @$ bot),
               unit,
               Alpha_context.Entrypoint.default,
               halt (option (contract unit) @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ITransfer_tokens
        ~kinstr:
          (ITransfer_tokens
             ( kinfo (unit @$ mutez @$ contract unit @$ bot),
               halt (operation @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IImplicit_account
        ~kinstr:
          (IImplicit_account
             (kinfo (key_hash @$ bot), halt (contract unit @$ bot)))
        ()

    let () =
      let lambda =
        let open Script_typed_ir in
        let descr =
          {
            kloc = 0;
            kbef = pair unit unit @$ bot;
            kaft = pair (list operation) unit @$ bot;
            kinstr =
              ICdr
                ( kinfo (pair unit unit @$ bot),
                  INil
                    ( kinfo (unit @$ bot),
                      ICons_pair
                        ( kinfo (list operation @$ unit @$ bot),
                          IHalt (kinfo (pair (list operation) unit @$ bot)) ) )
                );
          }
        in
        Lam (descr, Micheline.Int (0, Z.zero))
      in
      simple_benchmark
        ~name:Interpreter_workload.N_ICreate_contract
        ~kinstr:
          (ICreate_contract
             {
               kinfo = kinfo (option key_hash @$ mutez @$ unit @$ bot);
               storage_type = unit;
               arg_type = unit;
               lambda;
               views = Script_map.empty string_key;
               entrypoints = no_entrypoints;
               k = halt (operation @$ address @$ bot);
             })
        ()

    let () =
      let name =
        match Protocol.Alpha_context.Script_string.of_string "view" with
        | Ok s -> s
        | Error _ -> assert false
      in
      simple_benchmark
        ~name:Interpreter_workload.N_IView
        ~kinstr:
          (IView
             ( kinfo (unit @$ address @$ bot),
               View_signature {name; input_ty = unit; output_ty = unit},
               halt (option unit @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISet_delegate
        ~kinstr:
          (ISet_delegate
             (kinfo (option key_hash @$ bot), halt (operation @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INow
        ~kinstr:(INow (kinfo (unit @$ bot), halt (timestamp @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IMin_block_time
        ~kinstr:(IMin_block_time (kinfo bot, halt (nat @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IBalance
        ~kinstr:(IBalance (kinfo (unit @$ bot), halt (mutez @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ILevel
        ~kinstr:(ILevel (kinfo (unit @$ bot), halt (nat @$ unit @$ bot)))
        ()

    let check_signature (algo : Signature.algo) ~for_intercept =
      let name =
        match algo with
        | Signature.Ed25519 -> Interpreter_workload.N_ICheck_signature_ed25519
        | Signature.Secp256k1 ->
            Interpreter_workload.N_ICheck_signature_secp256k1
        | Signature.P256 -> Interpreter_workload.N_ICheck_signature_p256
      in
      benchmark_with_stack_sampler
        ~intercept:for_intercept
        ~name
        ~kinstr:
          (ICheck_signature
             ( kinfo (public_key @$ signature @$ bytes @$ bot),
               halt (bool @$ bot) ))
        ~stack_sampler:(fun cfg rng_state ->
          let ((module Crypto_samplers), (module Samplers)) =
            make_default_samplers ~algo:(`Algo algo) cfg.Default_config.sampler
          in
          fun () ->
            let (_pkh, pk, sk) = Crypto_samplers.all rng_state in
            let unsigned_message =
              if for_intercept then Environment.Bytes.empty
              else Samplers.Random_value.value Script_typed_ir.bytes_t rng_state
            in
            let signed_message = Signature.sign sk unsigned_message in
            let signed_message = Script_signature.make signed_message in
            (pk, (signed_message, (unsigned_message, eos))))
        ()

    let check_signature algo =
      check_signature algo ~for_intercept:true ;
      check_signature algo ~for_intercept:false

    let () = check_signature Signature.Ed25519

    let () = check_signature Signature.Secp256k1

    let () = check_signature Signature.P256

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IHash_key
        ~kinstr:(IHash_key (kinfo (public_key @$ bot), halt (key_hash @$ bot)))
        ()

    let () =
      benchmark
        ~name:Interpreter_workload.N_IPack
        ~kinstr_and_stack_sampler:(fun _cfg _rng_state ->
          let kinstr = IPack (kinfo (unit @$ bot), unit, halt (bytes @$ bot)) in
          fun () -> Ex_stack_and_kinstr {stack = ((), eos); kinstr})
        ()

    let () =
      benchmark
        ~name:Interpreter_workload.N_IUnpack
        ~kinstr_and_stack_sampler:(fun _cfg rng_state ->
          let b =
            raise_if_error
              (Lwt_main.run
                 ( Execution_context.make ~rng_state >>=? fun (ctxt, _) ->
                   Script_ir_translator.pack_data ctxt unit ()
                   >|= Environment.wrap_tzresult
                   >>=? fun (bytes, _) -> return bytes ))
          in
          let kinstr =
            IUnpack (kinfo (bytes @$ bot), unit, halt (option unit @$ bot))
          in
          fun () -> Ex_stack_and_kinstr {stack = (b, eos); kinstr})
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IBlake2b
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~kinstr:(IBlake2b (kinfo (bytes @$ bot), halt (bytes @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISha256
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~kinstr:(ISha256 (kinfo (bytes @$ bot), halt (bytes @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISha512
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~kinstr:(ISha512 (kinfo (bytes @$ bot), halt (bytes @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IKeccak
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~kinstr:(IKeccak (kinfo (bytes @$ bot), halt (bytes @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISha3
        ~intercept_stack:(Environment.Bytes.empty, eos)
        ~kinstr:(ISha3 (kinfo (bytes @$ bot), halt (bytes @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISource
        ~kinstr:(ISource (kinfo (unit @$ bot), halt (address @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISender
        ~kinstr:(ISender (kinfo (unit @$ bot), halt (address @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISelf
        ~kinstr:
          (ISelf
             ( kinfo (unit @$ bot),
               unit,
               Alpha_context.Entrypoint.default,
               halt (contract unit @$ unit @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ISelf_address
        ~kinstr:
          (ISelf_address (kinfo (unit @$ bot), halt (address @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAmount
        ~kinstr:(IAmount (kinfo (unit @$ bot), halt (mutez @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IChainId
        ~kinstr:(IChainId (kinfo (unit @$ bot), halt (chain_id @$ unit @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IVoting_power
        ~kinstr:(IVoting_power (kinfo (key_hash @$ bot), halt (nat @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ITotal_voting_power
        ~kinstr:
          (ITotal_voting_power (kinfo (unit @$ bot), halt (nat @$ unit @$ bot)))
        ()
  end

  module Sapling = struct
    let () =
      let memo_size =
        match Alpha_context.Sapling.Memo_size.parse_z Z.zero with
        | Error _ -> assert false
        | Ok sz -> sz
      in
      simple_benchmark
        ~name:Interpreter_workload.N_ISapling_empty_state
        ~kinstr:
          (ISapling_empty_state
             ( kinfo (unit @$ bot),
               memo_size,
               halt (sapling_state memo_size @$ unit @$ bot) ))
        ()

    let () =
      (* Note that memo_size is hardcoded to 0 in module [Sapling_generation]. *)
      let memo_size =
        match Alpha_context.Sapling.Memo_size.parse_z Z.zero with
        | Error _ -> assert false
        | Ok sz -> sz
      in
      let (info, name) =
        info_and_name ~intercept:false "ISapling_verify_update"
      in
      let module B : Benchmark.S = struct
        let name = name

        let info = info

        include Default_config
        include Default_boilerplate

        let models =
          Interpreter_model.make_model
            (Some (Instr_name Interpreter_workload.N_ISapling_verify_update))

        let kinstr =
          let spl_state = sapling_state memo_size in
          let spl_tx = sapling_transaction memo_size in
          ISapling_verify_update
            ( kinfo (spl_tx @$ spl_state @$ bot),
              halt (option (pair int spl_state) @$ bot) )

        let prepare_sapling_execution_environment sapling_forge_rng_seed
            sapling_transition =
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
              ( Execution_context.make ~rng_state:sapling_forge_rng_state
              >>=? fun (ctxt, step_constants) ->
                (* Prepare a sapling state able to replay the transition. *)
                Sapling_generation.prepare_seeded_state sapling_transition ctxt
                >>=? fun (_, _, _, _, ctxt, state_id) ->
                Alpha_context.Sapling.(state_from_id ctxt (Id.parse_z state_id))
                >|= Environment.wrap_tzresult
                >>=? fun (state, ctxt) -> return (ctxt, state, step_constants)
              )
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
                Sapling_generation.load ~filename:sapling_txs_file
              in
              let length = List.length transitions in
              if length < bench_num then
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
                  let (ctxt, state, step_constants) =
                    prepare_sapling_execution_environment seed transition
                  in
                  let stack_instr =
                    Ex_stack_and_kinstr
                      {stack = (transition.sapling_tx, (state, eos)); kinstr}
                  in
                  benchmark_from_kinstr_and_stack
                    ctxt
                    step_constants
                    stack_instr)
                transitions
      end in
      Registration_helpers.register (module B)
  end

  (* when benchmarking, compile bls12-381-unix without ADX, see
     https://gitlab.com/dannywillems/ocaml-bls12-381/-/blob/71d0b4d467fbfaa6452d702fcc408d7a70916a80/README.md#install
  *)
  module Bls12_381 = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAdd_bls12_381_g1
        ~kinstr:
          (IAdd_bls12_381_g1
             ( kinfo (bls12_381_g1 @$ bls12_381_g1 @$ bot),
               halt (bls12_381_g1 @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAdd_bls12_381_g2
        ~kinstr:
          (IAdd_bls12_381_g2
             ( kinfo (bls12_381_g2 @$ bls12_381_g2 @$ bot),
               halt (bls12_381_g2 @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IAdd_bls12_381_fr
        ~kinstr:
          (IAdd_bls12_381_fr
             ( kinfo (bls12_381_fr @$ bls12_381_fr @$ bot),
               halt (bls12_381_fr @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IMul_bls12_381_g1
        ~kinstr:
          (IMul_bls12_381_g1
             ( kinfo (bls12_381_g1 @$ bls12_381_fr @$ bot),
               halt (bls12_381_g1 @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IMul_bls12_381_g2
        ~kinstr:
          (IMul_bls12_381_g2
             ( kinfo (bls12_381_g2 @$ bls12_381_fr @$ bot),
               halt (bls12_381_g2 @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IMul_bls12_381_fr
        ~kinstr:
          (IMul_bls12_381_fr
             ( kinfo (bls12_381_fr @$ bls12_381_fr @$ bot),
               halt (bls12_381_fr @$ bot) ))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IMul_bls12_381_z_fr
        ~kinstr:
          (IMul_bls12_381_z_fr
             (kinfo (bls12_381_fr @$ int @$ bot), halt (bls12_381_fr @$ bot)))
        ()

    let () =
      benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMul_bls12_381_z_fr
        ~intercept:true
        ~kinstr:
          (IMul_bls12_381_z_fr
             (kinfo (bls12_381_fr @$ int @$ bot), halt (bls12_381_fr @$ bot)))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          let fr_sampler = Samplers.Random_value.value bls12_381_fr in
          let zero = Alpha_context.Script_int.zero in
          fun () -> (fr_sampler rng_state, (zero, eos)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IMul_bls12_381_fr_z
        ~kinstr:
          (IMul_bls12_381_fr_z
             (kinfo (int @$ bls12_381_fr @$ bot), halt (bls12_381_fr @$ bot)))
        ()

    let () =
      benchmark_with_stack_sampler
        ~name:Interpreter_workload.N_IMul_bls12_381_fr_z
        ~intercept:true
        ~kinstr:
          (IMul_bls12_381_fr_z
             (kinfo (int @$ bls12_381_fr @$ bot), halt (bls12_381_fr @$ bot)))
        ~stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          let fr_sampler = Samplers.Random_value.value bls12_381_fr in
          let zero = Alpha_context.Script_int.zero in
          fun () -> (zero, (fr_sampler rng_state, eos)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IInt_bls12_381_z_fr
        ~kinstr:
          (IInt_bls12_381_fr (kinfo (bls12_381_fr @$ bot), halt (int @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INeg_bls12_381_g1
        ~kinstr:
          (INeg_bls12_381_g1
             (kinfo (bls12_381_g1 @$ bot), halt (bls12_381_g1 @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INeg_bls12_381_g2
        ~kinstr:
          (INeg_bls12_381_g2
             (kinfo (bls12_381_g2 @$ bot), halt (bls12_381_g2 @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_INeg_bls12_381_fr
        ~kinstr:
          (INeg_bls12_381_fr
             (kinfo (bls12_381_fr @$ bot), halt (bls12_381_fr @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IPairing_check_bls12_381
        ~kinstr:
          (IPairing_check_bls12_381
             ( kinfo (list (pair bls12_381_g1 bls12_381_g2) @$ bot),
               halt (bool @$ bot) ))
        ()
  end

  module Tickets = struct
    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_ITicket
        ~kinstr:
          (ITicket (kinfo (unit @$ nat @$ bot), halt (ticket unit_cmp @$ bot)))
        ()

    let () =
      simple_benchmark
        ~name:Interpreter_workload.N_IRead_ticket
        ~kinstr:
          (IRead_ticket
             ( kinfo (ticket unit_cmp @$ bot),
               halt (pair address (pair unit nat) @$ ticket unit_cmp @$ bot) ))
        ()

    let split_ticket_instr =
      ISplit_ticket
        ( kinfo (ticket unit_cmp @$ pair nat nat @$ bot),
          halt (option (pair (ticket unit_cmp) (ticket unit_cmp)) @$ bot) )

    let () =
      let zero = Alpha_context.Script_int.zero_n in
      let ticket =
        {
          ticketer =
            Alpha_context.Contract.implicit_contract
              Environment.Signature.Public_key_hash.zero;
          contents = ();
          amount = zero;
        }
      in
      benchmark_with_fixed_stack
        ~intercept:true
        ~name:Interpreter_workload.N_ISplit_ticket
        ~stack:(ticket, ((zero, zero), eos))
        ~kinstr:split_ticket_instr
        ()

    let () =
      benchmark
        ~name:Interpreter_workload.N_ISplit_ticket
        ~kinstr_and_stack_sampler:(fun config rng_state ->
          let (_, (module Samplers)) =
            make_default_samplers config.Default_config.sampler
          in
          fun () ->
            let half_amount = Samplers.Random_value.value nat rng_state in
            let amount =
              Alpha_context.Script_int.add_n half_amount half_amount
            in
            let ticket =
              Samplers.Random_value.value (ticket unit_cmp) rng_state
            in
            let ticket = {ticket with amount} in
            Ex_stack_and_kinstr
              {
                stack = (ticket, ((half_amount, half_amount), eos));
                kinstr = split_ticket_instr;
              })
        ()

    let join_tickets_instr =
      IJoin_tickets
        ( kinfo (pair (ticket string_cmp) (ticket string_cmp) @$ bot),
          string_cmp,
          halt (option (ticket string_cmp) @$ bot) )

    let () =
      benchmark
        ~intercept:true
        ~name:Interpreter_workload.N_IJoin_tickets
        ~kinstr_and_stack_sampler:(fun config rng_state ->
          let (_, (module Samplers)) =
            make_default_samplers config.Default_config.sampler
          in
          fun () ->
            let ticket =
              Samplers.Random_value.value (ticket string_cmp) rng_state
            in
            let ticket =
              {
                ticket with
                contents = Alpha_context.Script_string.empty;
                amount = Script_int_repr.zero_n;
              }
            in
            Ex_stack_and_kinstr
              {stack = ((ticket, ticket), eos); kinstr = join_tickets_instr})
        ()

    let () =
      benchmark
        ~name:Interpreter_workload.N_IJoin_tickets
        ~kinstr_and_stack_sampler:(fun config rng_state ->
          let (_, (module Samplers)) =
            make_default_samplers config.Default_config.sampler
          in
          fun () ->
            let ticket =
              Samplers.Random_value.value (ticket string_cmp) rng_state
            in
            let alt_amount = Samplers.Random_value.value nat rng_state in
            let ticket' = {ticket with amount = alt_amount} in
            Ex_stack_and_kinstr
              {stack = ((ticket, ticket'), eos); kinstr = join_tickets_instr})
        ()
  end

  module Timelock = struct
    let name = Interpreter_workload.N_IOpen_chest

    let kinstr =
      IOpen_chest
        ( kinfo
            (Michelson_types.chest_key @$ Michelson_types.chest @$ nat @$ bot),
          halt (union bytes bool @$ bot) )

    let resulting_stack chest chest_key time =
      let chest = Script_timelock.make_chest chest in
      let chest_key = Script_timelock.make_chest_key chest_key in
      ( chest_key,
        ( chest,
          ( Script_int_repr.is_nat (Script_int_repr.of_int time)
            |> WithExceptions.Option.get ~loc:"Timelock:gas benchmarks",
            eos ) ) )

    let () =
      benchmark_with_stack_sampler
        ~intercept:true
        ~name
        ~kinstr
        ~stack_sampler:(fun _ rng_state () ->
          let (chest, chest_key) =
            Timelock_samplers.chest_sampler ~plaintext_size:1 ~time:0 ~rng_state
          in
          resulting_stack chest chest_key 0)
        ()

    let () =
      benchmark_with_stack_sampler
        ~name
        ~kinstr
        ~stack_sampler:(fun _ rng_state () ->
          let log_time =
            Base_samplers.sample_in_interval
              ~range:{min = 0; max = 29}
              rng_state
          in
          let time = Random.State.int rng_state (Int.shift_left 1 log_time) in
          let plaintext_size =
            Base_samplers.sample_in_interval
              ~range:{min = 1; max = 10000}
              rng_state
          in

          let (chest, chest_key) =
            Timelock_samplers.chest_sampler ~plaintext_size ~time ~rng_state
          in
          resulting_stack chest chest_key time)
        ()
  end

  module Continuations = struct
    let () =
      (*
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KNil
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KNil in
          let stack = eos in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KCons -> step
        KHalt -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KCons
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KCons (halt_unit, KNil) in
          let stack = ((), eos) in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KReturn -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KReturn
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KReturn (halt_unit, KNil) in
          let stack = ((), eos) in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KView_exit -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KView_exit
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let open Script_typed_ir in
          let open Alpha_context in
          let zero =
            Contract.implicit_contract Signature.Public_key_hash.zero
          in
          let step_constants =
            {
              source = zero;
              payer = zero;
              self = zero;
              amount = Tez.zero;
              balance = Tez.zero;
              chain_id = Chain_id.zero;
              now = Script_timestamp.of_zint Z.zero;
              level = Script_int.zero_n;
            }
          in
          let cont = KView_exit (step_constants, KNil) in
          let stack = ((), eos) in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KLoop_in -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KLoop_in
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont =
            KLoop_in
              (IConst (kinfo_unit, false, halt (bool @$ unit @$ bot)), KNil)
          in
          let stack = (false, ((), eos)) in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KLoop_in_left -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KLoop_in_left
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont =
            KLoop_in_left
              (ICons_right (kinfo_unit, halt (union unit unit @$ bot)), KNil)
          in
          let stack = (R (), eos) in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KUndip -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KUndip
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KUndip ((), KNil) in
          let stack = eos in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KIter (empty case) -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KIter
        ~salt:"_empty"
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KIter (IDrop (kinfo_unitunit, halt_unit), [], KNil) in
          let stack = ((), eos) in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KIter (nonempty case) -> step
        KDrop -> step
        KHalt -> next
        KIter (empty case) -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KIter
        ~salt:"_nonempty"
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let cont = KIter (IDrop (kinfo_unitunit, halt_unit), [()], KNil) in
          let stack = ((), eos) in
          fun () -> Ex_stack_and_cont {stack; cont})
        ()

    let () =
      (*
        KList_enter_body ([()], bot accumulator case) -> step
        KHalt -> next
        KList_exit_body ([], []) ->
        KList_enter_body ([], [()] ->
        List.rev singleton
        KNil
      *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KList_enter_body
        ~salt:"_singleton_list"
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let kbody = halt_unitunit in
          fun () ->
            let cont = KList_enter_body (kbody, [()], [], 1, KNil) in
            Ex_stack_and_cont {stack = ((), eos); cont})
        ()

    let () =
      (*
        KList_enter_body (empty list, nonempty accumulator case) ->
        {List.rev n elements} -> next
        KNil
      *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KList_enter_body
        ~salt:"_terminal"
        ~cont_and_stack_sampler:(fun cfg rng_state ->
          let (_, (module Samplers)) = make_default_samplers cfg.sampler in
          let kbody = halt_unitunit in
          fun () ->
            let ys = Samplers.Random_value.value (list unit) rng_state in
            let cont =
              KList_enter_body (kbody, [], ys.elements, ys.length, KNil)
            in
            Ex_stack_and_cont {stack = ((), eos); cont})
        ()

    let () =
      (*
        KList_enter_body (empty list, bot accumulator case) ->
        {List.rev singleton} -> next
        KNil
      *)
      continuation_benchmark
        ~amplification:100
        ~intercept:true
        ~name:Interpreter_workload.N_KList_enter_body
        ~salt:"_terminal"
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let kbody = halt_unitunit in
          fun () ->
            let cont = KList_enter_body (kbody, [], [], 1, KNil) in
            Ex_stack_and_cont {stack = ((), eos); cont})
        ()

    let () =
      (*
        KList_exit_body (empty list) -> next
        KList_enter_body ->
        {List.rev 1 element} -> next
        KNil
      *)
      continuation_benchmark
        ~amplification:100
        ~intercept:true
        ~name:Interpreter_workload.N_KList_exit_body
        ~salt:"_terminal"
        ~cont_and_stack_sampler:(fun _cfg _rng_state ->
          let kbody = halt_unitunit in
          let cont = KList_exit_body (kbody, [], [], 1, KNil) in
          fun () -> Ex_stack_and_cont {stack = ((), ((), eos)); cont})
        ()

    let map_enter_body_code =
      let kbody = ICdr (kinfo (pair int unit @$ unit @$ bot), halt_unitunit) in
      fun accu -> KMap_enter_body (kbody, accu, Script_map.empty int_cmp, KNil)

    let () =
      (*
        KMap_enter_body (empty case) -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~salt:"_empty"
        ~name:Interpreter_workload.N_KMap_enter_body
        ~cont_and_stack_sampler:(fun _cfg _rng_state () ->
          Ex_stack_and_cont {stack = ((), eos); cont = map_enter_body_code []})
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
      continuation_benchmark
        ~amplification:100
        ~salt:"_singleton"
        ~name:Interpreter_workload.N_KMap_enter_body
        ~cont_and_stack_sampler:(fun _cfg _rng_state () ->
          Ex_stack_and_cont
            {
              stack = ((), eos);
              cont = map_enter_body_code [(Script_int_repr.zero, ())];
            })
        ()

    let () =
      (*
        KMap_exit_body ->
        (map_update) -> next
        KMap_enter_body (empty case) -> next
        KNil
       *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KMap_exit_body
        ~cont_and_stack_sampler:(fun cfg rng_state ->
          let kbody =
            ICdr (kinfo (pair int unit @$ unit @$ bot), halt_unitunit)
          in
          fun () ->
            let (key, map) = Maps.generate_map_and_key_in_map cfg rng_state in
            let cont = KMap_exit_body (kbody, [], map, key, KNil) in
            Ex_stack_and_cont {stack = ((), ((), eos)); cont})
        ()

    let () =
      (* KMap_head -> KNil *)
      continuation_benchmark
        ~amplification:100
        ~name:Interpreter_workload.N_KMap_head
        ~cont_and_stack_sampler:(fun _cfg _rng_state () ->
          let cont = KMap_head (Option.some, KNil) in
          Ex_stack_and_cont {stack = ((), ((), eos)); cont})
        ()
  end
end
