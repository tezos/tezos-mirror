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

let ns = Namespace.make Registration_helpers.ns "cache"

let fv s = Free_variable.of_namespace (ns s)

(** {2 [Alpha_context.Cache]-related benchmarks} *)

let assert_ok_lwt x =
  match Lwt_main.run x with Ok x -> x | Error _ -> assert false

let assert_ok = function Ok x -> x | Error _ -> assert false

module Admin = Alpha_context.Cache.Admin

(** We can't construct a dummy cache client from outside the protocol.
    We'll have to benchmark the {!Environment_cache} through the interface
    exposed by {!Script_cache}. *)
module Cache = Script_cache

(** {2 Constructing a dummy cached value} *)

let make_context ~rng_state =
  Execution_context.make ~rng_state |> assert_ok_lwt |> fst

let throwaway_context =
  let rng_state = Random.State.make [|0x1337; 0x533D|] in
  make_context ~rng_state

let dummy_script : Cache.cached_contract =
  let str = "{ parameter unit; storage unit; code FAILWITH }" in
  let storage =
    let parsed, _ = Michelson_v1_parser.parse_expression "Unit" in
    Alpha_context.Script.lazy_expr parsed.expanded
  in
  let code =
    let parsed, _ = Michelson_v1_parser.parse_expression ~check:false str in
    Alpha_context.Script.lazy_expr parsed.expanded
  in
  let script = Alpha_context.Script.{code; storage} in
  let ex_script, _ =
    Script_ir_translator.parse_script
      throwaway_context
      ~elab_conf:(Script_ir_translator_config.make ~legacy:true ())
      ~allow_forged_in_storage:false
      script
    |> assert_ok_lwt
  in
  (script, ex_script)

(** {2 Creating dummy cache value identifiers.} *)

(** Configuration shared among all cache benchmarks. *)
module Cache_shared_config = struct
  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = {cache_cardinal : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {cache_cardinal} -> cache_cardinal)
      (fun cache_cardinal -> {cache_cardinal})
      (obj1 (req "cache_cardinal" int31))

  let tags = [Tags.cache]

  let workload_to_vector {cache_cardinal} =
    Sparse_vec.String.of_list [("cache_cardinal", float_of_int cache_cardinal)]
end

(* We can't produce a Script_cache.identifier without calling [Script_cache.find]. *)
let identifier_of_contract (c : Contract_hash.t) : Cache.identifier =
  let _, id, _ = Cache.find throwaway_context c |> assert_ok_lwt in
  id

let contract_of_int i : Contract_hash.t =
  Contract_hash.(of_b58check_exn (to_b58check (hash_string [string_of_int i])))

let identifier_of_int i = identifier_of_contract @@ contract_of_int i

(** Prepare a context with a cache of the prescribed cardinality. A key in the domain of
    the cache is returned along the context: this key is used to benchmark
    (successful) cache accesses. *)
let prepare_context rng_state cache_cardinal =
  assert (cache_cardinal > 0) ;
  let ctxt = make_context ~rng_state in
  let some_key_in_domain = identifier_of_int 0 in
  let rec loop i ctxt =
    if Compare.Int.(i = cache_cardinal) then ctxt
    else
      let key = identifier_of_int i in
      loop (i + 1) (Cache.update ctxt key dummy_script 1 |> assert_ok)
  in
  (loop 0 ctxt, some_key_in_domain)

(** Benchmark {!Script_cache.update}. This almost directly calls {!Environment_cache.update}.
    We also use the result of this benchmark to assign a cost to {!Environment_cache.find},
    which alas can't be directly benchmarked from the interface provided by {!Script_cache}. *)
module Cache_update_benchmark : Benchmark.S = struct
  include Cache_shared_config

  let name = ns "CACHE_UPDATE"

  let info = "Benchmarking the time it takes to update a key in the cache"

  let module_filename = __FILE__

  let generated_code_destination = None

  (** It is expected that cache keys are non-adversarial,
      ie do not share a long common prefix. This is the case for [Script_cache],
      for which the keys are B58-encoded contract hashes.

      To rephrase: with high probability, comparing two keys in the domain of the cache is
      a constant-time operation (two keys will differ after the first few characters).
      We therefore do not take into account the length of the key in the model. *)
  let model =
    let affine_logn ~intercept ~coeff =
      let open Model in
      let module M = struct
        let name = name

        type arg_type = int * unit

        module Def (X : Costlang.S) = struct
          open X

          type model_type = size -> size

          let arity = arity_1

          let model =
            lam ~name:"size" @@ fun size ->
            free ~name:intercept + (free ~name:coeff * log2 (int 1 + size))
        end
      end in
      (module M : Model_impl with type arg_type = int * unit)
    in
    (* Looking at the plots, it looks like this benchmark underestimates the constant term.
       In the interpreter, this would warrant a dedicated benchmark for the intercept. *)
    let intercept_variable =
      fv (Format.asprintf "%s_const" (Namespace.basename name))
    in
    let coeff_variable =
      fv (Format.asprintf "%s_coeff" (Namespace.basename name))
    in
    Model.make
      ~conv:(function {cache_cardinal} -> (cache_cardinal, ()))
      ~model:(affine_logn ~intercept:intercept_variable ~coeff:coeff_variable)

  let models = [("cache_model", model)]

  let cache_update_benchmark ctxt some_key_in_domain cache_cardinal =
    let workload = {cache_cardinal} in
    let closure () =
      ignore (Cache.update ctxt some_key_in_domain dummy_script 1)
    in
    Generator.Plain {workload; closure}

  (** At the time of writing (Protocol H) the worst case execution path for
      [Cache.update] is to update a key which is already present. *)
  let make_bench rng_state _cfg () =
    let cache_cardinal =
      Base_samplers.sample_in_interval ~range:{min = 1; max = 100_000} rng_state
    in
    let ctxt, some_key_in_domain = prepare_context rng_state cache_cardinal in
    cache_update_benchmark ctxt some_key_in_domain cache_cardinal

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Cache_update_benchmark)
