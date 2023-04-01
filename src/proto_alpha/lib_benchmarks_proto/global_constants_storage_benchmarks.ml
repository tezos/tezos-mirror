(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <team@marigold.dev>                           *)
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

(* This module includes benchmarks for [Global_constants_storage.expand]
   and [Global_constants_storage.Internal_for_tests.expr_to_address_in_context].
   The other main function exported by [Global_constants_storage] is [register];
   however, [register] calls [expand] and does little else, and thus does
   not need to be further carbonated.

   In the process of creating these benchmarks, we benchmarked several OCaml
   stdlib functions and [Script_expr_hash.of_b58check_opt]. While these cost
   models are not used in the protocol, they are kept here to ensure the
   assumptions underlying [register] and [expand] don't change out
   from under us.*)

open Tezos_benchmark
open Benchmarks_proto
open Tezos_micheline
open Protocol

let ns = Namespace.make Registration_helpers.ns "global_constants_storage"

let fv s = Free_variable.of_namespace (ns s)

let assert_ok_lwt x =
  match Lwt_main.run x with
  | Ok x -> x
  | Error errs ->
      Format.eprintf "%a@." pp_print_trace errs ;
      exit 1

let assert_ok = function
  | Ok x -> x
  | Error errs ->
      Format.eprintf "%a@." pp_print_trace errs ;
      exit 1

(** [seq_of_n_constants n hash] generates a Seq filled
    with [n] constant primitives containing [hash] *)
let seq_of_n_constants n hash =
  let open Micheline in
  Seq
    ( -1,
      Stdlib.List.init n (fun _ ->
          Prim (-1, Michelson_v1_primitives.H_constant, [String (-1, hash)], []))
    )

(** Computes the b58check hash of a Micheline node as a string.  *)
let node_to_hash node =
  let expr_bytes =
    Micheline.strip_locations node
    |> Script_repr.lazy_expr |> Script_repr.force_bytes |> Stdlib.Result.get_ok
  in
  Script_expr_hash.hash_bytes [expr_bytes] |> Script_expr_hash.to_b58check

(* An ad-hoc sampler for Micheline values. Boltzmann sampling would do well
   here.

   Copied from lib_micheline and modified to use [Michelson_v1_primitives.prim]. *)
module Micheline_sampler = struct
  type node = Alpha_context.Script.node

  let prims =
    let open Protocol.Michelson_v1_primitives in
    [|
      K_parameter;
      K_storage;
      K_code;
      D_False;
      D_Elt;
      D_Left;
      D_None;
      D_Pair;
      D_Right;
      D_Some;
      D_True;
      D_Unit;
      I_PACK;
      I_UNPACK;
      I_BLAKE2B;
      I_SHA256;
      I_SHA512;
      I_ABS;
      I_ADD;
      I_AMOUNT;
      I_AND;
      I_BALANCE;
      I_CAR;
      I_CDR;
      I_CHAIN_ID;
      I_CHECK_SIGNATURE;
      I_COMPARE;
      I_CONCAT;
      I_CONS;
      I_CREATE_ACCOUNT;
      I_CREATE_CONTRACT;
      I_IMPLICIT_ACCOUNT;
      I_DIP;
      I_DROP;
      I_DUP;
      I_EDIV;
      I_EMPTY_BIG_MAP;
      I_EMPTY_MAP;
      I_EMPTY_SET;
      I_EQ;
      I_EXEC;
      I_APPLY;
      I_FAILWITH;
      I_GE;
      I_GET;
      I_GET_AND_UPDATE;
      I_GT;
      I_HASH_KEY;
      I_IF;
      I_IF_CONS;
      I_IF_LEFT;
      I_IF_NONE;
      I_INT;
      I_LAMBDA;
      I_LE;
      I_LEFT;
      I_LEVEL;
      I_LOOP;
      I_LSL;
      I_LSR;
      I_LT;
      I_MAP;
      I_MEM;
      I_MUL;
      I_NEG;
      I_NEQ;
      I_NIL;
      I_NONE;
      I_NOT;
      I_NOW;
      I_OR;
      I_PAIR;
      I_UNPAIR;
      I_PUSH;
      I_RIGHT;
      I_SIZE;
      I_SOME;
      I_SOURCE;
      I_SENDER;
      I_SELF;
      I_SELF_ADDRESS;
      I_SLICE;
      I_STEPS_TO_QUOTA;
      I_SUB;
      I_SWAP;
      I_TRANSFER_TOKENS;
      I_SET_DELEGATE;
      I_UNIT;
      I_UPDATE;
      I_XOR;
      I_ITER;
      I_LOOP_LEFT;
      I_ADDRESS;
      I_CONTRACT;
      I_ISNAT;
      I_CAST;
      I_RENAME;
      I_SAPLING_EMPTY_STATE;
      I_SAPLING_VERIFY_UPDATE;
      I_DIG;
      I_DUG;
      I_NEVER;
      I_VOTING_POWER;
      I_TOTAL_VOTING_POWER;
      I_KECCAK;
      I_SHA3;
      I_PAIRING_CHECK;
      I_TICKET;
      I_READ_TICKET;
      I_SPLIT_TICKET;
      I_JOIN_TICKETS;
      T_bool;
      T_contract;
      T_int;
      T_key;
      T_key_hash;
      T_lambda;
      T_list;
      T_map;
      T_big_map;
      T_nat;
      T_option;
      T_or;
      T_pair;
      T_set;
      T_signature;
      T_string;
      T_bytes;
      T_mutez;
      T_timestamp;
      T_unit;
      T_operation;
      T_address;
      T_sapling_transaction_deprecated;
      T_sapling_state;
      T_chain_id;
      T_never;
      T_bls12_381_g1;
      T_bls12_381_g2;
      T_bls12_381_fr;
      T_ticket
      (* We don't want constants in our generator, else the constants
         functions might fail because it's ill-formed. *)
      (* H_constant; *);
    |]

  module Sampler = Micheline_sampler.Make (struct
    type prim = Michelson_v1_primitives.prim

    let sample_prim : Michelson_v1_primitives.prim Base_samplers.sampler =
     fun rng_state ->
      let i = Random.State.int rng_state (Array.length prims) in
      prims.(i)

    let sample_annots : string list Base_samplers.sampler = fun _rng_state -> []

    let sample_string _ = ""

    let sample_bytes _ = Bytes.empty

    let sample_z _ = Z.zero

    let width_function = Micheline_sampler.reasonable_width_function
  end)

  let sample = Sampler.sample

  type size = {nodes : int; bytes : int}

  let int z = {nodes = 1; bytes = (Z.numbits z + 7) / 8}

  let string s = {nodes = 1; bytes = String.length s}

  let bytes b = {nodes = 1; bytes = Bytes.length b}

  let node = {nodes = 1; bytes = 0}

  let ( @+ ) x y = {nodes = x.nodes + y.nodes; bytes = x.bytes + y.bytes}

  let micheline_size (n : node) =
    let rec micheline_size n acc =
      let open Micheline in
      match n with
      | Int (_, i) -> acc @+ int i
      | String (_, s) -> acc @+ string s
      | Bytes (_, b) -> acc @+ bytes b
      | Seq (_, terms) ->
          List.fold_left
            (fun acc term -> micheline_size term acc)
            (acc @+ node)
            terms
      | Prim (_, _, terms, _) ->
          List.fold_left
            (fun acc term -> micheline_size term acc)
            (acc @+ node)
            terms
    in
    micheline_size n {nodes = 0; bytes = 0}
end

(** Cost model and benchmarks for set element addition from the
    OCaml stdlib.

    The cost model is not currently used
    in the protocol, but we include the benchmarks to validate our
    assumptions about functions that use this. *)
module Set_add : Benchmark.S = struct
  let name = ns "Set_add"

  let info =
    "Benchmarks and cost model for set element addition from OCaml stdlib."

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["global_constants"]

  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = int

  let workload_encoding = Data_encoding.int31

  let workload_to_vector : workload -> Sparse_vec.String.t =
   fun size -> Sparse_vec.String.of_list [("size", float_of_int size)]

  (*  As an OCaml set is a balanced binary tree, complexity is O(log n). *)
  let model = Model.(make ~conv:(fun size -> (size, ())) ~model:logn)

  module Int_set = Set.Make (Int)

  let create_benchmark ~rng_state _config =
    let range : Base_samplers.range = {min = 0; max = 10_000} in
    let size = Base_samplers.sample_in_interval ~range rng_state in
    let set = Stdlib.List.init size Fun.id |> Int_set.of_list in
    let closure () = ignore (Int_set.add (size + 1) set) in
    Generator.Plain {workload = size; closure}
end

let () = Registration.register (module Set_add)

(** Cost model and benchmarks for set elements from the
    OCaml stdlib.

    The cost model is not currently used
    in the protocol, but we include the benchmarks to validate our
    assumptions about functions that use this. *)
module Set_elements : Benchmark.S = struct
  let name = ns "Set_elements"

  let info = "Benchmarks and cost model for set elements from OCaml stdlib."

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["global_constants"]

  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = int

  let workload_encoding = Data_encoding.int31

  let workload_to_vector : workload -> Sparse_vec.String.t =
   fun size -> Sparse_vec.String.of_list [("size", float_of_int size)]

  (* Cost of retrieving all elements from the set is linear with the size
      of the set.*)
  let model = Model.(make ~conv:(fun size -> (size, ())) ~model:linear)

  module Int_set = Set.Make (Int)

  let create_benchmark ~rng_state _config =
    let range : Base_samplers.range = {min = 0; max = 10_000} in
    let size = Base_samplers.sample_in_interval ~range rng_state in
    let set = Stdlib.List.init size (fun x -> x) |> Int_set.of_list in
    let closure () = ignore (Int_set.elements set) in
    Generator.Plain {workload = size; closure}
end

let () = Registration.register (module Set_elements)

(** Cost model and benchmarks for [Script_expr_hash.of_b58_check_opt].
    Under the hood this function uses the [Blake2b] functor, which uses
    the HACL* crypto library.

    The cost model is not currently used
    in the protocol, but we include the benchmarks to validate our
    assumptions about functions that use this. *)
module Script_expr_hash_of_b58check_opt : Benchmark.S = struct
  let name = ns "Script_expr_hash_of_b58check_opt"

  let info = "Benchmark for Script_expr_hash.of_b58check_opt"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["global_constants"]

  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = Micheline_sampler.size

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun Micheline_sampler.{nodes; bytes} -> (nodes, bytes))
      (fun (nodes, bytes) -> {nodes; bytes})
      (obj2 (req "nodes" int31) (req "bytes" int31))

  let workload_to_vector Micheline_sampler.{nodes; bytes} =
    Sparse_vec.String.of_list
      [("nodes", float_of_int nodes); ("bytes", float_of_int bytes)]

  (* On testing we found that this function is a constant
     time operation. However, to test this, we use an affine model. If
     our assumption holds, the coefficient should be near zero. *)
  let model =
    Model.(
      make ~conv:(fun Micheline_sampler.{nodes; _} -> (nodes, ())) ~model:affine)

  (* To create realistic benchmarks, we generate a random Micheline expression,
     hash it, then benchmark the cost of validating the hash. *)
  let create_benchmark ~rng_state _config =
    let open Protocol in
    let term = Micheline_sampler.sample rng_state in
    let size = Micheline_sampler.micheline_size term in
    let expr_encoding = Alpha_context.Script.expr_encoding in
    let lazy_expr =
      Data_encoding.make_lazy expr_encoding (Micheline.strip_locations term)
    in
    let expr_bytes = Data_encoding.force_bytes lazy_expr in
    let hash = Script_expr_hash.hash_bytes [expr_bytes] in
    let hash_str = Script_expr_hash.to_b58check hash in
    let closure () = ignore (Script_expr_hash.of_b58check_opt hash_str) in
    Generator.Plain {workload = size; closure}
end

let () = Registration.register (module Script_expr_hash_of_b58check_opt)

module Global_constants_storage_expr_to_address_in_context : Benchmark.S =
struct
  let name = ns "expr_to_address_in_context"

  let info =
    "Benchmark for the  \
     Global_constants_storage.Internal_for_tests.expr_to_address_in_context \
     function"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["global_constants"]

  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = int

  let workload_encoding = Data_encoding.int31

  let workload_to_vector : workload -> Sparse_vec.String.t =
   fun size -> Sparse_vec.String.of_list [("size", float_of_int size)]

  (** The cost of a Blake2b hashing function is linear with the size of the input *)
  let model = Model.(make ~conv:(fun size -> (size, ())) ~model:linear)

  let create_benchmark ~rng_state _config =
    let open Micheline in
    let expr = Micheline_sampler.sample rng_state |> strip_locations in
    let b =
      Script_repr.lazy_expr expr |> Script_repr.force_bytes
      |> Environment.wrap_tzresult |> assert_ok
    in
    let size = Bytes.length b in

    let closure () = ignore (Script_expr_hash.hash_bytes [b]) in
    Generator.Plain {workload = size; closure}
end

let () =
  Registration.register
    (module Global_constants_storage_expr_to_address_in_context)

(** [Global_constants_storage.expand] traverses a Micheline node,
    searching for constants and replacing them with their values
    retrieved from storage.

    There are three branches in the iterations of [Global_constants_storage.expand]
    can take, each with different costs:
    - Branch 1: The first time a particular constant is found, the hash is parsed with
      [Script_expr_hash.of_b58check_opt], and its value is retrieved
      from storage. This storage call (implemented [Global_constants_storage.get])
      is already carbonated and dominates the cost in this case, so do not need to
      benchmark Branch 1 - the benchmarks for storage access are sufficient.
    - Branch 2: If the same constant is found a subsequent time, its value is looked up
      in a map. On testing we determined that the cost of [Script_expr_hash.of_b58check_opt]
      dominates the cost of this branch - the cost of an OCaml map lookup is O(log 2 n), and
      n has to be unreasonably large to catch up to the constant time cost of validating the
      hash.
    - Branch 3: When no constant is found, the cost is merely that of pattern matching
      and calling the continuation (similar to that of [Micheline.strip_locations]).

    Because we don't know the full size of node being traversed ahead of time (because they
    are retrieved from storage), it is impossible to calculate the full gas cost upfront.
    However, each time we find a new expression to traverse, we can calculate its size upfront
    and charge the cost of all Branch 3 cases. We can then do an additional charge for Branch 2
    each time we find a constant, and let storage handle charging for Branch 1.

    Below are models for Branch 2 and 3 respectively.
    *)
module Global_constants_storage_expand_models = struct
  module Global_constants_storage_expand_constant_branch : Benchmark.S = struct
    let name = ns "expand_constant_branch"

    let info =
      "Benchmark for the constant branch Global_constants_storage.expand \
       function"

    let module_filename = __FILE__

    let generated_code_destination = None

    let tags = ["global_constants"]

    type config = unit

    let config_encoding = Data_encoding.unit

    let default_config = ()

    type workload = int

    let workload_encoding = Data_encoding.int31

    let workload_to_vector : workload -> Sparse_vec.String.t =
     fun constants ->
      Sparse_vec.String.of_list
        [("number_of_constants", float_of_int constants)]

    (** The cost of Branch 2 is linear to the number of constants in the expression. As
        discussed above, the constant time operation [Script_expr_hash.of_b58check_opt]
        dominates the cost of each iteration. *)
    let model = Model.(make ~conv:(fun size -> (size, ())) ~model:linear)

    (* To test Branch 2 as nearly as possible, we generate a Micheline Seq
       consisting of the same constant repeated n times. As n increases,
       the benchmark more closely approximates the true cost of Branch 2. *)
    let create_benchmark ~rng_state _config =
      let open Micheline in
      let node = Micheline_sampler.sample rng_state in
      let size = (Micheline_sampler.micheline_size node).nodes in
      let registered_constant = Int (-1, Z.of_int 1) in
      let hash = registered_constant |> node_to_hash in
      let context, _ = Execution_context.make ~rng_state |> assert_ok_lwt in
      let context, _, _ =
        Alpha_context.Global_constants_storage.register
          context
          (strip_locations registered_constant)
        >|= Environment.wrap_tzresult |> assert_ok_lwt
      in
      let node = seq_of_n_constants size hash in
      let closure () =
        ignore
          (Lwt_main.run
          @@ Alpha_context.Global_constants_storage.expand
               context
               (strip_locations node))
      in
      Generator.Plain {workload = size; closure}
  end

  let () =
    Registration.register
      (module Global_constants_storage_expand_constant_branch)

  module Global_constants_storage_expand_no_constant_branch : Benchmark.S =
  struct
    let name = ns "expand_no_constant_branch"

    let info =
      "Benchmark for the Global_constants_storage.expand function on the case \
       without constants"

    let module_filename = __FILE__

    let generated_code_destination = None

    let tags = ["global_constants"]

    type config = unit

    let config_encoding = Data_encoding.unit

    let default_config = ()

    type workload = int

    let workload_encoding = Data_encoding.int31

    let workload_to_vector : workload -> Sparse_vec.String.t =
     fun size ->
      Sparse_vec.String.of_list [("number_of_nodes", float_of_int size)]

    (* The cost of Branch 3 is the cost of traversing a single node. It
       is therefore linear to the number of nodes being traversed. This is
       very similar to [Micheline.strip_locations].

       On testing I observed that while the linear model was accurate
       for small numbers of nodes, after 1000 nodes the cost seems to increase more
       than linearly. I think I would have to fine tune the sampler to better test
       past this amount; however, I don't think it's necessary - to get large orders
       of nodes, you need to use constants, in which case the cost of
       [Script_expr_hash.of_b58check_opt] will dominate. A n*log(n) model seems
       accurate enough for the range of values tested.
    *)
    let model = Model.(make ~conv:(fun size -> (size, ())) ~model:nlogn)

    (** We benchmark this by generating a random Micheline expression without constants
        and calling [expand] on it. This causes the function to spend all its time in
        Branch 3. *)
    let create_benchmark ~rng_state _config =
      let open Micheline in
      let node = Micheline_sampler.sample rng_state in
      let size = (Micheline_sampler.micheline_size node).nodes in
      let context, _ = Execution_context.make ~rng_state |> assert_ok_lwt in
      let expr = strip_locations node in
      let closure () =
        ignore
          (Lwt_main.run
          @@ Alpha_context.Global_constants_storage.expand context expr)
      in
      Generator.Plain {workload = size; closure}
  end

  let () =
    Registration.register
      (module Global_constants_storage_expand_no_constant_branch)
end
