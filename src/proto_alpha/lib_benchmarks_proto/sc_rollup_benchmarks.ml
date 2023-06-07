(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.com>                        *)
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

let ns = Namespace.make Registration_helpers.ns "sc_rollup"

let fv s = Free_variable.of_namespace (ns s)

let ( -- ) min max : Base_samplers.range = {min; max}

(** This section contains preliminary definitions for building a pvm state from
    scratch. *)
module Pvm_state_generator = struct
  module Context = Tezos_context_memory.Context_binary

  module Wasm_context = struct
    type Tezos_tree_encoding.tree_instance += Tree of Context.tree

    module Tree = struct
      include Context.Tree

      type tree = Context.tree

      type t = Context.t

      type key = string list

      type value = bytes

      let select = function
        | Tree t -> t
        | _ -> raise Tezos_tree_encoding.Incorrect_tree_type

      let wrap t = Tree t
    end

    type tree = Context.tree

    type proof = Context.Proof.tree Context.Proof.t

    let verify_proof p f =
      Lwt.map Result.to_option (Context.verify_tree_proof p f)

    let produce_proof context tree step =
      let open Lwt_syntax in
      let* context = Context.add_tree context [] tree in
      let* _hash = Context.commit ~time:Time.Protocol.epoch context in
      let index = Context.index context in
      match Context.Tree.kinded_key tree with
      | Some k ->
          let* p = Context.produce_tree_proof index k step in
          return (Some p)
      | None -> return None

    let kinded_hash_to_state_hash = function
      | `Value hash | `Node hash ->
          Sc_rollup_repr.State_hash.context_hash_to_state_hash hash

    let proof_before proof =
      kinded_hash_to_state_hash proof.Context.Proof.before

    let proof_after proof = kinded_hash_to_state_hash proof.Context.Proof.after

    let proof_encoding =
      let module Proof_encoding =
        Tezos_context_merkle_proof_encoding.Merkle_proof_encoding
      in
      Proof_encoding.V2.Tree2.tree_proof_encoding
  end

  let make_transaction value text contract =
    let entrypoint = Entrypoint_repr.default in
    let destination : Contract_hash.t =
      Contract_hash.of_bytes_exn @@ Bytes.of_string contract
    in
    let open Tezos_micheline.Micheline in
    let open Michelson_v1_primitives in
    let unparsed_parameters =
      strip_locations
      @@ Prim
           ( 0,
             I_TICKET,
             [
               Prim
                 (0, I_PAIR, [Int (0, Z.of_int32 value); String (1, text)], []);
             ],
             [] )
    in
    Sc_rollup_outbox_message_repr.{unparsed_parameters; entrypoint; destination}

  let make_transactions ~rng_state ~max =
    let open Base_samplers in
    let n = sample_in_interval ~range:(0 -- max) rng_state in
    Stdlib.List.init n (fun _ ->
        let contract = uniform_string ~nbytes:20 rng_state in
        let value =
          Int32.of_int @@ sample_in_interval ~range:(-1000 -- 1000) rng_state
        in
        let text = string ~size:(0 -- 40) rng_state in
        make_transaction value text contract)

  let make_outbox_message ~nb_transactions ~rng_state =
    let transactions = make_transactions ~rng_state ~max:nb_transactions in
    Sc_rollup_outbox_message_repr.Atomic_transaction_batch {transactions}

  let dummy_context =
    let dummy = Context.init "/tmp" in
    Context.empty @@ Lwt_main.run dummy

  let empty_tree = Context.Tree.empty dummy_context

  (* Build a pvm state from scratch. *)
  let build_pvm_state rng_state ~nb_inbox_messages ~input_payload_size
      ~nb_output_buffer_levels ~output_buffer_size ~nb_transactions ~tree_depth
      ~tree_branching_factor =
    let open Lwt_result_syntax in
    let random_key () =
      Base_samplers.readable_ascii_string ~size:(5 -- 5) rng_state
    in
    (* [gen_tree] Generates a tree for the given depth and branching factor.
       This function is witten in CPS to avoid [stack-overflow] errors when
       branching factor is 1 and tree depth is big. *)
    let gen_tree () =
      let bottom_tree =
        let tree = empty_tree in
        let key = [random_key ()] in
        let value = Bytes.empty in
        Context.Tree.add tree key value
      in
      let rec gen_tree tree_depth kont =
        if tree_depth = 0 then kont bottom_tree
        else
          gen_tree
            (tree_depth - 1)
            (let rec kont' nb_subtrees acc_subtrees subtree =
               let*! subtree in
               let acc_subtrees = subtree :: acc_subtrees in
               let nb_subtrees = nb_subtrees + 1 in
               if nb_subtrees = tree_branching_factor then
                 let tree = empty_tree in
                 kont
                 @@ List.fold_left_s
                      (fun tree subtree ->
                        let key = [random_key ()] in
                        Context.Tree.add_tree tree key subtree)
                      tree
                      acc_subtrees
               else gen_tree (tree_depth - 1) (kont' nb_subtrees acc_subtrees)
             in
             kont' 0 [])
      in
      gen_tree tree_depth Fun.id
    in
    (* Add trees of junk data in the [durable] and [wasm] parts
       of the storage. *)
    let*! durable_junk_tree = gen_tree () in
    let*! wasm_junk_tree = gen_tree () in
    let tree = empty_tree in
    let*! tree = Context.Tree.add_tree tree ["durable"] durable_junk_tree in
    let*! tree = Context.Tree.add_tree tree ["wasm"] wasm_junk_tree in
    (* Create an output buffers and fill it with random batches of
       transactions. *)
    let open Tezos_webassembly_interpreter in
    let open Tezos_scoru_wasm in
    let module Index_Vector = Lazy_vector.Mutable.ZVector in
    let module Level_Map = Lazy_map.Mutable.LwtInt32Map in
    let output =
      Level_Map.create
        ~produce_value:(fun _ ->
          Lwt.return @@ Index_Vector.create (Z.of_int output_buffer_size))
        ()
    in
    let*! () =
      let open Sc_rollup_outbox_message_repr in
      List.iter_s
        (fun l ->
          let*! outbox = Level_Map.get (Int32.of_int l) output in
          Lwt.return
          @@ List.iter
               (fun i ->
                 let out = make_outbox_message ~nb_transactions ~rng_state in
                 let outbox_message =
                   Data_encoding.Binary.to_bytes_exn encoding out
                 in
                 Index_Vector.set (Z.of_int i) outbox_message outbox)
               Misc.(0 --> (output_buffer_size - 1)))
        Misc.(0 --> (nb_output_buffer_levels - 1))
    in
    let output = Output_buffer.Internal_for_tests.make output in
    (* Create the input buffer. *)
    let input = Index_Vector.create (Z.of_int nb_inbox_messages) in
    let make_input_message (message_counter : int) : Input_buffer.message =
      let open Base_samplers in
      let random_payload () =
        uniform_bytes ~nbytes:input_payload_size rng_state
      in
      {
        raw_level = Int32.of_int message_counter;
        message_counter = Z.of_int message_counter;
        payload = random_payload ();
      }
    in
    let () =
      List.iter
        (fun counter ->
          Index_Vector.set (Z.of_int counter) (make_input_message counter) input)
        Misc.(0 --> (nb_inbox_messages - 1))
    in
    (* Encode the buffers and update the state of the pvm. *)
    let buffers = Eval.{input; output} in
    let buffers_encoding = Wasm_pvm.durable_buffers_encoding in
    let module Tree_encoding_runner =
      Tezos_tree_encoding.Runner.Make (Wasm_context.Tree) in
    let*! tree =
      Tree_encoding_runner.encode
        (Tezos_tree_encoding.option buffers_encoding)
        (Some buffers)
        tree
    in
    Lwt.return (dummy_context, output, tree)

  let select_output ~output_buffer ~nb_output_buffer_levels ~output_buffer_size
      rng_state =
    let open Lwt_result_syntax in
    let open Base_samplers in
    (* Pick a level. *)
    let outbox_level =
      Int32.of_int
      @@ sample_in_interval
           ~range:(0 -- (nb_output_buffer_levels - 1))
           rng_state
    in
    (* Pick a message. *)
    let message_index =
      Z.of_int
      @@ sample_in_interval ~range:(0 -- (output_buffer_size - 1)) rng_state
    in
    let*! bytes_output_message =
      Tezos_webassembly_interpreter.Output_buffer.get_message
        output_buffer
        {outbox_level; message_index}
    in
    let message =
      Data_encoding.Binary.of_bytes_exn
        Sc_rollup_outbox_message_repr.encoding
        bytes_output_message
    in
    let*? outbox_level =
      Environment.wrap_tzresult @@ Raw_level_repr.of_int32 outbox_level
    in
    (* Produce an output proof for the picked message, and return the proof
       and its length. *)
    return Sc_rollup_PVM_sig.{outbox_level; message_index; message}
end

(** This benchmark estimates the cost of verifying an output proof for the
    Wasm PVM.
    The inferred cost model is [c1 + c2 * proof_length]. *)
module Sc_rollup_verify_output_proof_benchmark = struct
  open Pvm_state_generator
  module Full_Wasm =
    Sc_rollup_wasm.V2_0_0.Make (Environment.Wasm_2_0_0.Make) (Wasm_context)

  (* Benchmark starts here. *)

  let name = ns "Sc_rollup_verify_output_proof_benchmark"

  let info = "Estimating the cost of verifying an output proof"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["sc_rollup"]

  type config = {
    nb_inbox_messages : int;
    input_payload_size : int;
    nb_output_buffer_levels : int;
    output_buffer_size : int;
    nb_transactions : int;
    tree_depth : int;
    tree_branching_factor : int;
  }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {
             nb_inbox_messages;
             input_payload_size;
             nb_output_buffer_levels;
             output_buffer_size;
             nb_transactions;
             tree_depth;
             tree_branching_factor;
           } ->
        ( nb_inbox_messages,
          input_payload_size,
          nb_output_buffer_levels,
          output_buffer_size,
          nb_transactions,
          tree_depth,
          tree_branching_factor ))
      (fun ( nb_inbox_messages,
             input_payload_size,
             nb_output_buffer_levels,
             output_buffer_size,
             nb_transactions,
             tree_depth,
             tree_branching_factor ) ->
        {
          nb_inbox_messages : int;
          input_payload_size : int;
          nb_output_buffer_levels;
          output_buffer_size;
          nb_transactions;
          tree_depth;
          tree_branching_factor;
        })
      (obj7
         (req "nb_inbox_messages" int31)
         (req "input_payload_size" int31)
         (req "nb_output_buffer_levels" int31)
         (req "output_buffer_size" int31)
         (req "nb_transactions" int31)
         (req "tree_depth" int31)
         (req "tree_branching_factor" int31))

  (** The actual config used to generate the more accurate model in
      [sc_rollup_costs.ml] is :
      [{
        nb_inbox_messages = 1000;
        input_payload_size = 4096;
        nb_output_buffer_levels = 10_000;
        output_buffer_size = 100;
        nb_transactions = 50;
        tree_depth = 10;
        tree_branching_factor = 4;
      }]
      With the config above, the benchmark takes more than an hour. The default
      config is lighter and takes a few minutes.

      The table below shows benchmarking results for different tree depths and
      number of outbox levels of the pvm state. The branching factor of the
      generated "junk" trees in this benchmark is 4 (i.e for a depth of 10 the
      generated tree contains more than 1_000_000 nodes). A tree depth of more
      than 10 or a number of outbox levels of more than 10000 reaches the
      memory limit of a laptop with 16Gb of memory. All proofs generated by
      these benchmarks are below 10kb.

      +-----------+---------------+-------------------------+-----------------+
      | Junk tree | Number of     | Inferred model          | Gas cost for a  |
      | depth     | outbox levels |                         | proof 10kb long |
      +-----------+---------------+-------------------------+-----------------+
      | 5         | 1000          |  7.907*size + 99291.292 | 178361          |
      +-----------+---------------+-------------------------+-----------------+
      | 6         | 2000          |  9.510*size + 99516.012 | 194616          |
      +-----------+---------------+-------------------------+-----------------+
      | 7         | 4000          | 11.383*size + 95445.175 | 209275          |
      +-----------+---------------+-------------------------+-----------------+
      | 8         | 6000          | 11.316*size + 100760.29 | 213920          |
      +-----------+---------------+-------------------------+-----------------+
      | 9         | 8000          | 11.227*size + 98748.490 | 211018          |
      +-----------+---------------+-------------------------+-----------------+
      | 10        | 10000         | 11.680*size + 98707.082 | 215507          |
      +-----------+---------------+-------------------------+-----------------+

      The [nb_transactions] parameter is the max number of transactions in an
      outbox message, it is set at 50 because a message with 50 transactions
      approches the max size of an outbox message. Hence, this allows to
      benchmark for various proof lengths. The [nb_inbox_messages] parameter is
      set to correspond to the max number of messages in an inbox. And the
      [input_payload_size] parameter is set to the biggest possible size of an
      input message. These two parameters impact the number of nodes in the pvm
      state and are stored in the "input" part of the state. We add data in this
      "input" part because of its proximity with the "output" part in the pvm
      state. *)
  let default_config =
    {
      nb_inbox_messages = 1000;
      input_payload_size = 4096;
      nb_output_buffer_levels = 10_000;
      output_buffer_size = 100;
      nb_transactions = 50;
      tree_depth = 10;
      tree_branching_factor = 4;
    }

  type workload = {proof_length : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {proof_length} -> proof_length)
      (fun proof_length -> {proof_length})
      (obj1 (req "proof_length" int31))

  let workload_to_vector {proof_length} =
    Sparse_vec.String.of_list [("proof_length", float_of_int proof_length)]

  let model =
    let open Benchmarks_proto in
    Model.make
      ~conv:(fun {proof_length} -> (proof_length, ()))
      ~model:Model.affine

  let pvm_state = ref None

  let create_benchmark ~rng_state conf =
    let nb_output_buffer_levels = conf.nb_output_buffer_levels in
    let output_buffer_size = conf.output_buffer_size in
    let prepare_benchmark_scenario () =
      let open Lwt_result_syntax in
      (* Build [pvm_state] and save it to be used for all benchmarks. The state
         is large enough for each benchmark to be relatively random. *)
      let*! context, output_buffer, initial_tree =
        match !pvm_state with
        | None ->
            let res =
              build_pvm_state
                rng_state
                ~nb_inbox_messages:conf.nb_inbox_messages
                ~input_payload_size:conf.input_payload_size
                ~nb_output_buffer_levels:conf.nb_output_buffer_levels
                ~output_buffer_size:conf.output_buffer_size
                ~nb_transactions:conf.nb_transactions
                ~tree_depth:conf.tree_depth
                ~tree_branching_factor:conf.tree_branching_factor
            in
            pvm_state := Some res ;
            res
        | Some pvm_state -> pvm_state
      in
      (* Select an output. *)
      let* output =
        select_output
          ~output_buffer
          ~nb_output_buffer_levels
          ~output_buffer_size
          rng_state
      in
      (* produce an output proof, and also return the length of its encoding.*)
      let*! pf = Full_Wasm.produce_output_proof context initial_tree output in
      match pf with
      | Ok proof ->
          let proof_length =
            Data_encoding.Binary.length Full_Wasm.output_proof_encoding proof
          in
          return (proof, proof_length)
      | Error _ -> assert false
    in

    let output_proof, proof_length =
      match Lwt_main.run @@ prepare_benchmark_scenario () with
      | Ok (proof, len) -> (proof, len)
      | Error _ -> assert false
    in
    let workload = {proof_length} in

    let closure () =
      ignore (Lwt_main.run @@ Full_Wasm.verify_output_proof output_proof)
    in
    Generator.Plain {workload; closure}
end

(** This benchmark estimates the cost of verifying an output proof for the
    Wasm PVM.
    The inferred cost model is [c1 + c2 * proof_length]. *)
module Sc_rollup_deserialize_output_proof_benchmark = struct
  open Pvm_state_generator
  module Full_Wasm =
    Sc_rollup_wasm.V2_0_0.Make (Environment.Wasm_2_0_0.Make) (Wasm_context)

  (* Benchmark starts here. *)

  let name = ns "Sc_rollup_deserialize_output_proof_benchmark"

  let info = "Estimating the cost of deserializing an output proof"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["sc_rollup"]

  type config = {
    nb_output_buffer_levels : int;
    output_buffer_size : int;
    nb_transactions : int;
    tree_depth : int;
  }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {
             nb_output_buffer_levels;
             output_buffer_size;
             nb_transactions;
             tree_depth;
           } ->
        ( nb_output_buffer_levels,
          output_buffer_size,
          nb_transactions,
          tree_depth ))
      (fun ( nb_output_buffer_levels,
             output_buffer_size,
             nb_transactions,
             tree_depth ) ->
        {
          nb_output_buffer_levels;
          output_buffer_size;
          nb_transactions;
          tree_depth;
        })
      (obj4
         (req "nb_output_buffer_levels" int31)
         (req "output_buffer_size" int31)
         (req "nb_transactions" int31)
         (req "tree_depth" int31))

  let default_config =
    {
      nb_output_buffer_levels = 10_000;
      output_buffer_size = 100;
      nb_transactions = 50;
      tree_depth = 10;
    }

  type workload = {proof_length : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {proof_length} -> proof_length)
      (fun proof_length -> {proof_length})
      (obj1 (req "proof_length" int31))

  let workload_to_vector {proof_length} =
    Sparse_vec.String.of_list [("proof_length", float_of_int proof_length)]

  let model =
    let open Benchmarks_proto in
    Model.make
      ~conv:(fun {proof_length} -> (proof_length, ()))
      ~model:Model.affine

  let pvm_state = ref None

  let create_benchmark ~rng_state conf =
    let prepared_benchmark_scenario =
      let nb_output_buffer_levels = conf.nb_output_buffer_levels in
      let output_buffer_size = conf.output_buffer_size in
      let tree_depth = conf.tree_depth in
      let open Lwt_result_syntax in
      (* Build [pvm_state] and save it to be used for all benchmarks. The state
         is large enough for each benchmark to be relatively random. *)
      let*! context, output_buffer, initial_tree =
        match !pvm_state with
        | Some pvm_state -> pvm_state
        | None ->
            let res =
              build_pvm_state
                rng_state
                ~nb_inbox_messages:0
                ~input_payload_size:0
                ~nb_output_buffer_levels
                ~output_buffer_size
                ~nb_transactions:conf.nb_transactions
                ~tree_depth
                ~tree_branching_factor:2
            in
            pvm_state := Some res ;
            res
      in
      (* Select an output. *)
      let* output =
        select_output
          ~output_buffer
          ~nb_output_buffer_levels
          ~output_buffer_size
          rng_state
      in
      (* Produce an output proof, and return its encoding and the length of the
         encoding. *)
      let*! pf = Full_Wasm.produce_output_proof context initial_tree output in
      match pf with
      | Ok proof ->
          let encoded_proof =
            Data_encoding.Binary.to_bytes_exn
              Full_Wasm.output_proof_encoding
              proof
          in
          let proof_length = Bytes.length encoded_proof in
          return (encoded_proof, proof_length)
      | Error _ -> assert false
    in

    let encoded_proof, proof_length =
      prepared_benchmark_scenario |> Lwt_main.run
      |> WithExceptions.Result.get_ok ~loc:__LOC__
    in
    let workload = {proof_length} in

    let closure () =
      ignore
        (Data_encoding.Binary.of_bytes_exn
           Full_Wasm.output_proof_encoding
           encoded_proof)
    in
    Generator.Plain {workload; closure}
end

let () =
  Benchmarks_proto.Registration.register
    (module Sc_rollup_verify_output_proof_benchmark)

let () =
  Benchmarks_proto.Registration.register
    (module Sc_rollup_deserialize_output_proof_benchmark)
