(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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
open Benchmarks_shell
module Context = Tezos_protocol_environment.Context
module Shell_monad = Tezos_error_monad.Error_monad
module Key_map = Io_helpers.Key_map

let ns = Namespace.make Shell_namespace.ns "io"

let fv s = Free_variable.of_namespace (ns s)

let read_model =
  Model.bilinear_affine
    ~name:(ns "read_model")
    ~intercept:(fv "read_latency")
    ~coeff1:(fv "depth")
    ~coeff2:(fv "storage_bytes")

let write_model =
  Model.bilinear_affine
    ~name:(ns "write_model")
    ~intercept:(fv "write_latency")
    ~coeff1:(fv "keys_written")
    ~coeff2:(fv "storage_bytes")

module Helpers = struct
  (* Samples keys in an alphabet of [card] elements. *)
  let sample_key ~card =
    assert (card > 0) ;
    let i = string_of_int (Random.int card) in
    "key" ^ i

  let make_key_sampler ~card =
    assert (card > 0) ;
    let suffixes = Array.init card string_of_int in
    fun () ->
      let i = Random.int card in
      "key" ^ suffixes.(i)

  let random_key rng_state ~card ~depth =
    let depth = Base_samplers.sample_in_interval rng_state ~range:depth in
    let rec loop depth acc =
      if depth = 0 then List.rev acc
      else
        let key = sample_key ~card in
        loop (depth - 1) (key :: acc)
    in
    loop depth []

  (* Initializes a context by setting random bytes for each key in the
     given [key_set]. *)
  let random_contents rng_state base_dir index context key_set commit_batch_size
      =
    let open Lwt_syntax in
    let* index, context, _ =
      Key_map.fold_lwt
        (fun path size (index, context, current_commit_batch_size) ->
          let* context =
            Io_helpers.initialize_key rng_state context path size
          in
          if current_commit_batch_size < commit_batch_size then
            Lwt.return (index, context, current_commit_batch_size + 1)
          else
            (* save and proceed with fresh diff *)
            let* context, index =
              Io_helpers.commit_and_reload base_dir index context
            in
            Lwt.return (index, context, 0))
        key_set
        (index, context, 0)
    in
    Io_helpers.commit_and_reload base_dir index context

  let random_key_set rng_state ~depth ~key_card ~insertions =
    let rec loop remaining acc =
      if remaining = 0 then acc
      else
        let key = random_key rng_state ~card:key_card ~depth in
        match Key_map.does_not_collide key acc with
        | `Key_exists | `Key_has_prefix | `Key_has_suffix -> loop remaining acc
        | `Key_does_not_collide ->
            let size = 1000 in
            let acc = Key_map.insert key size acc in
            loop (remaining - 1) acc
    in
    let initial =
      let key = random_key rng_state ~card:key_card ~depth in
      let size = 1000 in
      Key_map.insert key size Key_map.empty
    in
    loop insertions initial

  let prepare_random_context rng_state base_dir commit_batch_size keys =
    let context_hash =
      Io_helpers.assert_ok ~msg:"Io_helpers.prepare_empty_context"
      @@ Lwt_main.run (Io_helpers.prepare_empty_context base_dir)
    in
    let context, index =
      Io_helpers.load_context_from_disk base_dir context_hash
    in
    Lwt_main.run
      (let open Lwt_syntax in
      let* context, index =
        random_contents rng_state base_dir index context keys commit_batch_size
      in
      Io_helpers.commit_and_reload base_dir index context)
end

module Context_size_dependent_shared = struct
  (* ----------------------------------------------------------------------- *)
  (* Config *)

  open Base_samplers

  type config = {
    depth : range;
    storage_chunk_bytes : int;
    storage_chunks : range;
    insertions : range;
    key_card : int;
    commit_batch_size : int;
    temp_dir : string option;
  }

  let default_config =
    {
      depth = {min = 10; max = 1000};
      storage_chunk_bytes = 1000;
      storage_chunks = {min = 10; max = 1000};
      insertions = {min = 100; max = 65536};
      key_card = 16;
      commit_batch_size = 10_000;
      temp_dir = None;
    }

  let config_encoding =
    let open Data_encoding in
    let int = int31 in
    conv
      (fun {
             depth;
             storage_chunk_bytes;
             storage_chunks;
             insertions;
             key_card;
             commit_batch_size;
             temp_dir;
           } ->
        ( depth,
          storage_chunk_bytes,
          storage_chunks,
          insertions,
          key_card,
          commit_batch_size,
          temp_dir ))
      (fun ( depth,
             storage_chunk_bytes,
             storage_chunks,
             insertions,
             key_card,
             commit_batch_size,
             temp_dir ) ->
        {
          depth;
          storage_chunk_bytes;
          storage_chunks;
          insertions;
          key_card;
          commit_batch_size;
          temp_dir;
        })
      (obj7
         (req "depth" range_encoding)
         (req "storage_chunk_bytes" int)
         (req "storage_chunks" range_encoding)
         (req "insertions" range_encoding)
         (req "key_card" int)
         (req "commit_batch_size" int)
         (opt "temp_dir" string))

  let rec sample_accessed_key rng_state cfg keys =
    let key =
      Helpers.random_key rng_state ~card:cfg.key_card ~depth:cfg.depth
    in
    match Key_map.does_not_collide key keys with
    | `Key_exists | `Key_has_prefix | `Key_has_suffix ->
        sample_accessed_key rng_state cfg keys
    | `Key_does_not_collide ->
        let size =
          Base_samplers.sample_in_interval rng_state ~range:cfg.storage_chunks
          * cfg.storage_chunk_bytes
        in
        (key, size)

  type workload =
    | Random_context_random_access of {
        depth : int;
        storage_bytes : int;
        context_size : int;
      }

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun (Random_context_random_access {depth; storage_bytes; context_size}) ->
        (depth, storage_bytes, context_size))
      (fun (depth, storage_bytes, context_size) ->
        Random_context_random_access {depth; storage_bytes; context_size})
      (tup3 int31 int31 int31)

  let workload_to_vector = function
    | Random_context_random_access {depth; storage_bytes; context_size} ->
        let keys =
          [
            ("depth", float_of_int depth);
            ("storage_bytes", float_of_int storage_bytes);
            ("context_size", float_of_int context_size);
          ]
        in
        Sparse_vec.String.of_list keys

  let read_access =
    Model.make
      ~conv:(function
        | Random_context_random_access {depth; storage_bytes; _} ->
            (depth, (storage_bytes, ())))
      ~model:read_model

  let models = [("io_read", read_access)]
end

module Context_size_dependent_read_bench : Benchmark.S = struct
  include Context_size_dependent_shared

  (* ----------------------------------------------------------------------- *)
  (* Benchmark def *)

  let name = ns "CONTEXT_SIZE_DEPENDENT_READ"

  let info =
    "Benchmarking the read accesses with contexts of various sizes (with fixed \
     storage size except for the accessed key)"

  let tags = ["io"]

  let module_filename = __FILE__

  let generated_code_destination = None

  include Context_size_dependent_shared

  let make_bench rng_state cfg () =
    let insertions =
      Base_samplers.sample_in_interval rng_state ~range:cfg.insertions
    in
    let keys =
      Helpers.random_key_set
        rng_state
        ~depth:cfg.depth
        ~key_card:cfg.key_card
        ~insertions
    in
    let random_key, value_size = sample_accessed_key rng_state cfg keys in
    let keys = Key_map.insert random_key value_size keys in
    Format.eprintf "preparing bench: insertions = %d@." insertions ;
    let closure context =
      match
        Lwt_main.run
          (Tezos_protocol_environment.Context.find context random_key)
      with
      | Some _ -> ()
      | None ->
          let s = String.concat "/" random_key in
          Format.eprintf "key %s not found@." s ;
          exit 1
    in
    let workload =
      Random_context_random_access
        {
          depth = List.length random_key;
          storage_bytes = value_size;
          (* context_size !=  insertions, but there should
             be a linear relationship. *)
          context_size = insertions;
        }
    in
    let with_context f =
      let base_dir =
        Filename.temp_file ?temp_dir:cfg.temp_dir (Namespace.basename name) ""
      in
      Io_helpers.prepare_base_dir base_dir ;
      let context, index =
        Helpers.prepare_random_context
          rng_state
          base_dir
          cfg.commit_batch_size
          keys
      in
      let finalizer () =
        Gc.compact () ;
        Lwt_main.run
          (let open Lwt_syntax in
          let* () = Tezos_context.Context.close index in
          Tezos_stdlib_unix.Lwt_utils_unix.remove_dir base_dir)
      in
      let result =
        try f context
        with _ ->
          finalizer () ;
          exit 1
      in
      finalizer () ;
      result
    in
    Generator.With_context {workload; closure; with_context}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration.register (module Context_size_dependent_read_bench)

module Context_size_dependent_write_bench : Benchmark.S = struct
  include Context_size_dependent_shared

  (* ----------------------------------------------------------------------- *)
  (* Benchmark def *)

  let name = ns "CONTEXT_SIZE_DEPENDENT_WRITE"

  let info =
    "Benchmarking the write accesses with contexts of various sizes (with \
     fixed storage size except for the written key)"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["io"]

  let write_storage context key bytes =
    Lwt_main.run (Tezos_protocol_environment.Context.add context key bytes)

  let make_bench rng_state cfg () =
    let insertions =
      Base_samplers.sample_in_interval rng_state ~range:cfg.insertions
    in
    let keys =
      Helpers.random_key_set
        rng_state
        ~depth:cfg.depth
        ~key_card:cfg.key_card
        ~insertions
    in
    let random_key, value_size = sample_accessed_key rng_state cfg keys in
    Format.eprintf "preparing bench: insertions = %d@." insertions ;
    let closure context =
      Lwt_main.run
        (let open Lwt_syntax in
        let* _ = Io_helpers.commit context in
        Lwt.return_unit)
    in
    let workload =
      Random_context_random_access
        {
          depth = List.length random_key;
          storage_bytes = value_size;
          (* context_size !=  insertions, but there should
             be a linear relationship. *)
          context_size = insertions;
        }
    in
    let with_context f =
      let base_dir =
        Filename.temp_file ?temp_dir:cfg.temp_dir (Namespace.basename name) ""
      in
      Io_helpers.prepare_base_dir base_dir ;
      let context, index =
        Helpers.prepare_random_context
          rng_state
          base_dir
          cfg.commit_batch_size
          keys
      in
      let bytes = Base_samplers.uniform_bytes rng_state ~nbytes:value_size in
      let context = write_storage context random_key bytes in
      let finalizer () =
        Gc.compact () ;
        Lwt_main.run
          (let open Lwt_syntax in
          let* () = Tezos_context.Context.close index in
          Tezos_stdlib_unix.Lwt_utils_unix.remove_dir base_dir)
      in
      let result =
        try f context
        with _ ->
          finalizer () ;
          exit 1
      in
      finalizer () ;
      result
    in
    Generator.With_context {workload; closure; with_context}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration.register (module Context_size_dependent_write_bench)

module Irmin_pack_shared = struct
  open Base_samplers

  type config = {
    depth : range;
    insertions : range;
    key_card : int;
    irmin_pack_max_width : int;
    storage_chunk_bytes : int;
    storage_chunks : range;
    default_storage_bytes : int;
    commit_batch_size : int;
    temp_dir : string option;
  }

  let config_encoding =
    let open Data_encoding in
    let int = int31 in
    conv
      (fun {
             depth;
             insertions;
             key_card;
             irmin_pack_max_width;
             storage_chunk_bytes;
             storage_chunks;
             default_storage_bytes;
             commit_batch_size;
             temp_dir;
           } ->
        ( depth,
          insertions,
          key_card,
          irmin_pack_max_width,
          storage_chunk_bytes,
          storage_chunks,
          default_storage_bytes,
          commit_batch_size,
          temp_dir ))
      (fun ( depth,
             insertions,
             key_card,
             irmin_pack_max_width,
             storage_chunk_bytes,
             storage_chunks,
             default_storage_bytes,
             commit_batch_size,
             temp_dir ) ->
        {
          depth;
          insertions;
          key_card;
          irmin_pack_max_width;
          storage_chunk_bytes;
          storage_chunks;
          default_storage_bytes;
          commit_batch_size;
          temp_dir;
        })
      (obj9
         (req "depth" range_encoding)
         (req "insertions" range_encoding)
         (req "key_card" int)
         (req "irmin_pack_max_width" int)
         (req "storage_chunk_bytes" int)
         (req "storage_chunks" range_encoding)
         (req "default_storage_bytes" int)
         (req "commit_batch_size" int)
         (opt "temp_dir" string))

  let default_config =
    {
      depth = {min = 3; max = 30};
      insertions = {min = 100; max = 65536};
      key_card = 64;
      irmin_pack_max_width = 8192;
      storage_chunk_bytes = 1000;
      storage_chunks = {min = 1; max = 50};
      default_storage_bytes = 1000;
      commit_batch_size = 10_000;
      temp_dir = None;
    }

  let rec sample_irmin_directory_key rng_state (cfg : config) keys =
    let key =
      Helpers.random_key rng_state ~card:cfg.key_card ~depth:cfg.depth
    in
    match Key_map.does_not_collide key keys with
    | `Key_exists | `Key_has_prefix | `Key_has_suffix ->
        sample_irmin_directory_key rng_state cfg keys
    | `Key_does_not_collide -> key

  let irmin_pack_key i = "pack_" ^ string_of_int i

  let sample_irmin_directory rng_state ~cfg ~key_set =
    if cfg.irmin_pack_max_width < 256 then
      Stdlib.failwith
        "Irmin_pack_read_bench: irmin_pack_max_width < 256, invalid \
         configuration"
    else
      let prefix = sample_irmin_directory_key rng_state cfg key_set in
      let dir_width =
        Base_samplers.sample_in_interval
          rng_state
          ~range:{min = 256; max = cfg.irmin_pack_max_width}
      in
      let directories =
        Array.init dir_width (fun i -> prefix @ [irmin_pack_key i])
      in
      (prefix, directories)
end

module Irmin_pack_read_bench : Benchmark.S = struct
  include Irmin_pack_shared

  let prepare_irmin_directory rng_state ~cfg ~key_set =
    if cfg.irmin_pack_max_width < 256 then
      Stdlib.failwith
        "Irmin_pack_read_bench: irmin_pack_max_width < 256, invalid \
         configuration"
    else
      let _prefix, directories =
        sample_irmin_directory rng_state ~cfg ~key_set
      in
      let dir_width = Array.length directories in
      let target_index = Random.int dir_width in
      let target_key = directories.(target_index) in
      let value_size =
        Base_samplers.sample_in_interval rng_state ~range:cfg.storage_chunks
        * cfg.storage_chunk_bytes
      in
      let key_set =
        let acc = ref key_set in
        for index = 0 to Array.length directories - 1 do
          let key = directories.(index) in
          if index = target_index then acc := Key_map.insert key value_size !acc
          else acc := Key_map.insert key cfg.default_storage_bytes !acc
        done ;
        !acc
      in
      (target_key, value_size, key_set, directories)

  let name = ns "IRMIN_PACK_READ"

  let info = "Benchmarking read accesses in irmin-pack directories"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["io"]

  type workload =
    | Irmin_pack_read of {
        depth : int;
        irmin_width : int;
        storage_bytes : int;
        context_size : int;
      }

  let workload_to_vector = function
    | Irmin_pack_read {depth; irmin_width; storage_bytes; context_size} ->
        let keys =
          [
            ("depth", float_of_int depth);
            ("irmin_width", float_of_int irmin_width);
            ("storage_bytes", float_of_int storage_bytes);
            ("context_size", float_of_int context_size);
          ]
        in
        Sparse_vec.String.of_list keys

  let read_access =
    Model.make
      ~conv:(function
        | Irmin_pack_read {depth; storage_bytes; _} ->
            (depth, (storage_bytes, ())))
      ~model:read_model

  let models = [("io_read", read_access)]

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun (Irmin_pack_read {depth; irmin_width; storage_bytes; context_size}) ->
        (depth, irmin_width, storage_bytes, context_size))
      (fun (depth, irmin_width, storage_bytes, context_size) ->
        Irmin_pack_read {depth; irmin_width; storage_bytes; context_size})
      (tup4 int31 int31 int31 int31)

  let make_bench rng_state (cfg : config) () =
    let insertions =
      Base_samplers.sample_in_interval rng_state ~range:cfg.insertions
    in
    let keys =
      Helpers.random_key_set
        rng_state
        ~depth:cfg.depth
        ~key_card:cfg.key_card
        ~insertions
    in
    let target_key, value_size, keys, irmin_pack_paths =
      prepare_irmin_directory rng_state ~cfg ~key_set:keys
    in
    let irmin_width = Array.length irmin_pack_paths in
    let stats = Io_stats.tree_statistics keys in
    Format.eprintf
      "preparing bench: insertions = %d, stats = %a@."
      (insertions + irmin_width)
      Io_stats.pp
      stats ;
    let closure context =
      match Lwt_main.run (Context.find context target_key) with
      | Some _ -> ()
      | None ->
          let s = String.concat "/" target_key in
          Format.eprintf "key %s not found@." s ;
          exit 1
    in
    let workload =
      Irmin_pack_read
        {
          depth = List.length target_key;
          irmin_width;
          storage_bytes = value_size;
          context_size = stats.total;
        }
    in
    let with_context f =
      let base_dir =
        Filename.temp_file ?temp_dir:cfg.temp_dir (Namespace.basename name) ""
      in
      Io_helpers.prepare_base_dir base_dir ;
      let context, index =
        Helpers.prepare_random_context
          rng_state
          base_dir
          cfg.commit_batch_size
          keys
      in
      let finalizer () =
        Gc.compact () ;
        Lwt_main.run
          (let open Lwt_syntax in
          let* () = Tezos_context.Context.close index in
          Tezos_stdlib_unix.Lwt_utils_unix.remove_dir base_dir)
      in
      let result =
        try f context
        with _ ->
          finalizer () ;
          exit 1
      in
      finalizer () ;
      result
    in
    Generator.With_context {workload; closure; with_context}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration.register (module Irmin_pack_read_bench)

module Irmin_pack_write_bench : Benchmark.S = struct
  include Irmin_pack_shared

  let prepare_irmin_directory rng_state ~cfg ~key_set ~bench_init =
    if cfg.irmin_pack_max_width < 256 then
      Stdlib.failwith
        "Irmin_pack_read_bench: irmin_pack_max_width < 256, invalid \
         configuration"
    else
      let _prefix, directories =
        sample_irmin_directory rng_state ~cfg ~key_set
      in
      let total_keys_in_pack = Array.length directories in
      let number_of_keys_written = Random.int total_keys_in_pack in
      let keys_written_to, keys_not_written_to =
        Io_helpers.sample_without_replacement
          number_of_keys_written
          (Array.to_list directories)
      in
      let key_set =
        (* Initialize keys not written to with random bytes of fixed size *)
        List.fold_left
          (fun key_set key ->
            Key_map.insert key cfg.default_storage_bytes key_set)
          key_set
          keys_not_written_to
      in
      let key_set =
        if bench_init then
          (* If we wish to benchmark writing to fresh keys, we should not
             add the keys written to in the initial context *)
          key_set
        else
          (* Else, if we wish to benchmark overwriting existing keys,
             we initialize them to bytes of fixed size. *)
          List.fold_left
            (fun key_set key ->
              Key_map.insert key cfg.default_storage_bytes key_set)
            key_set
            keys_written_to
      in
      ( number_of_keys_written,
        keys_written_to,
        keys_not_written_to,
        key_set,
        total_keys_in_pack )

  let name = ns "IRMIN_PACK_WRITE"

  let info = "Benchmarking write accesses in irmin-pack directories"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["io"]

  type workload =
    | Irmin_pack_write of {
        keys_written : int;
        irmin_width : int;
        storage_bytes : int;
        context_size : int;
      }

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun (Irmin_pack_write
             {keys_written; irmin_width; storage_bytes; context_size}) ->
        (keys_written, irmin_width, storage_bytes, context_size))
      (fun (keys_written, irmin_width, storage_bytes, context_size) ->
        Irmin_pack_write
          {keys_written; irmin_width; storage_bytes; context_size})
      (tup4 int31 int31 int31 int31)

  let workload_to_vector = function
    | Irmin_pack_write {keys_written; irmin_width; storage_bytes; context_size}
      ->
        let keys =
          [
            ("keys_written", float_of_int keys_written);
            ("irmin_width", float_of_int irmin_width);
            ("storage_bytes", float_of_int storage_bytes);
            ("context_size", float_of_int context_size);
          ]
        in
        Sparse_vec.String.of_list keys

  let write_access =
    Model.make
      ~conv:(function
        | Irmin_pack_write {keys_written; storage_bytes; _} ->
            (keys_written, (storage_bytes, ())))
      ~model:write_model

  let models = [("io_write", write_access)]

  let write_storage context key bytes =
    Lwt_main.run (Context.add context key bytes)

  let make_bench rng_state (cfg : config) () =
    let insertions =
      Base_samplers.sample_in_interval rng_state ~range:cfg.insertions
    in
    let keys =
      Helpers.random_key_set
        rng_state
        ~depth:cfg.depth
        ~key_card:cfg.key_card
        ~insertions
    in
    let ( number_of_keys_written,
          keys_written_to,
          _keys_not_written_to,
          key_set,
          total_keys_in_pack ) =
      prepare_irmin_directory rng_state ~cfg ~key_set:keys ~bench_init:true
    in
    let stats = Io_stats.tree_statistics keys in
    Format.eprintf
      "preparing bench: insertions = %d, stats = %a@."
      (insertions + total_keys_in_pack)
      Io_stats.pp
      stats ;
    let base_dir =
      Filename.temp_file ?temp_dir:cfg.temp_dir (Namespace.basename name) ""
    in
    let value_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.storage_chunks
      * cfg.storage_chunk_bytes
    in
    let with_context f =
      Io_helpers.prepare_base_dir base_dir ;
      let context, index =
        Helpers.prepare_random_context
          rng_state
          base_dir
          cfg.commit_batch_size
          key_set
      in
      let context =
        List.fold_left
          (fun context key ->
            let bytes =
              Base_samplers.uniform_bytes rng_state ~nbytes:value_size
            in
            write_storage context key bytes)
          context
          keys_written_to
      in
      let finalizer () =
        Gc.compact () ;
        Lwt_main.run
          (let open Lwt_syntax in
          let* () = Tezos_context.Context.close index in
          Tezos_stdlib_unix.Lwt_utils_unix.remove_dir base_dir)
      in
      let result =
        try f context
        with _ ->
          finalizer () ;
          exit 1
      in
      finalizer () ;
      result
    in
    let closure context =
      Lwt_main.run
        (let open Lwt_syntax in
        let* _ = Io_helpers.commit context in
        Lwt.return_unit)
    in
    let workload =
      Irmin_pack_write
        {
          keys_written = number_of_keys_written;
          irmin_width = total_keys_in_pack;
          storage_bytes = value_size;
          context_size = stats.total;
        }
    in
    Generator.With_context {workload; closure; with_context}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration.register (module Irmin_pack_write_bench)

module Read_random_key_bench : Benchmark.S = struct
  type config = {
    existing_context : string * Context_hash.t;
    subdirectory : string list;
  }

  let default_config =
    {
      existing_context = ("/no/such/directory", Context_hash.zero);
      subdirectory = ["no"; "such"; "key"];
    }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {existing_context; subdirectory} -> (existing_context, subdirectory))
      (fun (existing_context, subdirectory) -> {existing_context; subdirectory})
      (obj2
         (req "existing_context" (tup2 string Context_hash.encoding))
         (req "subdirectory" (list string)))

  let name = ns "READ_RANDOM_KEY"

  let info = "Benchmarking random read accesses in a subdirectory"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["io"]

  type workload = Read_random_key of {depth : int; storage_bytes : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (function
        | Read_random_key {depth; storage_bytes} -> (depth, storage_bytes))
      (fun (depth, storage_bytes) -> Read_random_key {depth; storage_bytes})
      (tup2 int31 int31)

  let workload_to_vector = function
    | Read_random_key {depth; storage_bytes} ->
        let keys =
          [
            ("depth", float_of_int depth);
            ("storage_bytes", float_of_int storage_bytes);
          ]
        in
        Sparse_vec.String.of_list keys

  let read_access =
    Model.make
      ~conv:(function
        | Read_random_key {depth; storage_bytes} -> (depth, (storage_bytes, ())))
      ~model:read_model

  let models = [("io_read", read_access)]

  let make_bench rng_state config keys () =
    let card = Array.length keys in
    assert (card > 0) ;
    let key, value_size = keys.(Random.State.int rng_state card) in
    let with_context f =
      let context, index =
        let base_dir, context_hash = config.existing_context in
        Io_helpers.load_context_from_disk base_dir context_hash
      in
      let finalizer () =
        Gc.compact () ;
        Lwt_main.run (Tezos_context.Context.close index)
      in
      let result =
        try f context
        with _ ->
          finalizer () ;
          exit 1
      in
      finalizer () ;
      result
    in
    let closure context =
      match Lwt_main.run (Context.find context key) with
      | Some _ -> ()
      | None ->
          let s = String.concat "/" key in
          Format.eprintf "key %s not found@." s ;
          exit 1
    in
    let workload =
      Read_random_key {depth = List.length key; storage_bytes = value_size}
    in
    Generator.With_context {workload; closure; with_context}

  let create_benchmarks ~rng_state ~bench_num config =
    let base_dir, context_hash = config.existing_context in
    let tree =
      Io_helpers.with_context ~base_dir ~context_hash (fun context ->
          Io_stats.load_tree context config.subdirectory)
    in
    let keys = Array.of_seq (Io_helpers.Key_map.to_seq tree) in
    List.repeat bench_num (make_bench rng_state config keys)
end

let () = Registration.register (module Read_random_key_bench)

module Write_random_keys_bench : Benchmark.S = struct
  open Base_samplers

  type config = {
    existing_context : string * Context_hash.t;
    storage_chunk_bytes : int;
    storage_chunks : range;
    max_written_keys : int;
    temp_dir : string option;
    subdirectory : string list;
  }

  let default_config =
    {
      existing_context = ("/no/such/directory", Context_hash.zero);
      storage_chunk_bytes = 1000;
      storage_chunks = {min = 1; max = 1000};
      max_written_keys = 10_000;
      temp_dir = None;
      subdirectory = ["no"; "such"; "key"];
    }

  let config_encoding =
    let open Data_encoding in
    let int = int31 in
    conv
      (fun {
             existing_context;
             storage_chunk_bytes;
             storage_chunks;
             max_written_keys;
             temp_dir;
             subdirectory;
           } ->
        ( existing_context,
          storage_chunk_bytes,
          storage_chunks,
          max_written_keys,
          temp_dir,
          subdirectory ))
      (fun ( existing_context,
             storage_chunk_bytes,
             storage_chunks,
             max_written_keys,
             temp_dir,
             subdirectory ) ->
        {
          existing_context;
          storage_chunk_bytes;
          storage_chunks;
          max_written_keys;
          temp_dir;
          subdirectory;
        })
      (obj6
         (req "existing_context" (tup2 string Context_hash.encoding))
         (req "storage_chunk_bytes" int)
         (req "storage_chunks" range_encoding)
         (req "max_written_keys" int)
         (req "temp_dir" (option string))
         (req "subdirectory" (list string)))

  let name = ns "WRITE_RANDOM_KEYS"

  let info = "Benchmarking random read accesses in a subdirectory"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["io"]

  type workload =
    | Write_random_keys of {keys_written : int; storage_bytes : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (function
        | Write_random_keys {keys_written; storage_bytes} ->
            (keys_written, storage_bytes))
      (fun (keys_written, storage_bytes) ->
        Write_random_keys {keys_written; storage_bytes})
      (tup2 int31 int31)

  let workload_to_vector = function
    | Write_random_keys {keys_written; storage_bytes} ->
        let keys =
          [
            ("keys_written", float_of_int keys_written);
            ("storage_bytes", float_of_int storage_bytes);
          ]
        in
        Sparse_vec.String.of_list keys

  let write_access =
    Model.make
      ~conv:(function
        | Write_random_keys {keys_written; storage_bytes; _} ->
            (keys_written, (storage_bytes, ())))
      ~model:write_model

  let models = [("io_write", write_access)]

  let write_storage context key bytes =
    Lwt_main.run (Context.add context key bytes)

  let make_bench rng_state (cfg : config) (keys : (string list * int) Seq.t) ()
      =
    let keys = List.of_seq keys in
    let total_keys_in_directory = List.length keys in
    let number_of_keys_written =
      min
        total_keys_in_directory
        (Random.State.int rng_state cfg.max_written_keys)
    in
    let keys_written_to, _keys_not_written_to =
      Io_helpers.sample_without_replacement number_of_keys_written keys
    in
    let source_base_dir, context_hash = cfg.existing_context in
    let value_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.storage_chunks
      * cfg.storage_chunk_bytes
    in
    let with_context f =
      let target_base_dir =
        let temp_dir = Option.value cfg.temp_dir ~default:"/tmp" in
        Format.asprintf
          "%s/%s_%d"
          temp_dir
          (Namespace.basename name)
          (Random.int 65536)
      in
      Io_helpers.copy_rec source_base_dir target_base_dir ;
      Format.eprintf "Finished copying original context to %s@." target_base_dir ;
      let context, index =
        Io_helpers.load_context_from_disk target_base_dir context_hash
      in
      let context =
        List.fold_left
          (fun context (key, _) ->
            let bytes =
              Base_samplers.uniform_bytes rng_state ~nbytes:value_size
            in
            write_storage context key bytes)
          context
          keys_written_to
      in
      let finalizer () =
        Gc.compact () ;
        Lwt_main.run
          (let open Lwt_syntax in
          let* () = Tezos_context.Context.close index in
          Tezos_stdlib_unix.Lwt_utils_unix.remove_dir target_base_dir)
      in
      let result =
        try f context
        with _ ->
          finalizer () ;
          exit 1
      in
      finalizer () ;
      result
    in
    let closure context =
      Lwt_main.run
        (let open Lwt_syntax in
        let* _context_hash = Io_helpers.commit context in
        Lwt.return_unit)
    in
    let workload =
      Write_random_keys
        {keys_written = number_of_keys_written; storage_bytes = value_size}
    in
    Generator.With_context {workload; closure; with_context}

  let create_benchmarks ~rng_state ~bench_num config =
    let base_dir, context_hash = config.existing_context in
    let tree =
      Io_helpers.with_context ~base_dir ~context_hash (fun context ->
          Io_stats.load_tree context config.subdirectory)
    in
    let keys = Io_helpers.Key_map.to_seq tree in
    List.repeat bench_num (make_bench rng_state config keys)
end

let () = Registration.register (module Write_random_keys_bench)
