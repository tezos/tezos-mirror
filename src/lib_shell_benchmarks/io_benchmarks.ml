(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@nomadic-labs.com>             *)
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

let purpose =
  Benchmark.Other_purpose "Measuring the time to access context file system"

(* io/s *)
let ns s = Namespace.make Shell_namespace.ns "io" s

(* io/s1/s2 *)
let ns2 s1 s2 = (Namespace.make ns s1) s2

let fv s1 s2 = Free_variable.of_namespace (ns2 s1 s2)

let read_model ~name =
  let fv = fv name in
  Model.bilinear_affine
    ~name:(ns2 name "read_model")
    ~intercept:(fv "read_latency")
    ~coeff1:(fv "depth")
    ~coeff2:(fv "storage_bytes_read")

let write_model ~name =
  let fv = fv name in
  Model.bilinear_affine
    ~name:(ns2 name "write_model")
    ~intercept:(fv "write_latency")
    ~coeff1:(fv "keys_written")
    ~coeff2:(fv "storage_bytes_write")

let read_model2 ~name =
  let fv = fv name in
  Model.bilinear_affine
    ~name:(ns2 name "read")
    ~intercept:(fv "intercept")
    ~coeff1:(fv "depth")
    ~coeff2:(fv "bytes")

let write_model2 ~name =
  let fv = fv name in
  Model.bilinear_affine
    ~name:(ns2 name "write")
    ~intercept:(fv "intercept")
    ~coeff1:(fv "depth")
    ~coeff2:(fv "bytes")

module Helpers = struct
  (* Samples keys in an alphabet of [card] elements. *)
  let sample_key ~card =
    assert (card > 0) ;
    let i = string_of_int (Random.int card) in
    "key" ^ i

  let random_key rng_state ~card ~depth =
    let depth = Base_samplers.sample_in_interval rng_state ~range:depth in
    Stdlib.List.init depth (fun _ -> sample_key ~card)

  (* Initializes a context by setting random bytes for each key in the
     given [key_set]. *)
  let random_contents rng_state context_dir index context key_set
      commit_batch_size =
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
              Io_helpers.commit_and_reload context_dir index context
            in
            Lwt.return (index, context, 0))
        key_set
        (index, context, 0)
    in
    Io_helpers.commit_and_reload context_dir index context

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

  let prepare_random_context rng_state context_dir commit_batch_size keys =
    let context_hash =
      Io_helpers.assert_ok ~msg:"Io_helpers.prepare_empty_context"
      @@ Lwt_main.run (Io_helpers.prepare_empty_context context_dir)
    in
    let context, index =
      Io_helpers.load_context_from_disk context_dir context_hash
    in
    Lwt_main.run
      (let open Lwt_syntax in
       let* context, index =
         random_contents
           rng_state
           context_dir
           index
           context
           keys
           commit_batch_size
       in
       Io_helpers.commit_and_reload context_dir index context)
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

  (* This config creates:
     - 1 target file of at most 1MB
     - At most 65536 files of 1KB

     - Files are scattered in directories of depth 10 to 1000
     - Commit for each 10_000 file additions

     In total, 66.5MB of max file contents. Produces a context file of 2GB.
  *)
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
           }
         ->
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
             temp_dir )
         ->
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
      (fun (Random_context_random_access {depth; storage_bytes; context_size})
         -> (depth, storage_bytes, context_size))
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
end

module Context_size_dependent_read_bench = struct
  (* ----------------------------------------------------------------------- *)
  (* Benchmark def *)

  let name = ns "CONTEXT_SIZE_DEPENDENT_READ"

  let info =
    "Benchmarking the read accesses with contexts of various sizes (with fixed \
     storage size except for the accessed key)"

  let tags = ["io"]

  let module_filename = __FILE__

  let purpose = purpose

  include Context_size_dependent_shared

  let create_benchmark ~rng_state cfg =
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
      let context_dir =
        Filename.temp_file ?temp_dir:cfg.temp_dir (Namespace.basename name) ""
      in
      Io_helpers.prepare_context_dir context_dir ;
      let context, index =
        Helpers.prepare_random_context
          rng_state
          context_dir
          cfg.commit_batch_size
          keys
      in
      let finalizer () =
        Gc.compact () ;
        Lwt_main.run
          (let open Lwt_syntax in
           let* () = Tezos_context.Context.close index in
           Tezos_stdlib_unix.Lwt_utils_unix.remove_dir context_dir)
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

  let group = Benchmark.Group "io"

  let model =
    Model.make
      ~conv:(function
        | Random_context_random_access {depth; storage_bytes; _} ->
            (depth, (storage_bytes, ())))
      (read_model ~name:"context_dependent")
end

let () = Registration.register_simple (module Context_size_dependent_read_bench)

module Context_size_dependent_write_bench = struct
  include Context_size_dependent_shared

  (* ----------------------------------------------------------------------- *)
  (* Benchmark def *)

  let name = ns "CONTEXT_SIZE_DEPENDENT_WRITE"

  let info =
    "Benchmarking the write accesses with contexts of various sizes (with \
     fixed storage size except for the written key)"

  let module_filename = __FILE__

  let purpose = purpose

  let tags = ["io"]

  let write_storage context key bytes =
    Lwt_main.run (Tezos_protocol_environment.Context.add context key bytes)

  let group = Benchmark.Group "io"

  let model =
    Model.make
      ~conv:(function
        | Random_context_random_access {depth; storage_bytes; _} ->
            (depth, (storage_bytes, ())))
      (write_model ~name:"context_dependent")

  let create_benchmark ~rng_state cfg =
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
      let context_dir =
        Filename.temp_file ?temp_dir:cfg.temp_dir (Namespace.basename name) ""
      in
      Io_helpers.prepare_context_dir context_dir ;
      let context, index =
        Helpers.prepare_random_context
          rng_state
          context_dir
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
           Tezos_stdlib_unix.Lwt_utils_unix.remove_dir context_dir)
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
end

let () =
  Registration.register_simple (module Context_size_dependent_write_bench)

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
           }
         ->
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
             temp_dir )
         ->
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

  (* This config creates:
     - 1 big directory with [256, 8192] items
       - 1 of the item of the big directory is the target file of at most 50KB
       - The other files in the big directory have 1KB each.
     - and at most 65536 files of 1KB each

     - Files and the big directory are scattered in directories of depth 3 to 30.
     - Commit for each 10_000 file additions

     In total, out 73.85MB of file contents. Produces a context file around 2.2GB.
  *)
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
      let files_under_big_directory =
        Array.init dir_width (fun i -> prefix @ [irmin_pack_key i])
      in
      (prefix, files_under_big_directory)
end

module Irmin_pack_read_bench = struct
  include Irmin_pack_shared

  let prepare_irmin_directory rng_state ~cfg ~key_set =
    if cfg.irmin_pack_max_width < 256 then
      Stdlib.failwith
        "Irmin_pack_read_bench: irmin_pack_max_width < 256, invalid \
         configuration"
    else
      let _prefix, files_under_big_directory =
        sample_irmin_directory rng_state ~cfg ~key_set
      in
      let dir_width = Array.length files_under_big_directory in
      let target_index = Random.int dir_width in
      let target_key = files_under_big_directory.(target_index) in
      let value_size =
        Base_samplers.sample_in_interval rng_state ~range:cfg.storage_chunks
        * cfg.storage_chunk_bytes
      in
      let key_set =
        let acc = ref key_set in
        Array.iteri
          (fun index key ->
            if index = target_index then
              acc := Key_map.insert key value_size !acc
            else acc := Key_map.insert key cfg.default_storage_bytes !acc)
          files_under_big_directory ;
        !acc
      in
      (target_key, value_size, key_set, files_under_big_directory)

  let name = ns "IRMIN_PACK_READ"

  let info = "Benchmarking read accesses in irmin-pack directories"

  let module_filename = __FILE__

  let purpose = purpose

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

  let model =
    Model.make
      ~conv:(function
        | Irmin_pack_read {depth; storage_bytes; _} ->
            (depth, (storage_bytes, ())))
      (read_model ~name:"irmin")

  let group = Benchmark.Group "io"

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun (Irmin_pack_read {depth; irmin_width; storage_bytes; context_size})
         -> (depth, irmin_width, storage_bytes, context_size))
      (fun (depth, irmin_width, storage_bytes, context_size) ->
        Irmin_pack_read {depth; irmin_width; storage_bytes; context_size})
      (tup4 int31 int31 int31 int31)

  let create_benchmark ~rng_state (cfg : config) =
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
      let context_dir =
        Filename.temp_file ?temp_dir:cfg.temp_dir (Namespace.basename name) ""
      in
      Io_helpers.prepare_context_dir context_dir ;
      let context, index =
        Helpers.prepare_random_context
          rng_state
          context_dir
          cfg.commit_batch_size
          keys
      in
      let finalizer () =
        Gc.compact () ;
        Lwt_main.run
          (let open Lwt_syntax in
           let* () = Tezos_context.Context.close index in
           Tezos_stdlib_unix.Lwt_utils_unix.remove_dir context_dir)
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
end

let () = Registration.register_simple (module Irmin_pack_read_bench)

module Irmin_pack_write_bench = struct
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

  let purpose = purpose

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
              {keys_written; irmin_width; storage_bytes; context_size})
         -> (keys_written, irmin_width, storage_bytes, context_size))
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

  let model =
    Model.make
      ~conv:(function
        | Irmin_pack_write {keys_written; storage_bytes; _} ->
            (keys_written, (storage_bytes, ())))
      (write_model ~name:"irmin")

  let group = Benchmark.Group "io"

  let write_storage context key bytes =
    Lwt_main.run (Context.add context key bytes)

  let create_benchmark ~rng_state (cfg : config) =
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
    let context_dir =
      Filename.temp_file ?temp_dir:cfg.temp_dir (Namespace.basename name) ""
    in
    let value_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.storage_chunks
      * cfg.storage_chunk_bytes
    in
    let with_context f =
      Io_helpers.prepare_context_dir context_dir ;
      let context, index =
        Helpers.prepare_random_context
          rng_state
          context_dir
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
           Tezos_stdlib_unix.Lwt_utils_unix.remove_dir context_dir)
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
end

let () = Registration.register_simple (module Irmin_pack_write_bench)

module Read_random_key_bench = struct
  type config = {
    existing_context : string * Context_hash.t;
    subdirectory : string;
  }

  let default_config =
    {
      existing_context = ("/no/such/directory", Context_hash.zero);
      subdirectory = "/no/such/key";
    }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {existing_context; subdirectory} -> (existing_context, subdirectory))
      (fun (existing_context, subdirectory) -> {existing_context; subdirectory})
      (obj2
         (req "existing_context" (tup2 string Context_hash.encoding))
         (req "subdirectory" string))

  let name = ns "READ_RANDOM_KEY"

  let info = "Benchmarking random read accesses in a subdirectory"

  let module_filename = __FILE__

  let purpose = purpose

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

  let group = Benchmark.Group "io"

  let model =
    Model.make
      ~conv:(function
        | Read_random_key {depth; storage_bytes} -> (depth, (storage_bytes, ())))
      (read_model ~name:"random")

  let make_bench rng_state config keys () =
    let card = Array.length keys in
    assert (card > 0) ;
    let key, value_size = keys.(Random.State.int rng_state card) in
    let with_context f =
      let context, index =
        let context_dir, context_hash = config.existing_context in
        Io_helpers.load_context_from_disk context_dir context_hash
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
    let context_dir, context_hash = config.existing_context in
    (* files under [config.subdirectory] *)
    let tree =
      Io_helpers.with_context ~context_dir ~context_hash (fun context ->
          Io_stats.load_tree context
          @@ Option.value_f ~default:(fun () ->
                 Stdlib.failwith
                   (Format.asprintf
                      "%a: invalid config subdirectory"
                      Namespace.pp
                      name))
          @@ Io_helpers.split_absolute_path config.subdirectory)
    in
    let keys = Array.of_seq @@ Io_helpers.Key_map.to_seq tree in
    List.repeat bench_num (make_bench rng_state config keys)
end

let () = Registration.register_simple_with_num (module Read_random_key_bench)

module Write_random_keys_bench = struct
  open Base_samplers

  type config = {
    existing_context : string * Context_hash.t;
    storage_chunk_bytes : int;
    storage_chunks : range;
    max_written_keys : int;
    temp_dir : string option;
    subdirectory : string;
  }

  let default_config =
    {
      existing_context = ("/no/such/directory", Context_hash.zero);
      storage_chunk_bytes = 1000;
      storage_chunks = {min = 1; max = 1000};
      max_written_keys = 10_000;
      temp_dir = None;
      subdirectory = "/no/such/key";
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
           }
         ->
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
             subdirectory )
         ->
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
         (req "subdirectory" string))

  let name = ns "WRITE_RANDOM_KEYS"

  let info = "Benchmarking random read accesses in a subdirectory"

  let module_filename = __FILE__

  let purpose = purpose

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

  let group = Benchmark.Group "io"

  let model =
    Model.make
      ~conv:(function
        | Write_random_keys {keys_written; storage_bytes; _} ->
            (keys_written, (storage_bytes, ())))
      (write_model ~name:"random")

  let write_storage context key bytes =
    Lwt_main.run (Context.add context key bytes)

  let make_bench rng_state (cfg : config) (keys : (string list * int) list) () =
    let total_keys_under_directory = List.length keys in
    let number_of_keys_written =
      min
        total_keys_under_directory
        (Random.State.int rng_state cfg.max_written_keys)
    in
    let keys_written_to, _keys_not_written_to =
      Io_helpers.sample_without_replacement number_of_keys_written keys
    in
    let source_context_dir, context_hash = cfg.existing_context in
    let value_size =
      Base_samplers.sample_in_interval rng_state ~range:cfg.storage_chunks
      * cfg.storage_chunk_bytes
    in
    let with_context f =
      let target_context_dir =
        let temp_dir = Option.value cfg.temp_dir ~default:"/tmp" in
        Format.asprintf
          "%s/%s_%d"
          temp_dir
          (Namespace.basename name)
          (Random.int 65536)
      in
      (* Copying the original context for EACH test *)
      Io_helpers.copy_rec source_context_dir target_context_dir ;
      Format.eprintf
        "Finished copying original context to %s@."
        target_context_dir ;
      let context, index =
        Io_helpers.load_context_from_disk target_context_dir context_hash
      in
      (* Overwrite [keys_written_to]. The times of the writes are not measured. *)
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
           Tezos_stdlib_unix.Lwt_utils_unix.remove_dir target_context_dir)
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
    (* This only measures the time to commit. *)
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
    let context_dir, context_hash = config.existing_context in
    (* Files under [config.subdirectory]. *)
    let tree =
      Io_helpers.with_context ~context_dir ~context_hash (fun context ->
          Io_stats.load_tree context
          @@ Option.value_f ~default:(fun () ->
                 Stdlib.failwith
                   (Format.asprintf
                      "%a: invalid config subdirectory"
                      Namespace.pp
                      name))
          @@ Io_helpers.split_absolute_path config.subdirectory)
    in
    let keys = List.of_seq @@ Io_helpers.Key_map.to_seq tree in
    List.repeat bench_num (make_bench rng_state config keys)
end

let () = Registration.register_simple_with_num (module Write_random_keys_bench)

module Shared = struct
  let purpose = purpose

  let tags = ["io"]

  let group = Benchmark.Group "io"

  type config = {
    tezos_data_dir : string;
    cache_dir : string;
    memory_available : float;
    runs : int;
  }

  type data_info = {
    data_dir : string;
    context_hash : Context_hash.t;
    cache_dir : string;
  }

  let default_config =
    {
      tezos_data_dir = "_snoop/tezos-node";
      cache_dir = "_snoop/cache";
      memory_available = 6.0;
      runs = 0;
    }

  (* [tezos-data-dir]/context *)
  let context_dir data_dir = Filename.concat data_dir "context"

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {tezos_data_dir; cache_dir; memory_available; runs} ->
        (tezos_data_dir, cache_dir, memory_available, runs))
      (fun (tezos_data_dir, cache_dir, memory_available, runs) ->
        {tezos_data_dir; cache_dir; memory_available; runs})
      (obj4
         (req "tezos_data_dir" string)
         (req "cache_dir" string)
         (req "memory_available" float)
         (req "runs" int31))

  type workload = Key of {depth : int; storage_bytes : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (function Key {depth; storage_bytes} -> (depth, storage_bytes))
      (fun (depth, storage_bytes) -> Key {depth; storage_bytes})
      (tup2 int31 int31)

  let workload_to_vector = function
    | Key {depth; storage_bytes} ->
        let keys =
          [
            ("depth", float_of_int depth);
            ("storage_bytes", float_of_int storage_bytes);
          ]
        in
        Sparse_vec.String.of_list keys

  let make_model model =
    Model.make
      ~conv:(function
        | Key {depth; storage_bytes} ->
            (* Shift depth so that it starts from 0 *)
            (depth - 1, (storage_bytes, ())))
      model

  (* To avoid long time (≒ 20mins) to traverse the tree, we have a cache file
     [<cache_dir>/<context_hash>_keysizes.txt] loadable in 3mins *)
  let build_key_list data_info =
    let open Lwt_result_syntax in
    let {cache_dir; context_hash; data_dir} = data_info in
    let context_dir = context_dir data_dir in
    let*! () = Tezos_stdlib_unix.Lwt_utils_unix.create_dir cache_dir in
    let fn_cache =
      Filename.concat
        cache_dir
        (Format.asprintf "%a_keysizes.txt" Context_hash.pp context_hash)
    in
    if Sys.file_exists fn_cache then return fn_cache
    else
      let open Lwt_io in
      with_file ~mode:Output fn_cache @@ fun oc ->
      let*! () =
        Io_stats.fold_tree context_dir context_hash [] () (fun () key tree ->
            let*! o = Context.Tree.to_value tree in
            match o with
            | Some bytes ->
                let len = Bytes.length bytes in
                write_line
                  oc
                  (Printf.sprintf "%s %d" (String.concat "/" key) len)
            | None -> Lwt.return_unit)
      in
      let*! () = write_line oc "END OF LIST" in
      return fn_cache

  let fold_tree data_info init f =
    let fn_cache =
      Lwt_main.run @@ build_key_list data_info
      |> Result.value_f ~default:(fun _ -> assert false)
    in
    Format.eprintf
      "Loading the cached trees of %s at %s@."
      data_info.data_dir
      fn_cache ;
    let tbl = Stdlib.Hashtbl.create 1024 in
    In_channel.with_open_text fn_cache @@ fun ic ->
    let rec loop acc =
      match input_line ic with
      | "END OF LIST" -> acc
      | l -> (
          match String.split ' ' ~limit:2 l with
          | [k; n] ->
              let ks =
                let ks = String.split '/' k in
                (* hashcons for shorter strings *)
                List.map
                  (fun k ->
                    if String.length k > 12 then k
                    else
                      match Stdlib.Hashtbl.find_opt tbl k with
                      | Some k -> k
                      | None ->
                          Stdlib.Hashtbl.add tbl k k ;
                          k)
                  ks
              in
              loop (f acc (ks, int_of_string n))
          | _ ->
              Stdlib.failwith (Printf.sprintf "Broken file list: %s" fn_cache))
    in
    loop init

  let stats_keys data_info =
    let depths_tbl = Stdlib.Hashtbl.create 101 in
    let blocks_tbl = Stdlib.Hashtbl.create 101 in
    let nkeys =
      let incr tbl key =
        let n = Option.value ~default:0 @@ Stdlib.Hashtbl.find_opt tbl key in
        Stdlib.Hashtbl.replace tbl key (n + 1)
      in
      fold_tree data_info 0 (fun nkeys (key, size) ->
          let depth = List.length key in
          incr depths_tbl depth ;
          let blocks = (size + 4095) / 4096 in
          incr blocks_tbl blocks ;
          nkeys + 1)
    in
    Format.eprintf "Got %d keys@." nkeys ;
    let to_sorted_list tbl =
      List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)
      @@ List.of_seq @@ Stdlib.Hashtbl.to_seq tbl
    in
    List.iter (fun (depth, n) -> Format.eprintf "Depth %d: %d@." depth n)
    @@ to_sorted_list depths_tbl ;
    List.iter (fun (blocks, n) -> Format.eprintf "Blocks %d: %d@." blocks n)
    @@ to_sorted_list blocks_tbl ;
    nkeys

  (* We have nearly 1_000_000_000 files in a recent context and it is impossible
     to carry the all in memory. Therefore we get 1_000_000+ random keys from
     the context for the benchmark.

     Most of the files are at the directory depth 4, 5 and 6 and smaller than
     4096 bytes. To infer the coefficients of the directory depth and the file
     size, the file selection is biased: the files in the directory depth less
     than 4 or the files bigger than 4096 are always selected. These special files
     are rare: only 23000 are found in today's context.
  *)
  let sample_keys ~rng data_info =
    let nkeys = stats_keys data_info in
    let nsamples = 1_000_000 in

    let normals, rares =
      let normals, rares =
        fold_tree data_info ([], []) (fun (acc, rares) (key, size) ->
            let depth = List.length key in
            if
              size > 4096 (* Big files are rare, so we keep all of them. *)
              || depth
                 <= 3 (* Shallow files are rare, so we keep all of them. *)
            then (acc, (key, size) :: rares)
            else if Random.State.int rng nkeys < nsamples then
              ((key, size) :: acc, rares)
            else (acc, rares))
      in
      (Array.of_list normals, Array.of_list rares)
    in
    Format.eprintf
      "Got %d normal keys and %d rare keys after filtering@."
      (Array.length normals)
      (Array.length rares) ;
    (normals, rares)

  (* Aggregate the samples

     Samples have lots of noises. To reduce the noise, the samples are grouped
     by [value_size]s and the median of each group is used for the data for
     [Measure].

     For the other benchmarks, this denoising is done by [Measure] module which
     repeats the run of the same parameter [nsamples] times and takes the median
     of the results. In the IO benchmarks we cannot rely on this mechanism due to
     the disk cache: the cache hits almost always when the same file is repeatedly
     accessed.
  *)
  let aggregate_samples samples =
    let tbl = Stdlib.Hashtbl.create 1023 in
    List.iter
      (fun (depth, value_size, nsecs) ->
        (* We round DOWN them to avoid underestimation *)
        let n = value_size / 256 * 256 in
        match Stdlib.Hashtbl.find_opt tbl (depth, n) with
        | None -> Stdlib.Hashtbl.replace tbl (depth, n) [nsecs]
        | Some nsecs_list ->
            Stdlib.Hashtbl.replace tbl (depth, n) (nsecs :: nsecs_list))
      samples ;

    (* medians *)
    let median xs =
      let open Float.Array in
      let a = of_list xs in
      sort Float.compare a ;
      get a (length a / 2)
    in

    Stdlib.Hashtbl.fold
      (fun (depth, n) nsecs_list acc ->
        if List.compare_length_with nsecs_list 5 = -1 then acc
        else
          let median = median nsecs_list in
          let workload = Key {depth; storage_bytes = n} in
          (fun () ->
            Generator.Calculated {workload; measure = (fun () -> median)})
          :: acc)
      tbl
      []

  (* - Use existing context.  Mainnet context just before a GC is preferable.
     - Restrict the available memory about to 6 GiB, to emulate an 8 GiB machine
     - Random accesses to the context to use the available memory for the disk cache.
     - Random accesses for the benchmark

     It ignores [bench_num].
  *)
  let prepare_io_benchmarks ~rng_state data_info memory_available f =
    let {context_hash; data_dir; _} = data_info in
    let context_dir = context_dir data_dir in
    (* We sample keys in the context, since we cannot carry the all *)
    let normal_keys, rare_keys = sample_keys ~rng:rng_state data_info in

    let get_random_key =
      let n_normal_keys = Array.length normal_keys in
      let n_rare_keys = Array.length rare_keys in
      fun () ->
        match Random.State.int rng_state 2 with
        | 0 ->
            let i = Random.State.int rng_state n_normal_keys in
            normal_keys.(i)
        | _ ->
            let i = Random.State.int rng_state n_rare_keys in
            rare_keys.(i)
    in
    (* Actual benchmarks *)
    let samples =
      Io_helpers.with_memory_restriction
        memory_available
        (fun restrict_memory ->
          restrict_memory () ;
          Io_helpers.purge_disk_cache () ;
          Io_helpers.with_context ~context_dir ~context_hash (fun context ->
              Io_helpers.fill_disk_cache
                ~rng:rng_state
                ~restrict_memory
                context
                [normal_keys; rare_keys]) ;
          restrict_memory () ;
          Lwt_main.run (f ~restrict_memory ~get_random_key))
    in
    aggregate_samples samples
end

module Read_bench = struct
  include Shared

  let default_config = {default_config with runs = 1_000_000}

  let name = ns "READ"

  let info = "Benchmarking random read accesses"

  let module_filename = __FILE__

  let model = make_model (read_model2 ~name:"read")

  let create_benchmarks ~rng_state ~bench_num:_ config =
    let context_dir = context_dir config.tezos_data_dir in
    let _, _, context_hash =
      Io_helpers.get_head_block_from_context_dir config.tezos_data_dir
    in
    let data_info =
      {
        data_dir = config.tezos_data_dir;
        context_hash;
        cache_dir = config.cache_dir;
      }
    in
    prepare_io_benchmarks ~rng_state data_info config.memory_available
    @@ fun ~restrict_memory ~get_random_key ->
    let open Lwt_syntax in
    let* context, index =
      Io_helpers.load_context_from_disk_lwt context_dir context_hash
    in
    let* acc =
      let rec loop acc n =
        if n <= 0 then Lwt.return acc
        else
          (* We need flush even for reading.
             Otherwise the tree on memory grows forever *)
          let* context = Io_helpers.flush context in
          let key, value_size = get_random_key () in
          let* nsecs, _ =
            (* Using [Lwt_main.run] here slows down the benchmark *)
            Measure.Time.measure_lwt (fun () -> Context.find context key)
          in
          let acc = (List.length key, value_size, nsecs) :: acc in
          if n mod 10000 = 0 then restrict_memory () ;
          loop acc (n - 1)
      in
      loop [] config.runs
    in
    let+ () = Tezos_context.Context.close index in
    acc
end

let () = Registration.register_simple_with_num (module Read_bench)

module Write_bench = struct
  include Shared

  let default_config = {default_config with runs = 100_000}

  let name = ns "WRITE"

  let info = "Benchmarking random write accesses"

  let module_filename = __FILE__

  let model = make_model (write_model2 ~name:"write")

  let create_benchmarks ~rng_state ~bench_num:_ config =
    let source_context_dir = context_dir config.tezos_data_dir in
    let _, _, context_hash =
      Io_helpers.get_head_block_from_context_dir config.tezos_data_dir
    in
    let data_info =
      {
        data_dir = config.tezos_data_dir;
        context_hash;
        cache_dir = config.cache_dir;
      }
    in
    let context_dir = source_context_dir ^ ".tmp" in
    prepare_io_benchmarks ~rng_state data_info config.memory_available
    @@ fun ~restrict_memory ~get_random_key ->
    let open Lwt.Syntax in
    (* Copy the context dir *)
    let () =
      Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.remove_dir context_dir
    in
    Format.eprintf "Copying the data directory to %s@." context_dir ;
    Io_helpers.copy_rec source_context_dir context_dir ;

    let* index = Tezos_context.Context.init ~readonly:false context_dir in
    let rec loop acc context_hash n =
      if n <= 0 then Lwt.return acc
      else
        let* context =
          let+ context = Tezos_context.Context.checkout index context_hash in
          match context with
          | None -> assert false
          | Some context ->
              Tezos_shell_context.Shell_context.wrap_disk_context context
        in
        let key, _value_size = get_random_key () in
        (* The biggest file we have is 368640B *)
        (* 0B - 4MB *)
        let value_size = Random.State.int rng_state 409600 in

        let random_bytes =
          Base_samplers.uniform_bytes rng_state ~nbytes:value_size
        in

        let* nsecs, context_hash =
          (* Using [Lwt_main.run] here slows down the benchmark *)
          Measure.Time.measure_lwt (fun () ->
              let* context = Context.add context key random_bytes in
              let* context_hash = Io_helpers.commit context in
              (* We need to call [flush] to finish the disk writing.
                 It is a sort of the worst case: in a real node,
                 it is rare to flush just after 1 write.
              *)
              let+ _context = Io_helpers.flush context in
              context_hash)
        in
        let acc = (List.length key, value_size, nsecs) :: acc in
        if n mod 100 = 0 then restrict_memory () ;
        loop acc context_hash (n - 1)
    in
    let* acc = loop [] context_hash config.runs in
    let+ () = Tezos_context.Context.close index in
    acc
end

let () = Registration.register_simple_with_num (module Write_bench)
