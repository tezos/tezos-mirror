(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

(* Backend-agnostic operations on the context *)

module Profiler =
  Tezos_protocol_environment.Environment_profiler.Context_ops_profiler

module Environment_context = Tezos_protocol_environment.Context
module Memory_context = Tezos_protocol_environment.Memory_context
module Brassaia = Tezos_context_brassaia.Tezos_context.Context

module Brassaia_memory =
  Tezos_context_brassaia_memory.Tezos_context_memory.Context

module Brassaia_memory_context =
  Tezos_protocol_environment.Brassaia_memory_context

let err_implementation_mismatch =
  Tezos_protocol_environment.err_implementation_mismatch

module Events = struct
  include Internal_event.Simple

  let section = ["node"; "context_ops"]

  let initializing_context =
    declare_2
      ~section
      ~level:Warning
      ~name:"initializing_context"
      ~msg:"initializing {context} context at {path}"
      ("context", Data_encoding.string)
      ("path", Data_encoding.string)

  let warning_experimental =
    declare_0
      ~section
      ~level:Warning
      ~name:"duocontext_warning_experimental"
      ~msg:"creating a duo context with Brassaia and Irmin."
      ()
end

(** Values of type [index] are used to [checkout] contexts specified by their hash. *)
type index =
  | Disk_index of Context.index
  | Memory_index of Tezos_context_memory.Context.index
  | Brassaia_index of Brassaia.index
  | Brassaia_memory_index of Brassaia_memory.index
  | Duo_index of Context_wrapper.Context.index
  | Duo_memory_index of Context_wrapper.Memory_context.index

open Environment_context

type t = Environment_context.t

let err_impl_mismatch ~got =
  err_implementation_mismatch
    ~expected:"shell, memory, brassaia or brassaia_memory"
    ~got

let irmin_dir = "context"

let brassaia_dir = "brassaia_context"

let backend_variable = "TEZOS_CONTEXT_BACKEND"

let irmin_context_dir root = Filename.(concat root irmin_dir)

let brassaia_context_dir root = Filename.(concat root brassaia_dir)

let context_dir root =
  match Sys.getenv backend_variable |> String.lowercase_ascii with
  | "brassaia" -> brassaia_context_dir root
  | "irmin" | "duo" | (exception Not_found) -> irmin_context_dir root
  | s ->
      Fmt.failwith
        "You tried to initialise the context with %s, this is not a known \
         context. Try `irmin` or `brassaia`"
        s

let do_not_use__brassaia_dir root = brassaia_context_dir root

let do_not_use__is_duo () =
  match Sys.getenv backend_variable |> String.lowercase_ascii with
  | "duo" -> true
  | _ | (exception Not_found) -> false

let init ~kind ?patch_context ?readonly ?index_log_size context_root_dir =
  let open Lwt_syntax in
  let irmin_dir = irmin_context_dir context_root_dir in
  let init_context () =
    let* () = Events.(emit initializing_context) ("irmin", irmin_dir) in
    let patch_context =
      Option.map
        (fun f context ->
          let open Lwt_result_syntax in
          let* context = f (Shell_context.wrap_disk_context context) in
          return @@ Shell_context.unwrap_disk_context context)
        patch_context
    in
    Context.init ?patch_context ?readonly ?index_log_size irmin_dir
  in

  let brassaia_dir = brassaia_context_dir context_root_dir in
  let init_brassaia_context () =
    let* () = Events.(emit initializing_context) ("brassaia", brassaia_dir) in
    let patch_context =
      Option.map
        (fun f context ->
          let open Lwt_result_syntax in
          let* context = f (Brassaia_context.wrap_disk_context context) in
          return @@ Brassaia_context.unwrap_disk_context context)
        patch_context
    in
    Brassaia.init ?patch_context ?readonly ?index_log_size brassaia_dir
  in

  let open Lwt_syntax in
  match kind with
  | `Disk ->
      let+ index = init_context () in
      Disk_index index
  | `Memory ->
      let+ index =
        Tezos_context_memory.Context.init ?readonly ?index_log_size irmin_dir
      in
      Memory_index index
  | `Brassaia ->
      let+ index = init_brassaia_context () in
      Brassaia_index index
  | `Brassaia_memory ->
      let+ index =
        Brassaia_memory.init ?readonly ?index_log_size brassaia_dir
      in
      Brassaia_memory_index index
  | `Duo_index ->
      let* irmin_index = init_context () in
      let+ brassaia_index = init_brassaia_context () in
      Duo_index {irmin_index; brassaia_index}
  | `Duo_index_memory ->
      let* irmin_index =
        Tezos_context_memory.Context.init ?readonly ?index_log_size irmin_dir
      in
      let+ brassaia_index =
        Brassaia_memory.init ?readonly ?index_log_size brassaia_dir
      in
      Duo_memory_index {irmin_index; brassaia_index}

(* Wrapper over init that uses an environment variable ('TEZOS_CONTEXT_BACKEND')
   to select the backend between Memory|Brassaia_memory and Disk|Brassaia *)
let init ~kind ?patch_context ?readonly ?index_log_size context_root_dir =
  let open Lwt_syntax in
  (* Gather the initialisation profiling otherwise aggregates will behave
     like records and create a section for each call *)
  () [@profiler.record {verbosity = Notice} "Context init"] ;
  match Sys.getenv backend_variable |> String.lowercase_ascii with
  | "brassaia" -> (
      match kind with
      | `Disk ->
          init
            ~kind:`Brassaia
            ?patch_context
            ?readonly
            ?index_log_size
            context_root_dir
      | `Memory ->
          init
            ~kind:`Brassaia_memory
            ?patch_context
            ?readonly
            ?index_log_size
            context_root_dir
      | _ ->
          init ~kind ?patch_context ?readonly ?index_log_size context_root_dir)
  | "duo" -> (
      match kind with
      | `Disk ->
          let* () = Events.(emit warning_experimental) () in
          init
            ~kind:`Duo_index
            ?patch_context
            ?readonly
            ?index_log_size
            context_root_dir
      | `Memory ->
          let* () = Events.(emit warning_experimental) () in
          init
            ~kind:`Duo_index_memory
            ?patch_context
            ?readonly
            ?index_log_size
            context_root_dir
      | _ ->
          init ~kind ?patch_context ?readonly ?index_log_size context_root_dir)
  | _ | (exception Not_found) ->
      init ~kind ?patch_context ?readonly ?index_log_size context_root_dir

let index (context : Environment_context.t) =
  match[@profiler.span_f {verbosity = Notice} ["context_ops"; "index"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Disk_index (Context.index ctxt)
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Memory_index (Tezos_context_memory.Context.index ctxt)
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia_index (Brassaia.index ctxt)
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory_index (Brassaia_memory.index ctxt)
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Duo_index (Context_wrapper.Context.index ctxt)
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Duo_memory_index (Context_wrapper.Memory_context.index ctxt)
  | Context t -> err_impl_mismatch ~got:t.impl_name

let mem (context : Environment_context.t) key =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "mem"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.mem ctxt key
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.mem ctxt key
  | Context {kind = Brassaia_context.Context; ctxt; _} -> Brassaia.mem ctxt key
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.mem ctxt key
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.mem ctxt key
  | Context t -> err_impl_mismatch ~got:t.impl_name

let mem_tree (context : Environment_context.t) key =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "mem_tree"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.mem_tree ctxt key
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.mem_tree ctxt key
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.mem_tree ctxt key
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.mem_tree ctxt key
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.mem_tree ctxt key
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.mem_tree ctxt key
  | Context t -> err_impl_mismatch ~got:t.impl_name

let find (context : Environment_context.t) key =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "find"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.find ctxt key
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.find ctxt key
  | Context {kind = Brassaia_context.Context; ctxt; _} -> Brassaia.find ctxt key
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.find ctxt key
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.find ctxt key
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.find ctxt key
  | Context t -> err_impl_mismatch ~got:t.impl_name

let add (context : Environment_context.t) key data =
  let open Lwt_syntax in
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "add"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add ctxt key data in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt = Tezos_context_memory.Context.add ctxt key data in
      Memory_context.wrap_memory_context ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia.add ctxt key data in
      Brassaia_context.wrap_disk_context ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia_memory.add ctxt key data in
      Brassaia_memory_context.wrap_memory_context ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      let+ ctxt = Context_wrapper.Context.add ctxt key data in
      Duo_context.wrap_disk_context ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      let+ ctxt = Context_wrapper.Memory_context.add ctxt key data in
      Duo_memory_context.wrap_memory_context ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let fold_value ?depth (context : Environment_context.t) key ~order ~init ~f =
  match[@profiler.span_f {verbosity = Notice} ["context_ops"; "fold_value"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.fold ?depth ctxt key ~order ~init ~f:(fun k tree acc ->
          let v () = Context.Tree.to_value tree in
          f k v acc)
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let open Tezos_context_memory in
      Context.fold ?depth ctxt key ~order ~init ~f:(fun k tree acc ->
          let v () = Context.Tree.to_value tree in
          f k v acc)
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.fold ?depth ctxt key ~order ~init ~f:(fun k tree acc ->
          let v () = Brassaia.Tree.to_value tree in
          f k v acc)
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.fold ?depth ctxt key ~order ~init ~f:(fun k tree acc ->
          let v () = Brassaia_memory.Tree.to_value tree in
          f k v acc)
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.fold
        ?depth
        ctxt
        key
        ~order
        ~init
        ~f:(fun k tree acc ->
          let v () = Context_wrapper.Context.Tree.to_value tree in
          f k v acc)
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.fold
        ?depth
        ctxt
        key
        ~order
        ~init
        ~f:(fun k tree acc ->
          let v () = Context_wrapper.Memory_context.Tree.to_value tree in
          f k v acc)
  | Context t -> err_impl_mismatch ~got:t.impl_name

let add_protocol (context : Environment_context.t) proto_hash =
  let open Lwt_syntax in
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "add_protocol"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_protocol ctxt proto_hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt = Tezos_context_memory.Context.add_protocol ctxt proto_hash in
      Memory_context.wrap_memory_context ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia.add_protocol ctxt proto_hash in
      Brassaia_context.wrap_disk_context ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia_memory.add_protocol ctxt proto_hash in
      Brassaia_memory_context.wrap_memory_context ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      let+ ctxt = Context_wrapper.Context.add_protocol ctxt proto_hash in
      Duo_context.wrap_disk_context ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      let+ ctxt = Context_wrapper.Memory_context.add_protocol ctxt proto_hash in
      Duo_memory_context.wrap_memory_context ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let get_protocol (context : Environment_context.t) =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "get_protocol"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.get_protocol ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.get_protocol ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.get_protocol ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.get_protocol ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.get_protocol ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.get_protocol ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let add_predecessor_block_metadata_hash (context : Environment_context.t) hash =
  let open Lwt_syntax in
  match[@profiler.span_s
         {verbosity = Notice}
           ["context_ops"; "add_predecessor_block_metadata_hash"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_predecessor_block_metadata_hash ctxt hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Tezos_context_memory.Context.add_predecessor_block_metadata_hash
          ctxt
          hash
      in
      Memory_context.wrap_memory_context ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia.add_predecessor_block_metadata_hash ctxt hash in
      Brassaia_context.wrap_disk_context ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Brassaia_memory.add_predecessor_block_metadata_hash ctxt hash
      in
      Brassaia_memory_context.wrap_memory_context ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      let+ ctxt =
        Context_wrapper.Context.add_predecessor_block_metadata_hash ctxt hash
      in
      Duo_context.wrap_disk_context ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Context_wrapper.Memory_context.add_predecessor_block_metadata_hash
          ctxt
          hash
      in
      Duo_memory_context.wrap_memory_context ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let add_predecessor_ops_metadata_hash (context : Environment_context.t) hash =
  let open Lwt_syntax in
  match[@profiler.span_s
         {verbosity = Notice}
           ["context_ops"; "add_predecessor_ops_metadata_hash"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_predecessor_ops_metadata_hash ctxt hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Tezos_context_memory.Context.add_predecessor_ops_metadata_hash ctxt hash
      in
      Memory_context.wrap_memory_context ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia.add_predecessor_ops_metadata_hash ctxt hash in
      Brassaia_context.wrap_disk_context ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia_memory.add_predecessor_ops_metadata_hash ctxt hash in
      Brassaia_memory_context.wrap_memory_context ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      let+ ctxt =
        Context_wrapper.Context.add_predecessor_ops_metadata_hash ctxt hash
      in
      Duo_context.wrap_disk_context ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Context_wrapper.Memory_context.add_predecessor_ops_metadata_hash
          ctxt
          hash
      in
      Duo_memory_context.wrap_memory_context ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let hash ~time ?message (context : Environment_context.t) =
  match[@profiler.span_f {verbosity = Notice} ["context_ops"; "hash"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.hash ~time ?message ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.hash ~time ?message ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.hash ~time ?message ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.hash ~time ?message ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.hash ~time ?message ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.hash ~time ?message ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let get_test_chain (context : Environment_context.t) =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "get_test_chain"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.get_test_chain ctxt
  | Context {kind = Memory_context.Context; _} ->
      Lwt.return Test_chain_status.Not_running
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.get_test_chain ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.get_test_chain ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.get_test_chain ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.get_test_chain ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let add_test_chain (context : Environment_context.t) status =
  let open Lwt_syntax in
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "add_test_chain"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_test_chain ctxt status in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt = Tezos_context_memory.Context.add_test_chain ctxt status in
      Memory_context.wrap_memory_context ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia.add_test_chain ctxt status in
      Brassaia_context.wrap_disk_context ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia_memory.add_test_chain ctxt status in
      Brassaia_memory_context.wrap_memory_context ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      let+ ctxt = Context_wrapper.Context.add_test_chain ctxt status in
      Duo_context.wrap_disk_context ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      let+ ctxt = Context_wrapper.Memory_context.add_test_chain ctxt status in
      Duo_memory_context.wrap_memory_context ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let fork_test_chain (context : Environment_context.t) ~protocol ~expiration =
  let open Lwt_syntax in
  match[@profiler.span_s
         {verbosity = Notice} ["context_ops"; "fork_test_chain"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.fork_test_chain ctxt ~protocol ~expiration in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Tezos_context_memory.Context.fork_test_chain ctxt ~protocol ~expiration
      in
      Memory_context.wrap_memory_context ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia.fork_test_chain ctxt ~protocol ~expiration in
      Brassaia_context.wrap_disk_context ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      let+ ctxt = Brassaia_memory.fork_test_chain ctxt ~protocol ~expiration in
      Brassaia_memory_context.wrap_memory_context ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      let+ ctxt =
        Context_wrapper.Context.fork_test_chain ctxt ~protocol ~expiration
      in
      Duo_context.wrap_disk_context ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Context_wrapper.Memory_context.fork_test_chain
          ctxt
          ~protocol
          ~expiration
      in
      Duo_memory_context.wrap_memory_context ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let commit ~time ?message (context : Environment_context.t) =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "commit"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.commit ~time ?message ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.commit ~time ?message ctxt
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.commit ~time ?message ctxt
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.commit ~time ?message ctxt
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.commit ~time ?message ctxt
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.commit ~time ?message ctxt
  | Context t -> err_impl_mismatch ~got:t.impl_name

let gc context_index context_hash =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "gc"]]
    context_index
  with
  | Disk_index index -> Context.gc index context_hash
  | Memory_index index -> Tezos_context_memory.Context.gc index context_hash
  | Brassaia_index index -> Brassaia.gc index context_hash
  | Brassaia_memory_index index -> Brassaia_memory.gc index context_hash
  | Duo_index index -> Context_wrapper.Context.gc index context_hash
  | Duo_memory_index index ->
      Context_wrapper.Memory_context.gc index context_hash

let wait_gc_completion context_index =
  match[@profiler.span_s
         {verbosity = Notice} ["context_ops"; "wait_gc_completion"]]
    context_index
  with
  | Disk_index index -> Context.wait_gc_completion index
  | Memory_index index -> Tezos_context_memory.Context.wait_gc_completion index
  | Brassaia_index index -> Brassaia.wait_gc_completion index
  | Brassaia_memory_index index -> Brassaia_memory.wait_gc_completion index
  | Duo_index index -> Context_wrapper.Context.wait_gc_completion index
  | Duo_memory_index index ->
      Context_wrapper.Memory_context.wait_gc_completion index

let is_gc_allowed context_index =
  match[@profiler.span_f {verbosity = Notice} ["context_ops"; "is_gc_allowed"]]
    context_index
  with
  | Disk_index index -> Context.is_gc_allowed index
  | Memory_index index -> Tezos_context_memory.Context.is_gc_allowed index
  | Brassaia_index index -> Brassaia.is_gc_allowed index
  | Brassaia_memory_index index -> Brassaia_memory.is_gc_allowed index
  | Duo_index index -> Context_wrapper.Context.is_gc_allowed index
  | Duo_memory_index index -> Context_wrapper.Memory_context.is_gc_allowed index

let split context_index =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "split"]]
    context_index
  with
  | Disk_index index -> Context.split index
  | Memory_index index -> Tezos_context_memory.Context.split index
  | Brassaia_index index -> Brassaia.split index
  | Brassaia_memory_index index -> Brassaia_memory.split index
  | Duo_index index -> Context_wrapper.Context.split index
  | Duo_memory_index index -> Context_wrapper.Memory_context.split index

let sync context_index =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "sync"]]
    context_index
  with
  | Disk_index index -> Context.sync index
  | Memory_index index -> Tezos_context_memory.Context.sync index
  | Brassaia_index index -> Brassaia.sync index
  | Brassaia_memory_index index -> Brassaia_memory.sync index
  | Duo_index index -> Context_wrapper.Context.sync index
  | Duo_memory_index index -> Context_wrapper.Memory_context.sync index

let commit_test_chain_genesis (context : Environment_context.t) block_header =
  match[@profiler.span_s
         {verbosity = Notice} ["context_ops"; "commit_test_chain_genesis"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.commit_test_chain_genesis ctxt block_header
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.commit_test_chain_genesis ctxt block_header
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.commit_test_chain_genesis ctxt block_header
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.commit_test_chain_genesis ctxt block_header
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.commit_test_chain_genesis ctxt block_header
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.commit_test_chain_genesis ctxt block_header
  | Context t -> err_impl_mismatch ~got:t.impl_name

let compute_testchain_genesis (context : Environment_context.t) block_hash =
  match[@profiler.span_f
         {verbosity = Notice} ["context_ops"; "compute_testchain_genesis"]]
    context
  with
  | Context {kind = Shell_context.Context; _} ->
      Context.compute_testchain_genesis block_hash
  | Context {kind = Memory_context.Context; _} ->
      Tezos_context_memory.Context.compute_testchain_genesis block_hash
  | Context {kind = Brassaia_context.Context; _} ->
      Brassaia.compute_testchain_genesis block_hash
  | Context {kind = Brassaia_memory_context.Context; _} ->
      Brassaia_memory.compute_testchain_genesis block_hash
  | Context {kind = Duo_context.Context; _} ->
      Context_wrapper.Context.compute_testchain_genesis block_hash
  | Context {kind = Duo_memory_context.Context; _} ->
      Context_wrapper.Memory_context.compute_testchain_genesis block_hash
  | Context t -> err_impl_mismatch ~got:t.impl_name

let merkle_tree (context : Environment_context.t) leaf_kind path =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "merkle_tree"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.merkle_tree ctxt leaf_kind path
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.merkle_tree ctxt leaf_kind path
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.merkle_tree ctxt leaf_kind path
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.merkle_tree ctxt leaf_kind path
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.merkle_tree ctxt leaf_kind path
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.merkle_tree ctxt leaf_kind path
  | Context t -> err_impl_mismatch ~got:t.impl_name

let merkle_tree_v2 (context : Environment_context.t) leaf_kind path =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "merkle_tree_v2"]]
    context
  with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.merkle_tree_v2 ctxt leaf_kind path
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.merkle_tree_v2 ctxt leaf_kind path
  | Context {kind = Brassaia_context.Context; ctxt; _} ->
      Brassaia.merkle_tree_v2 ctxt leaf_kind path
  | Context {kind = Brassaia_memory_context.Context; ctxt; _} ->
      Brassaia_memory.merkle_tree_v2 ctxt leaf_kind path
  | Context {kind = Duo_context.Context; ctxt; _} ->
      Context_wrapper.Context.merkle_tree_v2 ctxt leaf_kind path
  | Context {kind = Duo_memory_context.Context; ctxt; _} ->
      Context_wrapper.Memory_context.merkle_tree_v2 ctxt leaf_kind path
  | Context t -> err_impl_mismatch ~got:t.impl_name

let commit_genesis context_index ~chain_id ~time ~protocol =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "commit_genesis"]]
    context_index
  with
  | Disk_index index -> Context.commit_genesis index ~chain_id ~time ~protocol
  | Memory_index index ->
      Tezos_context_memory.Context.commit_genesis
        index
        ~chain_id
        ~time
        ~protocol
  | Brassaia_index index ->
      Brassaia.commit_genesis index ~chain_id ~time ~protocol
  | Brassaia_memory_index index ->
      Brassaia_memory.commit_genesis index ~chain_id ~time ~protocol
  | Duo_index index ->
      Context_wrapper.Context.commit_genesis index ~chain_id ~time ~protocol
  | Duo_memory_index index ->
      Context_wrapper.Memory_context.commit_genesis
        index
        ~chain_id
        ~time
        ~protocol

let checkout context_index context_hash =
  let open Lwt_syntax in
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "checkout"]]
    context_index
  with
  | Disk_index index ->
      let+ ctxt = Context.checkout index context_hash in
      Option.map Shell_context.wrap_disk_context ctxt
  | Memory_index index ->
      let+ ctxt = Tezos_context_memory.Context.checkout index context_hash in
      Option.map Memory_context.wrap_memory_context ctxt
  | Brassaia_index index ->
      let+ ctxt = Brassaia.checkout index context_hash in
      Option.map Brassaia_context.wrap_disk_context ctxt
  | Brassaia_memory_index index ->
      let+ ctxt = Brassaia_memory.checkout index context_hash in
      Option.map Brassaia_memory_context.wrap_memory_context ctxt
  | Duo_index index -> Duo_context.checkout index context_hash
  | Duo_memory_index index -> Duo_memory_context.checkout index context_hash

let checkout_exn context_index context_hash =
  let open Lwt_syntax in
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "checkout_exn"]]
    context_index
  with
  | Disk_index index ->
      let+ ctxt = Context.checkout_exn index context_hash in
      Shell_context.wrap_disk_context ctxt
  | Memory_index index ->
      let+ ctxt =
        Tezos_context_memory.Context.checkout_exn index context_hash
      in
      Memory_context.wrap_memory_context ctxt
  | Brassaia_index index ->
      let+ ctxt = Brassaia.checkout_exn index context_hash in
      Brassaia_context.wrap_disk_context ctxt
  | Brassaia_memory_index index ->
      let+ ctxt = Brassaia_memory.checkout_exn index context_hash in
      Brassaia_memory_context.wrap_memory_context ctxt
  | Duo_index index -> Duo_context.checkout_exn index context_hash
  | Duo_memory_index index -> Duo_memory_context.checkout_exn index context_hash

let exists context_index context_hash =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "exists"]]
    context_index
  with
  | Disk_index index -> Context.exists index context_hash
  | Memory_index index -> Tezos_context_memory.Context.exists index context_hash
  | Brassaia_index index -> Brassaia.exists index context_hash
  | Brassaia_memory_index index -> Brassaia_memory.exists index context_hash
  | Duo_index index -> Context_wrapper.Context.exists index context_hash
  | Duo_memory_index index ->
      Context_wrapper.Memory_context.exists index context_hash

let close context_index =
  match[@profiler.span_s {verbosity = Notice} ["context_ops"; "close"]]
    context_index
  with
  | Disk_index index -> Context.close index
  | Memory_index index -> Tezos_context_memory.Context.close index
  | Brassaia_index index -> Brassaia.close index
  | Brassaia_memory_index index -> Brassaia_memory.close index
  | Duo_index index -> Context_wrapper.Context.close index
  | Duo_memory_index index -> Context_wrapper.Memory_context.close index

let compute_testchain_chain_id (context : Environment_context.t) block_hash =
  match[@profiler.span_f
         {verbosity = Notice} ["context_ops"; "compute_testchain_chain_id"]]
    context
  with
  | Context {kind = Shell_context.Context; _} ->
      Context.compute_testchain_chain_id block_hash
  | Context {kind = Memory_context.Context; _} ->
      Tezos_context_memory.Context.compute_testchain_chain_id block_hash
  | Context {kind = Brassaia_context.Context; _} ->
      Brassaia.compute_testchain_chain_id block_hash
  | Context {kind = Brassaia_memory_context.Context; _} ->
      Brassaia_memory.compute_testchain_chain_id block_hash
  | Context {kind = Duo_context.Context; _} ->
      Context_wrapper.Context.compute_testchain_chain_id block_hash
  | Context {kind = Duo_memory_context.Context; _} ->
      Context_wrapper.Memory_context.compute_testchain_chain_id block_hash
  | Context t -> err_impl_mismatch ~got:t.impl_name

let export_snapshot context_index context_hash ~path =
  match context_index with
  | Disk_index index -> Context.export_snapshot index context_hash ~path
  | Memory_index index ->
      Tezos_context_memory.Context.export_snapshot index context_hash ~path
  | Brassaia_index index -> Brassaia.export_snapshot index context_hash ~path
  | Brassaia_memory_index index ->
      Brassaia_memory.export_snapshot index context_hash ~path
  | Duo_index index ->
      Context_wrapper.Context.export_snapshot index context_hash ~path
  | Duo_memory_index index ->
      Context_wrapper.Memory_context.export_snapshot index context_hash ~path

let integrity_check ?ppf ~root ~auto_repair ~always ~heads context_index =
  let open Lwt_syntax in
  match context_index with
  | Disk_index _ ->
      Context.Checks.Pack.Integrity_check.run
        ?ppf
        ~root:(irmin_context_dir root)
        ~auto_repair
        ~always
        ~heads
        ()
  | Memory_index _ ->
      Fmt.failwith
        "An in memory context doesn't need to be checked for integrity"
  | Brassaia_index _ ->
      Brassaia.Checks.Pack.Integrity_check.run
        ?ppf
        ~root:(brassaia_context_dir root)
        ~auto_repair
        ~always
        ~heads
        ()
  | Brassaia_memory_index _ ->
      Fmt.failwith
        "An in memory context doesn't need to be checked for integrity"
  | Duo_index _ ->
      let* () =
        Context.Checks.Pack.Integrity_check.run
          ?ppf
          ~root:(irmin_context_dir root)
          ~auto_repair
          ~always
          ~heads
          ()
      in
      Brassaia.Checks.Pack.Integrity_check.run
        ?ppf
        ~root:(brassaia_context_dir root)
        ~auto_repair
        ~always
        ~heads
        ()
  | Duo_memory_index _ ->
      Fmt.failwith
        "An in memory context doesn't need to be checked for integrity"
