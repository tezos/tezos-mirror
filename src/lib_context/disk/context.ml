(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2018-2020 Tarides <contact@tarides.com>                     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021 DaiLambda, Inc. <contact@dailambda.com>                *)
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

module Proof = Tezos_context_sigs.Context.Proof_types

(* Errors *)

type error +=
  | Cannot_create_file of string
  | Cannot_open_file of string
  | Cannot_find_protocol
  | Suspicious_file of int

let () =
  register_error_kind
    `Permanent
    ~id:"context_dump.write.cannot_open"
    ~title:"Cannot open file for context dump"
    ~description:""
    ~pp:(fun ppf uerr ->
      Format.fprintf
        ppf
        "@[Error while opening file for context dumping: %s@]"
        uerr)
    Data_encoding.(obj1 (req "context_dump_cannot_open" string))
    (function Cannot_create_file e -> Some e | _ -> None)
    (fun e -> Cannot_create_file e) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.read.cannot_open"
    ~title:"Cannot open file for context restoring"
    ~description:""
    ~pp:(fun ppf uerr ->
      Format.fprintf
        ppf
        "@[Error while opening file for context restoring: %s@]"
        uerr)
    Data_encoding.(obj1 (req "context_restore_cannot_open" string))
    (function Cannot_open_file e -> Some e | _ -> None)
    (fun e -> Cannot_open_file e) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.cannot_find_protocol"
    ~title:"Cannot find protocol"
    ~description:""
    ~pp:(fun ppf () -> Format.fprintf ppf "@[Cannot find protocol in context@]")
    Data_encoding.unit
    (function Cannot_find_protocol -> Some () | _ -> None)
    (fun () -> Cannot_find_protocol) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.read.suspicious"
    ~title:"Suspicious file: data after end"
    ~description:""
    ~pp:(fun ppf uerr ->
      Format.fprintf
        ppf
        "@[Remaining bytes in file after context restoring: %d@]"
        uerr)
    Data_encoding.(obj1 (req "context_restore_suspicious" int31))
    (function Suspicious_file e -> Some e | _ -> None)
    (fun e -> Suspicious_file e)

module type TEZOS_CONTEXT_UNIX = sig
  type error +=
    | Cannot_create_file of string
    | Cannot_open_file of string
    | Cannot_find_protocol
    | Suspicious_file of int

  include
    Tezos_context_sigs.Context.TEZOS_CONTEXT
      with type memory_context_tree := Tezos_context_memory.Context.tree

  (** Sync the context with disk. Only useful for read-only instances.
    Does not fail when the context is not in read-only mode. *)
  val sync : index -> unit Lwt.t

  val flush : t -> t Lwt.t

  (** {2 Context dumping} *)

  (** Rebuild a context from a given snapshot. *)
  val restore_context :
    index ->
    expected_context_hash:Context_hash.t ->
    nb_context_elements:int ->
    fd:Lwt_unix.file_descr ->
    in_memory:bool ->
    progress_display_mode:Animation.progress_display_mode ->
    unit tzresult Lwt.t

  (** Offline integrity checking and statistics for contexts. *)
  module Checks : sig
    module Pack : Irmin_pack_unix.Checks.S

    module Index : Index.Checks.S
  end
end

let reporter () =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k ()
    in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf
        k
        Fmt.stderr
        ("%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
        Logs_fmt.pp_header
        (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  {Logs.report}

let () =
  match Tezos_context_helpers.Env.(v.verbosity) with
  | `Info ->
      Logs.set_level (Some Logs.Info) ;
      Logs.set_reporter (reporter ())
  | `Debug ->
      Logs.set_level (Some Logs.Debug) ;
      Logs.set_reporter (reporter ())
  | `Default -> ()

module Events = struct
  include Internal_event.Simple

  let section = ["node"; "context"; "disk"]

  let init_context =
    declare_3
      ~section
      ~level:Info
      ~name:"init_context"
      ~msg:
        "initializing context (readonly: {readonly}, index_log_size: \
         {index_log_size}, lru_size: {lru_size})"
      ~pp1:Format.pp_print_bool
      ("readonly", Data_encoding.bool)
      ~pp2:Format.pp_print_int
      ("index_log_size", Data_encoding.int31)
      ~pp3:Format.pp_print_int
      ("lru_size", Data_encoding.int31)

  let starting_gc =
    declare_1
      ~section
      ~level:Info
      ~name:"starting_gc"
      ~msg:"starting context garbage collection for commit {context_hash}"
      ~pp1:Context_hash.pp
      ("context_hash", Context_hash.encoding)

  let ending_gc =
    declare_2
      ~section
      ~level:Info
      ~name:"ending_gc"
      ~msg:
        "context garbage collection finished in {duration} (finalised in \
         {finalisation})"
      ~pp1:Time.System.Span.pp_hum
      ("duration", Time.System.Span.encoding)
      ~pp2:Time.System.Span.pp_hum
      ("finalisation", Time.System.Span.encoding)

  let split_context =
    declare_0
      ~section
      ~level:Debug
      ~name:"split_context"
      ~msg:"splitting context into a new chunk"
      ()

  let gc_failure =
    declare_1
      ~section
      ~level:Warning
      ~name:"gc_failure"
      ~msg:"context garbage collection failed: {error}"
      ("error", Data_encoding.string)

  let gc_launch_failure =
    declare_1
      ~section
      ~level:Warning
      ~name:"gc_launch_failure"
      ~msg:"context garbage collection launch failed: {error}"
      ("error", Data_encoding.string)
end

module Make (Encoding : module type of Tezos_context_encoding.Context) = struct
  type error +=
    | Cannot_create_file = Cannot_create_file
    | Cannot_open_file = Cannot_open_file
    | Cannot_find_protocol = Cannot_find_protocol
    | Suspicious_file = Suspicious_file

  open Encoding

  (** Tezos - Versioned (key x value) store (over Irmin) *)

  module Store = struct
    module Maker = Irmin_pack_unix.Maker (Conf)
    include Maker.Make (Schema)
    module Schema = Tezos_context_encoding.Context.Schema
  end

  module Info = Store.Info
  module P = Store.Backend

  module Checks = struct
    module Conf = struct
      include Conf
    end

    module Maker = struct
      module Maker = Irmin_pack_unix.Maker (Conf)
      include Maker.Make (Schema)
    end

    module Pack : Irmin_pack_unix.Checks.S = Irmin_pack_unix.Checks.Make (Maker)

    module Index = struct
      module I = Irmin_pack_unix.Index.Make (Hash)
      include I.Checks
    end
  end

  type index = {
    path : string;
    repo : Store.Repo.t;
    patch_context : (context -> context tzresult Lwt.t) option;
    readonly : bool;
  }

  and context = {
    index : index;
    parents : Store.Commit.t list;
    tree : Store.tree;
    (* number of [remove], [add_tree] and [add] calls, not yet flushed *)
    ops : int;
  }

  type t = context

  let index {index; _} = index

  (*-- Version Access and Update -----------------------------------------------*)

  let current_protocol_key = ["protocol"]

  let current_test_chain_key = ["test_chain"]

  let current_predecessor_block_metadata_hash_key =
    ["predecessor_block_metadata_hash"]

  let current_predecessor_ops_metadata_hash_key =
    ["predecessor_ops_metadata_hash"]

  let sync index =
    if index.readonly then Store.reload index.repo ;
    Lwt.return ()

  let exists index key =
    let open Lwt_syntax in
    let* () = sync index in
    let+ o = Store.Commit.of_hash index.repo (Hash.of_context_hash key) in
    Option.is_some o

  let checkout index key =
    let open Lwt_syntax in
    let* () = sync index in
    let* o = Store.Commit.of_hash index.repo (Hash.of_context_hash key) in
    match o with
    | None -> return_none
    | Some commit ->
        let tree = Store.Commit.tree commit in
        return_some {index; tree; parents = [commit]; ops = 0}

  let checkout_exn index key =
    let open Lwt_syntax in
    let* o = checkout index key in
    match o with None -> Lwt.fail Not_found | Some p -> Lwt.return p

  (* unshallow possible 1-st level objects from previous partial
     checkouts ; might be better to pass directly the list of shallow
     objects. *)
  let unshallow context =
    let open Lwt_syntax in
    let* children = Store.Tree.list context.tree [] in
    P.Repo.batch context.index.repo (fun x y _ ->
        List.iter_s
          (fun (s, k) ->
            match Store.Tree.destruct k with
            | `Contents _ -> Lwt.return ()
            | `Node _ ->
                let* tree = Store.Tree.get_tree context.tree [s] in
                let+ _ =
                  Store.save_tree ~clear:true context.index.repo x y tree
                in
                ())
          children)

  let get_hash_version _c = Context_hash.Version.of_int 0

  let set_hash_version c v =
    let open Lwt_result_syntax in
    if Context_hash.Version.(of_int 0 = v) then return c
    else
      tzfail (Tezos_context_helpers.Context.Unsupported_context_hash_version v)

  let raw_commit ~time ?(message = "") context =
    let open Lwt_syntax in
    let info =
      Info.v ~author:"Tezos" (Time.Protocol.to_seconds time) ~message
    in
    let parents = List.map Store.Commit.key context.parents in
    let* () = unshallow context in
    let+ c = Store.Commit.v context.index.repo ~info ~parents context.tree in
    Store.Tree.clear context.tree ;
    c

  module Commit_hash = Irmin.Hash.Typed (Hash) (P.Commit_portable)

  let hash ~time ?(message = "") context =
    let info =
      Info.v ~author:"Tezos" (Time.Protocol.to_seconds time) ~message
    in
    let parents = List.map (fun c -> Store.Commit.hash c) context.parents in
    let node = Store.Tree.hash context.tree in
    let commit = P.Commit_portable.v ~parents ~node ~info in
    Hash.to_context_hash (Commit_hash.hash commit)

  let commit ~time ?message context =
    let open Lwt_syntax in
    let+ commit = raw_commit ~time ?message context in
    Hash.to_context_hash (Store.Commit.hash commit)

  let gc index context_hash =
    let open Lwt_syntax in
    let repo = index.repo in
    let* commit_opt =
      Store.Commit.of_hash index.repo (Hash.of_context_hash context_hash)
    in
    match commit_opt with
    | None ->
        Fmt.failwith "%a: unknown context hash" Context_hash.pp context_hash
    | Some commit -> (
        let* () = Events.(emit starting_gc) context_hash in
        Logs.info (fun m ->
            m "Launch GC for commit %a@." Context_hash.pp context_hash) ;
        let finished = function
          | Ok (stats : Irmin_pack_unix.Stats.Latest_gc.stats) ->
              let total_duration =
                Irmin_pack_unix.Stats.Latest_gc.total_duration stats
              in
              let finalise_duration =
                Irmin_pack_unix.Stats.Latest_gc.finalise_duration stats
              in
              Events.(emit ending_gc)
                ( Time.System.Span.of_seconds_exn total_duration,
                  Time.System.Span.of_seconds_exn finalise_duration )
          | Error (`Msg err) -> Events.(emit gc_failure) err
        in
        let commit_key = Store.Commit.key commit in
        let* launch_result = Store.Gc.run ~finished repo commit_key in
        match launch_result with
        | Ok _ -> return_unit
        | Error (`Msg err) ->
            let* () = Events.(emit gc_launch_failure) err in
            return_unit)

  let wait_gc_completion index =
    let open Lwt_syntax in
    let* () = sync index in
    let* r = Store.Gc.wait index.repo in
    match r with
    | Ok _stats_opt -> return_unit
    | Error (`Msg _msg) ->
        (* Logs will be printed by the [gc] caller. *)
        return_unit

  let is_gc_allowed index = Store.Gc.is_allowed index.repo

  let split index =
    let open Lwt_syntax in
    let* () = Events.(emit split_context ()) in
    Store.split index.repo ;
    Lwt.return_unit

  let export_snapshot index context_hash ~path =
    let open Lwt_syntax in
    let* commit_opt =
      Store.Commit.of_hash index.repo (Hash.of_context_hash context_hash)
    in
    match commit_opt with
    | None ->
        Fmt.failwith "%a: unknown context hash" Context_hash.pp context_hash
    | Some commit ->
        let h = Store.Commit.key commit in
        Store.create_one_commit_store index.repo h path

  (*-- Generic Store Primitives ------------------------------------------------*)

  let data_key = Tezos_context_sigs.Context.data_key

  type key = string list

  type value = bytes

  type tree = Store.tree

  type node_key = Store.node_key

  type value_key = Store.contents_key

  type kinded_key = [`Node of node_key | `Value of value_key]

  module Tree = Tezos_context_helpers.Context.Make_tree (Conf) (Store)
  include Tezos_context_helpers.Context.Make_config (Conf)
  include Tezos_context_helpers.Context.Make_proof (Store) (Conf)

  let mem ctxt key = Tree.mem ctxt.tree (data_key key)

  let mem_tree ctxt key = Tree.mem_tree ctxt.tree (data_key key)

  let raw_find ctxt key = Tree.find ctxt.tree key

  let list ctxt ?offset ?length key =
    Tree.list ctxt.tree ?offset ?length (data_key key)

  let length ctxt key = Tree.length ctxt.tree (data_key key)

  let find ctxt key = raw_find ctxt (data_key key)

  let incr_ops ctxt = {ctxt with ops = ctxt.ops + 1}

  let raw_add ctxt key data =
    let open Lwt_syntax in
    let+ tree = Tree.add ctxt.tree key data in
    incr_ops {ctxt with tree}

  let add ctxt key data = raw_add ctxt (data_key key) data

  let raw_remove ctxt k =
    let open Lwt_syntax in
    let+ tree = Tree.remove ctxt.tree k in
    incr_ops {ctxt with tree}

  let remove ctxt key = raw_remove ctxt (data_key key)

  let find_tree ctxt key = Tree.find_tree ctxt.tree (data_key key)

  let flush context =
    let open Lwt_syntax in
    let+ _ =
      P.Repo.batch context.index.repo (fun x y _ ->
          Store.save_tree ~clear:true context.index.repo x y context.tree)
    in
    {context with ops = 0}

  let may_flush context =
    if
      (not context.index.readonly)
      && context.ops >= Tezos_context_helpers.Env.(v.auto_flush)
    then flush context
    else Lwt.return context

  let add_tree ctxt key tree =
    let open Lwt_syntax in
    let* ctxt = may_flush ctxt in
    let+ tree = Tree.add_tree ctxt.tree (data_key key) tree in
    incr_ops {ctxt with tree}

  let fold ?depth ctxt key ~order ~init ~f =
    Tree.fold ?depth ctxt.tree (data_key key) ~order ~init ~f

  (** The light mode relies on the implementation of this
    function, because it uses Irmin.Type.of_string to rebuild values
    of type Irmin.Hash.t. This is a temporary workaround until we
    do that in a type safe manner when there are less moving pieces. *)
  let merkle_hash_to_string = Irmin.Type.to_string Store.Hash.t

  let rec tree_to_raw_context tree =
    let open Lwt_syntax in
    match Store.Tree.destruct tree with
    | `Contents (v, _) ->
        let+ v = Store.Tree.Contents.force_exn v in
        Proof.Key v
    | `Node _ ->
        let* kvs = Store.Tree.list tree [] in
        let f acc (key, _) =
          (* get_tree is safe, because we iterate over keys *)
          let* tree = Store.Tree.get_tree tree [key] in
          let+ sub_raw_context = tree_to_raw_context tree in
          String.Map.add key sub_raw_context acc
        in
        let+ res = List.fold_left_s f String.Map.empty kvs in
        Proof.Dir res

  let tree_to_memory_tree (tree : tree) :
      Tezos_context_memory.Context.tree Lwt.t =
    let contents path bytes acc =
      Tezos_context_memory.Context.Tree.add acc path bytes
    in
    Store.Tree.fold
      ~force:`True
      ~order:`Undefined
      ~cache:false
      ~uniq:`False
      ~contents
      tree
      (Tezos_context_memory.Context.make_empty_tree ())

  let to_memory_tree (ctxt : t) (key : string list) :
      Tezos_context_memory.Context.tree option Lwt.t =
    let open Lwt_option_syntax in
    let* ctxt_tree = find_tree ctxt key in
    let*! c = tree_to_memory_tree ctxt_tree in
    return c

  let merkle_hash tree =
    let merkle_hash_kind =
      match Store.Tree.destruct tree with
      | `Contents _ -> Proof.Contents
      | `Node _ -> Proof.Node
    in
    let hash_str = Store.Tree.hash tree |> merkle_hash_to_string in
    Proof.Hash (merkle_hash_kind, hash_str)

  let merkle_tree t leaf_kind key =
    let open Lwt_syntax in
    let* subtree_opt = Store.Tree.find_tree t.tree (data_key []) in
    match subtree_opt with
    | None -> Lwt.return String.Map.empty
    | Some subtree ->
        let key_to_string k = String.concat ";" k in
        let rec key_to_merkle_tree t target =
          match (Store.Tree.destruct t, target) with
          | _, [] ->
              (* We cannot use this case as the base case, because a merkle_node
                 is a map from string to something. In this case, we have
                 no key to put in the map's domain. *)
              raise
                (Invalid_argument
                   (Printf.sprintf "Reached end of key (top-level key was: %s)"
                   @@ key_to_string key))
          | _, [hd] ->
              let finally key =
                (* get_tree is safe because we iterate on keys *)
                let* tree = Store.Tree.get_tree t [key] in
                if key = hd then
                  (* on the target path: the final leaf *)
                  match leaf_kind with
                  | Proof.Hole -> Lwt.return @@ merkle_hash tree
                  | Proof.Raw_context ->
                      let+ raw_context = tree_to_raw_context tree in
                      Proof.Data raw_context
                else
                  (* a sibling of the target path: return a hash *)
                  Lwt.return @@ merkle_hash tree
              in
              let* l = Store.Tree.list t [] in
              List.fold_left_s
                (fun acc (key, _) ->
                  let+ v = finally key in
                  String.Map.add key v acc)
                String.Map.empty
                l
          | `Node _, target_hd :: target_tl ->
              let continue key =
                (* get_tree is safe because we iterate on keys *)
                let* tree = Store.Tree.get_tree t [key] in
                if key = target_hd then
                  (* on the target path: recurse *)
                  let+ sub = key_to_merkle_tree tree target_tl in
                  Proof.Continue sub
                else
                  (* a sibling of the target path: return a hash *)
                  Lwt.return @@ merkle_hash tree
              in
              let* l = Store.Tree.list t [] in
              List.fold_left_s
                (fun acc (key, _) ->
                  let+ atom = continue key in
                  String.Map.add key atom acc)
                String.Map.empty
                l
          | `Contents _, _ ->
              raise
                (Invalid_argument
                   (Printf.sprintf
                      "(`Contents _, l) when l <> [_] (in other words: found a \
                       leaf node whereas key %s (top-level key: %s) wasn't \
                       fully consumed)"
                      (key_to_string target)
                      (key_to_string key)))
        in
        key_to_merkle_tree subtree key

  let produce_tree_proof index = produce_tree_proof index.repo

  let produce_stream_proof index = produce_stream_proof index.repo

  module Storelike = struct
    type key = string list

    type tree = Store.tree

    type value = bytes

    let find = Tree.find

    let find_tree = Tree.find_tree

    let unshallow = Tree.unshallow
  end

  module Get_data = Tezos_context_sigs.Context.With_get_data ((
    Storelike : Tezos_context_sigs.Context.Storelike))

  let merkle_tree_v2 ctx leaf_kind key =
    let open Lwt_syntax in
    match Tree.kinded_key ctx.tree with
    | None -> raise (Invalid_argument "On-disk context.tree has no kinded_key")
    | Some kinded_key ->
        let* proof, _ =
          produce_tree_proof
            ctx.index
            kinded_key
            (Get_data.get_data leaf_kind [key])
        in
        return proof

  (*-- Predefined Fields -------------------------------------------------------*)

  module Root_tree = struct
    let get_protocol t =
      let open Lwt_syntax in
      let+ o = Tree.find t current_protocol_key in
      let data =
        WithExceptions.Option.to_exn_f ~none:(fun () -> assert false) o
      in
      Protocol_hash.of_bytes_exn data

    let add_protocol t v =
      let v = Protocol_hash.to_bytes v in
      Tree.add t current_protocol_key v

    let get_test_chain t =
      let open Lwt_syntax in
      let* o = Tree.find t current_test_chain_key in
      let data =
        WithExceptions.Option.to_exn
          ~none:(Failure "Unexpected error (Context.get_test_chain)")
          o
      in
      match Data_encoding.Binary.of_bytes Test_chain_status.encoding data with
      | Error re ->
          Format.kasprintf
            (fun s -> Lwt.fail (Failure s))
            "Error in Context.get_test_chain: %a"
            Data_encoding.Binary.pp_read_error
            re
      | Ok r -> Lwt.return r

    let add_test_chain t id =
      let id =
        Data_encoding.Binary.to_bytes_exn Test_chain_status.encoding id
      in
      Tree.add t current_test_chain_key id

    let find_predecessor_block_metadata_hash t =
      let open Lwt_syntax in
      let* o = Tree.find t current_predecessor_block_metadata_hash_key in
      match o with
      | None -> return_none
      | Some data -> (
          match
            Data_encoding.Binary.of_bytes_opt Block_metadata_hash.encoding data
          with
          | None ->
              raise
                (Failure
                   "Unexpected error \
                    (Context.get_predecessor_block_metadata_hash)")
          | Some r -> return_some r)

    let add_predecessor_block_metadata_hash t hash =
      let data =
        Data_encoding.Binary.to_bytes_exn Block_metadata_hash.encoding hash
      in
      Tree.add t current_predecessor_block_metadata_hash_key data

    let find_predecessor_ops_metadata_hash t =
      let open Lwt_syntax in
      let* o = Tree.find t current_predecessor_ops_metadata_hash_key in
      match o with
      | None -> return_none
      | Some data -> (
          match
            Data_encoding.Binary.of_bytes_opt
              Operation_metadata_list_list_hash.encoding
              data
          with
          | None ->
              raise
                (Failure
                   "Unexpected error \
                    (Context.get_predecessor_ops_metadata_hash)")
          | Some r -> return_some r)

    let add_predecessor_ops_metadata_hash t hash =
      let data =
        Data_encoding.Binary.to_bytes_exn
          Operation_metadata_list_list_hash.encoding
          hash
      in
      Tree.add t current_predecessor_ops_metadata_hash_key data
  end

  let get_protocol ctxt = Root_tree.get_protocol ctxt.tree

  let get_test_chain ctxt = Root_tree.get_test_chain ctxt.tree

  let find_predecessor_block_metadata_hash ctxt =
    Root_tree.find_predecessor_block_metadata_hash ctxt.tree

  let find_predecessor_ops_metadata_hash ctxt =
    Root_tree.find_predecessor_ops_metadata_hash ctxt.tree

  let lift_tree_add_to_ctxt tree_add ctxt v =
    let open Lwt_syntax in
    let+ tree = tree_add ctxt.tree v in
    incr_ops {ctxt with tree}

  let add_protocol = lift_tree_add_to_ctxt Root_tree.add_protocol

  let add_test_chain = lift_tree_add_to_ctxt Root_tree.add_test_chain

  let add_predecessor_block_metadata_hash =
    lift_tree_add_to_ctxt Root_tree.add_predecessor_block_metadata_hash

  let add_predecessor_ops_metadata_hash =
    lift_tree_add_to_ctxt Root_tree.add_predecessor_ops_metadata_hash

  let remove_test_chain v = raw_remove v current_test_chain_key

  let fork_test_chain v ~protocol ~expiration =
    add_test_chain v (Forking {protocol; expiration})

  (*-- Initialisation ----------------------------------------------------------*)

  let init ?patch_context ?(readonly = false) ?index_log_size:tbl_log_size root
      =
    let open Lwt_syntax in
    (* Forces the context to use the minimal indexing strategy. *)
    let indexing_strategy = Irmin_pack.Indexing_strategy.minimal in
    let+ repo =
      let env = Tezos_context_helpers.Env.v in
      let index_log_size =
        Option.value
          tbl_log_size
          ~default:Tezos_context_helpers.Env.(env.index_log_size)
      in
      let lru_size = env.lru_size in
      let* () =
        Events.(emit init_context (readonly, index_log_size, lru_size))
      in
      Store.Repo.v
        (Irmin_pack.config
           ~readonly
           ~indexing_strategy
           ~index_log_size
           ~lru_size
           root)
    in
    {path = root; repo; patch_context; readonly}

  let close index =
    let _interrupted_gc = Store.Gc.cancel index.repo in
    Store.Repo.close index.repo

  let get_branch chain_id = Format.asprintf "%a" Chain_id.pp chain_id

  let empty index = {index; tree = Store.Tree.empty (); parents = []; ops = 0}

  let is_empty {tree; _} = Store.Tree.is_empty tree

  let commit_genesis index ~chain_id ~time ~protocol =
    let open Lwt_result_syntax in
    let ctxt = empty index in
    let* ctxt =
      match index.patch_context with
      | None -> return ctxt
      | Some patch_context -> patch_context ctxt
    in
    let*! ctxt = add_protocol ctxt protocol in
    let*! ctxt = add_test_chain ctxt Not_running in
    let*! commit = raw_commit ~time ~message:"Genesis" ctxt in
    let*! () = Store.Branch.set index.repo (get_branch chain_id) commit in
    return (Hash.to_context_hash (Store.Commit.hash commit))

  let compute_testchain_chain_id genesis =
    let genesis_hash = Block_hash.hash_bytes [Block_hash.to_bytes genesis] in
    Chain_id.of_block_hash genesis_hash

  let compute_testchain_genesis forked_block =
    let genesis = Block_hash.hash_bytes [Block_hash.to_bytes forked_block] in
    genesis

  let commit_test_chain_genesis ctxt (forked_header : Block_header.t) =
    let open Lwt_syntax in
    let message =
      Format.asprintf
        "Forking testchain at level %ld."
        forked_header.shell.level
    in
    let* commit =
      raw_commit ~time:forked_header.shell.timestamp ~message ctxt
    in
    let faked_shell_header : Block_header.shell_header =
      {
        forked_header.shell with
        proto_level = succ forked_header.shell.proto_level;
        predecessor = Block_hash.zero;
        validation_passes = 0;
        operations_hash = Operation_list_list_hash.empty;
        context = Hash.to_context_hash (Store.Commit.hash commit);
      }
    in
    let forked_block = Block_header.hash forked_header in
    let genesis_hash = compute_testchain_genesis forked_block in
    let chain_id = compute_testchain_chain_id genesis_hash in
    let genesis_header : Block_header.t =
      {
        shell = {faked_shell_header with predecessor = genesis_hash};
        protocol_data = Bytes.create 0;
      }
    in
    let branch = get_branch chain_id in
    let+ () = Store.Branch.set ctxt.index.repo branch commit in
    genesis_header

  let clear_test_chain index chain_id =
    (* TODO remove commits... ??? *)
    let branch = get_branch chain_id in
    Store.Branch.remove index.repo branch

  let set_head index chain_id commit =
    let open Lwt_syntax in
    let branch = get_branch chain_id in
    let* o = Store.Commit.of_hash index.repo (Hash.of_context_hash commit) in
    match o with
    | None -> assert false
    | Some commit -> Store.Branch.set index.repo branch commit

  let set_master index commit =
    let open Lwt_syntax in
    let* o = Store.Commit.of_hash index.repo (Hash.of_context_hash commit) in
    match o with
    | None -> assert false
    | Some commit -> Store.Branch.set index.repo Store.Branch.main commit

  (* Context dumping *)

  module Dumpable_context = struct
    type nonrec index = index

    type nonrec context = context

    type tree = Store.tree

    type hash = Store.hash

    module Kinded_hash = struct
      type t = [`Blob of hash | `Node of hash]

      let encoding : t Data_encoding.t =
        let open Data_encoding in
        let kind_encoding = string_enum [("node", `Node); ("blob", `Blob)] in
        conv
          (function
            | `Blob h -> (`Blob, Context_hash.to_bytes (Hash.to_context_hash h))
            | `Node h -> (`Node, Context_hash.to_bytes (Hash.to_context_hash h)))
          (function
            | `Blob, h ->
                `Blob (Hash.of_context_hash (Context_hash.of_bytes_exn h))
            | `Node, h ->
                `Node (Hash.of_context_hash (Context_hash.of_bytes_exn h)))
          (obj2 (req "kind" kind_encoding) (req "value" bytes))
    end

    type commit_info = Info.t

    type batch =
      | Batch of
          Store.repo * [`Read | `Write] P.Contents.t * [`Read | `Write] P.Node.t

    let batch index f =
      P.Repo.batch index.repo (fun x y _ -> f (Batch (index.repo, x, y)))

    let commit_info_encoding =
      let open Data_encoding in
      conv
        (fun irmin_info ->
          let author = Info.author irmin_info in
          let message = Info.message irmin_info in
          let date = Info.date irmin_info in
          (author, message, date))
        (fun (author, message, date) -> Info.v ~author date ~message)
        (obj3 (req "author" string) (req "message" string) (req "date" int64))

    let hash_equal (h1 : hash) (h2 : hash) = h1 = h2

    let context_parents ctxt =
      match ctxt with
      | {parents = [commit]; _} ->
          let parents = Store.Commit.parents commit in
          let parents =
            List.map
              (fun k -> P.Commit.Key.to_hash k |> Hash.to_context_hash)
              parents
          in
          List.sort Context_hash.compare parents
      | _ -> assert false

    let context_info = function
      | {parents = [c]; _} -> Store.Commit.info c
      | _ -> assert false

    let checkout idx h = checkout idx h

    let set_context ~info ~parents ctxt context_hash =
      let open Lwt_syntax in
      let parents = List.sort Context_hash.compare parents in
      let parents =
        (* All commit objects in the context are indexed, so it's safe to build a
           hash-only key referencing them. *)
        List.map
          (fun h ->
            Hash.of_context_hash h |> Irmin_pack_unix.Pack_key.v_indexed)
          parents
      in
      let+ c = Store.Commit.v ctxt.index.repo ~info ~parents ctxt.tree in
      let h = Store.Commit.hash c in
      Context_hash.equal context_hash (Hash.to_context_hash h)

    let context_tree ctxt = ctxt.tree

    module Snapshot = struct
      include Store.Snapshot

      let kinded_hash_encoding : kinded_hash Data_encoding.t =
        let open Data_encoding in
        let kind_encoding =
          string_enum [("node", `Node); ("contents", `Contents)]
        in
        conv
          (function
            | Contents (h, ()) ->
                (`Contents, Context_hash.to_bytes (Hash.to_context_hash h))
            | Node h -> (`Node, Context_hash.to_bytes (Hash.to_context_hash h)))
          (function
            | `Contents, h ->
                let h = Hash.of_context_hash (Context_hash.of_bytes_exn h) in
                Contents (h, ())
            | `Node, h ->
                Node (Hash.of_context_hash (Context_hash.of_bytes_exn h)))
          (obj2 (req "kind" kind_encoding) (req "value" bytes))

      let hash_encoding : hash Data_encoding.t =
        let open Data_encoding in
        conv
          (fun h -> Context_hash.to_bytes (Hash.to_context_hash h))
          (fun h -> Hash.of_context_hash (Context_hash.of_bytes_exn h))
          bytes

      let entry_encoding : entry Data_encoding.t =
        let open Data_encoding in
        conv
          (fun {step; hash} -> (step, hash))
          (fun (step, hash) -> {step; hash})
          (obj2 (req "name" string) (req "hash" kinded_hash_encoding))

      let inode_tree_encoding : inode_tree Data_encoding.t =
        let open Data_encoding in
        let pair_encoding =
          obj2 (req "index" uint16) (req "hash" hash_encoding)
        in
        conv
          (fun {depth; length; pointers} ->
            (Int32.of_int depth, Int32.of_int length, pointers))
          (fun (depth, length, pointers) ->
            {depth = Int32.to_int depth; length = Int32.to_int length; pointers})
          (obj3
             (req "depth" int32)
             (req "length" int32)
             (req "pointers" (list pair_encoding)))

      let v_encoding : v Data_encoding.t =
        let open Data_encoding in
        let inode_tree_case =
          case
            ~title:"tree"
            (Tag (Char.code 't'))
            inode_tree_encoding
            (function Inode_tree t -> Some t | _ -> None)
            (fun t -> Inode_tree t)
        in
        let inode_value_case =
          case
            ~title:"value"
            (Tag (Char.code 'v'))
            (list entry_encoding)
            (function Inode_value t -> Some t | _ -> None)
            (fun t -> Inode_value t)
        in
        Data_encoding.union ~tag_size:`Uint8 [inode_tree_case; inode_value_case]

      let encoding : inode Data_encoding.t =
        let open Data_encoding in
        conv
          (fun {v; root} -> (v, root))
          (fun (v, root) -> {v; root})
          (obj2 (req "v" v_encoding) (req "root" bool))
    end

    let tree_iteri_unique ?(on_disk = false) index f tree =
      let root_key =
        match Store.Tree.key tree with None -> assert false | Some key -> key
      in
      let on_disk =
        if on_disk then
          let path = Filename.concat index.path "index_snapshot" in
          Some (`Path path)
        else None
      in
      Snapshot.export ?on_disk index.repo f ~root_key

    type import = Snapshot.Import.process

    let v_import ?(in_memory = false) idx =
      let on_disk =
        if in_memory then None
        else
          (* by default the import is using an on-disk index. *)
          let index_on_disk = Filename.concat idx.path "index_snapshot" in
          Some (`Path index_on_disk)
      in
      Snapshot.Import.v ?on_disk idx.repo

    let save_inode idx import snapshot =
      let open Lwt_syntax in
      let* key = Snapshot.Import.save_elt import snapshot in
      Store.Tree.of_key idx.repo (`Node key)

    let close_import import index = Snapshot.Import.close import index.repo

    let make_context index = empty index

    let update_context context tree = {context with tree}

    let add_hash (Batch (repo, _, _)) tree key hash =
      let open Lwt_syntax in
      let irmin_hash =
        match hash with `Blob hash -> `Contents (hash, ()) | `Node _ as n -> n
      in
      let* o = Store.Tree.of_hash repo irmin_hash in
      match o with
      | None -> Lwt.return_none
      | Some t ->
          let+ v = Store.Tree.add_tree tree key (t :> tree) in
          Some v

    let add_bytes (Batch (_, t, _)) bytes =
      let open Lwt_syntax in
      (* Save the contents in the store *)
      let+ _ = Store.save_contents t bytes in
      Store.Tree.of_contents bytes

    let add_dir batch l =
      let open Lwt_result_syntax in
      let add sub_tree (step, hash) =
        match sub_tree with
        | None -> Lwt.return_some (Store.Tree.empty ())
        | Some sub_tree -> add_hash batch sub_tree [step] hash
      in
      let* o = Seq_es.S.fold_left add (Some (Store.Tree.empty ())) l in
      match o with
      | None -> return_none
      | Some tree ->
          let (Batch (repo, x, y)) = batch in
          (* Save the node in the store ... *)
          let*! _ = Store.save_tree ~clear:true repo x y tree in
          return_some tree

    module Commit_hash = Context_hash
    module Block_header = Block_header
  end

  (* Context dumper *)

  open Tezos_context_dump
  module Context_dumper = Context_dump.Make (Dumpable_context)

  let restore_context idx ~expected_context_hash ~nb_context_elements ~fd
      ~in_memory ~progress_display_mode =
    Context_dumper.restore_context_fd
      idx
      ~in_memory
      ~expected_context_hash
      ~fd
      ~nb_context_elements
      ~progress_display_mode
end
