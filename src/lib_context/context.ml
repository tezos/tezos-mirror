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

(* Caps the number of entries stored in the Irmin's index. As a
   trade-off, increasing this value will delay index merges, and thus,
   make them more expensive in terms of disk usage, memory usage and
   computation time.*)
let index_log_size = ref 2_500_000

(* Caps the number of entries stored in the Irmin's LRU cache. As a
   trade-off, increasing this value will increase the memory
   consumption.*)
let lru_size = ref 5_000

(* This limit ensures that no trees with more than [auto_flush]
   mutations can exist in memory, bounding the memory usage of a
   single commit performed by a read-write process. As a trade-off,
   the intermediate flushed trees to the store might be unused and
   will have to be garbage collected later on to save space. *)
let auto_flush = ref 10_000

module Indexing_strategy : sig
  (* Determines the policy used to determine whether to add new
     objects to Irmin's index whenever they are exported to the
     data file. *)
  type t :=
    [ `Minimal  (** only newly-exported commit objects are added to the index *)
    | `Always  (** all newly-exported objects are added to the index *) ]

  val parse : string -> (t, string) result

  val set : t -> unit

  val get : unit -> t

  type irmin_t := Irmin_pack.Pack_store.Indexing_strategy.t

  val to_irmin : t -> irmin_t
end = struct
  module I = Irmin_pack.Pack_store.Indexing_strategy

  let singleton = ref `Minimal

  let set x = singleton := x

  let get () = !singleton

  let parse = function
    | "always" -> Ok `Always
    | "minimal" -> Ok `Minimal
    | x ->
        Error
          (Fmt.str
             "Unable to parse indexing strategy '%s'. Expected one of { \
              'always', 'minimal' }."
             x)

  let to_irmin = function `Always -> I.always | `Minimal -> I.minimal
end

let () =
  let verbose_info () =
    Logs.set_level (Some Logs.Info) ;
    Logs.set_reporter (reporter ())
  in
  let verbose_debug () =
    Logs.set_level (Some Logs.Debug) ;
    Logs.set_reporter (reporter ())
  in
  let index_log_size n = index_log_size := int_of_string n in
  let auto_flush n = auto_flush := int_of_string n in
  let lru_size n = lru_size := int_of_string n in
  let indexing_strategy x =
    match Indexing_strategy.parse x with
    | Ok x -> Indexing_strategy.set x
    | Error msg ->
        Fmt.failwith
          "Invalid value for TEZOS_CONTEXT environment variable: %s"
          msg
  in
  match Unix.getenv "TEZOS_CONTEXT" with
  | exception Not_found -> ()
  | v ->
      let args = String.split ',' v in
      List.iter
        (function
          | "v" | "verbose" -> verbose_info ()
          | "vv" -> verbose_debug ()
          | v -> (
              match String.split '=' v with
              | ["index-log-size"; n] -> index_log_size n
              | ["auto-flush"; n] -> auto_flush n
              | ["lru-size"; n] -> lru_size n
              | ["indexing-strategy"; x] -> indexing_strategy x
              | _ -> ()))
        args

module Make (Encoding : module type of Tezos_context_encoding.Context) = struct
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

  module type S = Tezos_context_sigs.Context.S

  (*-- Version Access and Update -----------------------------------------------*)

  let current_protocol_key = ["protocol"]

  let current_test_chain_key = ["test_chain"]

  let current_data_key = ["data"]

  let current_predecessor_block_metadata_hash_key =
    ["predecessor_block_metadata_hash"]

  let current_predecessor_ops_metadata_hash_key =
    ["predecessor_ops_metadata_hash"]

  let sync index =
    if index.readonly then Store.sync index.repo ;
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
    let open Lwt_tzresult_syntax in
    if Context_hash.Version.(of_int 0 = v) then return c
    else fail (Tezos_context_helpers.Context.Unsupported_context_hash_version v)

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

  (*-- Generic Store Primitives ------------------------------------------------*)

  let data_key key = current_data_key @ key

  type key = string list

  type value = bytes

  type tree = Store.tree

  type kinded_key = [`Node of Store.node_key | `Value of Store.contents_key]

  module Tree = Tezos_context_helpers.Context.Make_tree (Conf) (Store)
  include Tezos_context_helpers.Context.Make_config (Conf)
  include Tezos_context_helpers.Context.Make_proof (Store) (Conf)

  let mem ctxt key = Tree.mem ctxt.tree (data_key key)

  let mem_tree ctxt key = Tree.mem_tree ctxt.tree (data_key key)

  let raw_find ctxt key = Tree.find ctxt.tree key

  let list ctxt ?offset ?length key =
    Tree.list ctxt.tree ?offset ?length (data_key key)

  let length ctxt key = Tree.length ctxt.tree key

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
    if (not context.index.readonly) && context.ops >= !auto_flush then
      flush context
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
        Block_services.Key v
    | `Node _ ->
        let* kvs = Store.Tree.list tree [] in
        let f acc (key, _) =
          (* get_tree is safe, because we iterate over keys *)
          let* tree = Store.Tree.get_tree tree [key] in
          let+ sub_raw_context = tree_to_raw_context tree in
          String.Map.add key sub_raw_context acc
        in
        let+ res = List.fold_left_s f String.Map.empty kvs in
        Block_services.Dir res

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
      Tezos_context_memory.Context.(Tree.empty empty)

  let to_memory_tree (ctxt : t) (key : string list) :
      Tezos_context_memory.Context.tree option Lwt.t =
    let open Lwt_syntax in
    let* ctxt_tree = find_tree ctxt key in
    Option.map_s tree_to_memory_tree ctxt_tree

  let merkle_hash tree =
    let merkle_hash_kind =
      match Store.Tree.destruct tree with
      | `Contents _ -> Block_services.Contents
      | `Node _ -> Block_services.Node
    in
    let hash_str = Store.Tree.hash tree |> merkle_hash_to_string in
    Block_services.Hash (merkle_hash_kind, hash_str)

  let merkle_tree t leaf_kind key =
    let open Lwt_syntax in
    let* subtree_opt = Store.Tree.find_tree t.tree (data_key []) in
    match subtree_opt with
    | None -> Lwt.return String.Map.empty
    | Some subtree ->
        let key_to_string k = String.concat ";" k in
        let rec key_to_merkle_tree t target =
          match (Store.Tree.destruct t, target) with
          | (_, []) ->
              (* We cannot use this case as the base case, because a merkle_node
                 is a map from string to something. In this case, we have
                 no key to put in the map's domain. *)
              raise
                (Invalid_argument
                   (Printf.sprintf "Reached end of key (top-level key was: %s)"
                   @@ key_to_string key))
          | (_, [hd]) ->
              let finally key =
                (* get_tree is safe because we iterate on keys *)
                let* tree = Store.Tree.get_tree t [key] in
                if key = hd then
                  (* on the target path: the final leaf *)
                  match leaf_kind with
                  | Block_services.Hole -> Lwt.return @@ merkle_hash tree
                  | Block_services.Raw_context ->
                      let+ raw_context = tree_to_raw_context tree in
                      Block_services.Data raw_context
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
          | (`Node _, target_hd :: target_tl) ->
              let continue key =
                (* get_tree is safe because we iterate on keys *)
                let* tree = Store.Tree.get_tree t [key] in
                if key = target_hd then
                  (* on the target path: recurse *)
                  let+ sub = key_to_merkle_tree tree target_tl in
                  Block_services.Continue sub
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
          | (`Contents _, _) ->
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
      Tree.find t current_predecessor_block_metadata_hash_key >>= function
      | None -> Lwt.return_none
      | Some data -> (
          match
            Data_encoding.Binary.of_bytes_opt Block_metadata_hash.encoding data
          with
          | None ->
              Lwt.fail
                (Failure
                   "Unexpected error \
                    (Context.get_predecessor_block_metadata_hash)")
          | Some r -> Lwt.return_some r)

    let add_predecessor_block_metadata_hash t hash =
      let data =
        Data_encoding.Binary.to_bytes_exn Block_metadata_hash.encoding hash
      in
      Tree.add t current_predecessor_block_metadata_hash_key data

    let find_predecessor_ops_metadata_hash t =
      Tree.find t current_predecessor_ops_metadata_hash_key >>= function
      | None -> Lwt.return_none
      | Some data -> (
          match
            Data_encoding.Binary.of_bytes_opt
              Operation_metadata_list_list_hash.encoding
              data
          with
          | None ->
              Lwt.fail
                (Failure
                   "Unexpected error \
                    (Context.get_predecessor_ops_metadata_hash)")
          | Some r -> Lwt.return_some r)

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
    tree_add ctxt.tree v >|= fun tree -> incr_ops {ctxt with tree}

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

  let init ?patch_context ?(readonly = false) ?indexing_strategy root =
    let open Lwt_syntax in
    let+ repo =
      let indexing_strategy =
        Option.value indexing_strategy ~default:(Indexing_strategy.get ())
        |> Indexing_strategy.to_irmin
      in
      Store.Repo.v
        (Irmin_pack.config
           ~readonly
           ~indexing_strategy
           ~index_log_size:!index_log_size
           ~lru_size:!lru_size
           root)
    in
    {path = root; repo; patch_context; readonly}

  let close index = Store.Repo.close index.repo

  let get_branch chain_id = Format.asprintf "%a" Chain_id.pp chain_id

  let empty index = {index; tree = Store.Tree.empty (); parents = []; ops = 0}

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
            | (`Blob, h) ->
                `Blob (Hash.of_context_hash (Context_hash.of_bytes_exn h))
            | (`Node, h) ->
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
          (fun h -> Hash.of_context_hash h |> Irmin_pack.Pack_key.v_indexed)
          parents
      in
      let+ c = Store.Commit.v ctxt.index.repo ~info ~parents ctxt.tree in
      let h = Store.Commit.hash c in
      Context_hash.equal context_hash (Hash.to_context_hash h)

    let context_tree ctxt = ctxt.tree

    type binding = {
      key : string;
      value : tree;
      value_kind : [`Node | `Contents];
      value_hash : hash;
    }

    (** Unpack the bindings in a tree node (in lexicographic order) and clear its
       internal cache. *)
    let bindings tree : binding list Lwt.t =
      let open Lwt_syntax in
      let* keys = Store.Tree.list tree [] in
      let+ bindings =
        keys
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
        |> List.map_s (fun (key, value) ->
               let+ o = Store.Tree.kind value [] in
               match o with
               | None ->
                   (* The value must exist in the tree, because we're
                       iterating over existing keys *)
                   assert false
               | Some value_kind ->
                   let value_hash = Store.Tree.hash value in
                   {key; value; value_kind; value_hash})
      in
      Store.Tree.clear tree ;
      bindings

    module Hashset = struct
      module String_set = Utils.String_set

      let create () =
        String_set.create ~elt_length:Hash.hash_size ~initial_capacity:100_000

      let mem t h = String_set.mem t (Hash.to_raw_string h)

      let add t h = String_set.add t (Hash.to_raw_string h)
    end

    let tree_iteri_unique f tree =
      let open Lwt_syntax in
      let total_visited = ref 0 in
      (* Noting the visited hashes *)
      let visited_hash = Hashset.create () in
      let visited h = Hashset.mem visited_hash h in
      let set_visit h =
        incr total_visited ;
        Hashset.add visited_hash h
      in
      let rec aux : type a. tree -> (unit -> a) -> a Lwt.t =
       fun tree k ->
        let* bs = bindings tree in
        let* sub_keys =
          List.map_s
            (fun {key; value; value_hash; value_kind} ->
              let kinded_value_hash =
                match value_kind with
                | `Node -> `Node value_hash
                | `Contents -> `Blob value_hash
              in
              let kv = (key, kinded_value_hash) in
              if visited value_hash then Lwt.return kv
              else
                match value_kind with
                | `Node ->
                    (* Visit children first, in left-to-right order. *)
                    (aux [@ocaml.tailcall]) value (fun () ->
                        (* There cannot be a cycle. *)
                        set_visit value_hash ;
                        kv)
                | `Contents ->
                    let* data = Store.Tree.get value [] in
                    let+ () = f (`Leaf data) in
                    set_visit value_hash ;
                    kv)
            bs
        in
        let+ v = f (`Branch sub_keys) in
        k v
      in
      let* () = aux tree Fun.id in
      Lwt.return !total_visited

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
      let open Lwt_tzresult_syntax in
      let add sub_tree (step, hash) =
        match sub_tree with
        | None -> Lwt.return_some (Store.Tree.empty ())
        | Some sub_tree -> add_hash batch sub_tree [step] hash
      in
      let* o = Seq_es.fold_left_s add (Some (Store.Tree.empty ())) l in
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

  (* Protocol data *)

  let data_node_hash context =
    let open Lwt_syntax in
    let+ tree = Store.Tree.get_tree context.tree current_data_key in
    Hash.to_context_hash (Store.Tree.hash tree)

  let retrieve_commit_info index block_header =
    let open Lwt_syntax in
    let* context = checkout_exn index block_header.Block_header.shell.context in
    let irmin_info = Dumpable_context.context_info context in
    let author = Info.author irmin_info in
    let message = Info.message irmin_info in
    let timestamp = Time.Protocol.of_seconds (Info.date irmin_info) in
    let* protocol_hash = get_protocol context in
    let* test_chain_status = get_test_chain context in
    let* predecessor_block_metadata_hash =
      find_predecessor_block_metadata_hash context
    in
    let* predecessor_ops_metadata_hash =
      find_predecessor_ops_metadata_hash context
    in
    let* data_key = data_node_hash context in
    let parents_contexts = Dumpable_context.context_parents context in
    return_ok
      ( protocol_hash,
        author,
        message,
        timestamp,
        test_chain_status,
        data_key,
        predecessor_block_metadata_hash,
        predecessor_ops_metadata_hash,
        parents_contexts )

  let check_protocol_commit_consistency ~expected_context_hash
      ~given_protocol_hash ~author ~message ~timestamp ~test_chain_status
      ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash
      ~data_merkle_root ~parents_contexts =
    let open Lwt_syntax in
    let data_merkle_root = Hash.of_context_hash data_merkle_root in
    let parents = List.map Hash.of_context_hash parents_contexts in
    let info = Info.v ~author (Time.Protocol.to_seconds timestamp) ~message in
    let tree = Store.Tree.empty () in
    let* tree = Root_tree.add_test_chain tree test_chain_status in
    let* tree = Root_tree.add_protocol tree given_protocol_hash in
    let* tree =
      Option.fold
        predecessor_block_metadata_hash
        ~none:(Lwt.return tree)
        ~some:(Root_tree.add_predecessor_block_metadata_hash tree)
    in
    let* tree =
      Option.fold
        predecessor_ops_metadata_hash
        ~none:(Lwt.return tree)
        ~some:(Root_tree.add_predecessor_ops_metadata_hash tree)
    in
    let data_t = Store.Tree.pruned (`Node data_merkle_root) in
    let+ new_tree = Store.Tree.add_tree tree current_data_key data_t in
    let node = Store.Tree.hash new_tree in
    let ctxt_h =
      P.Commit_portable.v ~info ~parents ~node
      |> Commit_hash.hash |> Hash.to_context_hash
    in
    Context_hash.equal ctxt_h expected_context_hash

  (* Context dumper *)

  module Context_dumper = Context_dump.Make (Dumpable_context)

  (* provides functions dump_context and restore_context *)
  let dump_context idx data ~fd =
    let open Lwt_syntax in
    let* res = Context_dumper.dump_context_fd idx data ~context_fd:fd in
    let* () = Lwt_unix.fsync fd in
    Lwt.return res

  let restore_context idx ~expected_context_hash ~nb_context_elements ~fd =
    Context_dumper.restore_context_fd
      idx
      ~expected_context_hash
      ~fd
      ~nb_context_elements
end
