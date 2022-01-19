(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
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

open Tezos_context_encoding.Context
module Store =
  Irmin_pack_mem.Make (Node) (Commit) (Conf) (Metadata) (Contents) (Path)
    (Branch)
    (Hash)
module Tree = Tezos_context_helpers.Context.Make_tree (Store)
include Tree
include Tezos_context_helpers.Context.Make_proof (Store)

type index = Store.repo

type context = {repo : index; parents : Store.Commit.t list; tree : Store.tree}

type t = context

type tree = Store.tree

type key = string list

type value = bytes

let index {repo; _} = repo

let exists index key =
  let open Lwt_syntax in
  let+ o = Store.Commit.of_hash index (Hash.of_context_hash key) in
  Option.is_some o

let checkout index key =
  let open Lwt_syntax in
  let* o = Store.Commit.of_hash index (Hash.of_context_hash key) in
  match o with
  | None -> Lwt.return_none
  | Some commit ->
      let tree = Store.Commit.tree commit in
      let ctxt = {repo = index; tree; parents = [commit]} in
      Lwt.return_some ctxt

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
  Store.Private.Repo.batch context.repo (fun x y _ ->
      List.iter_s
        (fun (s, k) ->
          match Store.Tree.destruct k with
          | `Contents _ -> Lwt.return ()
          | `Node _ ->
              let* tree = Store.Tree.get_tree context.tree [s] in
              let+ _ = Store.save_tree ~clear:true context.repo x y tree in
              ())
        children)

let raw_commit ~time ?(message = "") context =
  let open Lwt_syntax in
  let info =
    Irmin.Info.v ~date:(Time.Protocol.to_seconds time) ~author:"Tezos" message
  in
  let parents = List.map Store.Commit.hash context.parents in
  let* () = unshallow context in
  let+ h = Store.Commit.v context.repo ~info ~parents context.tree in
  Store.Tree.clear context.tree ;
  h

let hash ~time ?(message = "") context =
  let info =
    Irmin.Info.v ~date:(Time.Protocol.to_seconds time) ~author:"Tezos" message
  in
  let parents = List.map (fun c -> Store.Commit.hash c) context.parents in
  let node = Store.Tree.hash context.tree in
  let commit = Store.Private.Commit.Val.v ~parents ~node ~info in
  let x = Store.Private.Commit.Key.hash commit in
  Hash.to_context_hash x

let commit ~time ?message context =
  let open Lwt_syntax in
  let+ commit = raw_commit ~time ?message context in
  Hash.to_context_hash (Store.Commit.hash commit)

(*-- Generic Store Primitives ------------------------------------------------*)

let data_key key = "data" :: key

let mem ctxt key = Tree.mem ctxt.tree (data_key key)

let mem_tree ctxt key = Tree.mem_tree ctxt.tree (data_key key)

let list ctxt ?offset ?length key =
  Tree.list ctxt.tree ?offset ?length (data_key key)

let length ctxt key = Tree.length ctxt.tree key

let find ctxt key = Tree.find ctxt.tree (data_key key)

let raw_add ctxt key data =
  let open Lwt_syntax in
  let+ tree = Tree.add ctxt.tree key data in
  {ctxt with tree}

let add ctxt key data = raw_add ctxt (data_key key) data

let raw_remove ctxt k =
  let open Lwt_syntax in
  let+ tree = Tree.remove ctxt.tree k in
  {ctxt with tree}

let remove ctxt key = raw_remove ctxt (data_key key)

let find_tree ctxt key = Tree.find_tree ctxt.tree (data_key key)

let add_tree ctxt key tree =
  let open Lwt_syntax in
  let+ tree = Tree.add_tree ctxt.tree (data_key key) tree in
  {ctxt with tree}

let fold ?depth ctxt key ~order ~init ~f =
  Tree.fold ?depth ctxt.tree (data_key key) ~order ~init ~f

let current_protocol_key = ["protocol"]

let current_predecessor_block_metadata_hash_key =
  ["predecessor_block_metadata_hash"]

let current_predecessor_ops_metadata_hash_key =
  ["predecessor_ops_metadata_hash"]

let get_protocol ctxt =
  let open Lwt_syntax in
  let* o = Tree.find ctxt.tree current_protocol_key in
  match o with
  | None -> assert false
  | Some data -> Lwt.return (Protocol_hash.of_bytes_exn data)

let add_protocol ctxt key =
  let key = Protocol_hash.to_bytes key in
  raw_add ctxt current_protocol_key key

let get_hash_version _c = Context_hash.Version.of_int 0

let set_hash_version c v =
  if Context_hash.Version.(of_int 0 = v) then return c
  else fail (Tezos_context_helpers.Context.Unsupported_context_hash_version v)

let add_predecessor_block_metadata_hash v hash =
  let data =
    Data_encoding.Binary.to_bytes_exn Block_metadata_hash.encoding hash
  in
  raw_add v current_predecessor_block_metadata_hash_key data

let add_predecessor_ops_metadata_hash v hash =
  let data =
    Data_encoding.Binary.to_bytes_exn
      Operation_metadata_list_list_hash.encoding
      hash
  in
  raw_add v current_predecessor_ops_metadata_hash_key data

let create () =
  let open Lwt_syntax in
  let cfg = Irmin_pack.config "/tmp" in
  let promise =
    let* repo = Store.Repo.v cfg in
    Lwt.return {repo; parents = []; tree = Store.Tree.empty ()}
  in
  match Lwt.state promise with
  | Lwt.Return result -> result
  | Lwt.Fail exn -> raise exn
  | Lwt.Sleep ->
      (* The in-memory context should never block *)
      assert false

let empty = create ()

let concrete_encoding : Store.Tree.concrete Data_encoding.t =
  let open Data_encoding in
  mu "memory_context" (fun encoding ->
      let map_encoding = list (tup2 string encoding) in
      union
        [
          case
            ~title:"tree"
            (Tag 0)
            map_encoding
            (function `Tree map -> Some map | `Contents _ -> None)
            (fun map -> `Tree map);
          case
            ~title:"value"
            (Tag 1)
            bytes
            (function `Contents (v, _) -> Some v | `Tree _ -> None)
            (fun v -> `Contents (v, ()));
        ])

let encoding : t Data_encoding.t =
  Data_encoding.conv
    (fun t ->
      let tree = Store.Tree.to_concrete t.tree in
      let tree =
        (* This is safe as store.Tree will never call any blocking
           functions. *)
        match Lwt.state tree with Return t -> t | _ -> assert false
      in
      tree)
    (fun t ->
      let tree = Store.Tree.of_concrete t in
      let ctxt = create () in
      {ctxt with tree})
    concrete_encoding

let current_test_chain_key = ["test_chain"]

let get_test_chain v =
  let open Lwt_syntax in
  let* o = Tree.find v.tree current_test_chain_key in
  match o with
  | None -> Lwt.fail (Failure "Unexpected error (Context.get_test_chain)")
  | Some data -> (
      match Data_encoding.Binary.of_bytes Test_chain_status.encoding data with
      | Error re ->
          Format.kasprintf
            (fun s -> Lwt.fail (Failure s))
            "Error in Context.get_test_chain: %a"
            Data_encoding.Binary.pp_read_error
            re
      | Ok r -> Lwt.return r)

let add_test_chain v id =
  let id = Data_encoding.Binary.to_bytes_exn Test_chain_status.encoding id in
  raw_add v current_test_chain_key id
