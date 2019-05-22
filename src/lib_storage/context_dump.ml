(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

let current_version = "tezos-snapshot-1.0.0"

(*****************************************************************************)
module type Dump_interface = sig
  type index
  type context
  type tree
  type hash
  type step = string
  type key = step list
  type commit_info

  val commit_info_encoding : commit_info Data_encoding.t

  val hash_encoding : hash Data_encoding.t
  val blob_encoding : [ `Blob of MBytes.t ] Data_encoding.t
  val node_encoding : [ `Node of MBytes.t ] Data_encoding.t

  module Block_header : sig
    type t = Block_header.t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val equal : t -> t -> bool
    val encoding : t Data_encoding.t
  end

  module Pruned_block : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val header : t -> Block_header.t
    val encoding : t Data_encoding.t
  end

  module Block_data : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val header : t -> Block_header.t
    val encoding : t Data_encoding.t
  end

  module Protocol_data : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val encoding : t Data_encoding.t
  end

  module Commit_hash : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t tzresult
    val encoding : t Data_encoding.t
  end

  (* hash manipulation *)
  val hash_export : hash -> [ `Node | `Blob ] * MBytes.t
  val hash_import : [ `Node | `Blob ]  -> MBytes.t -> hash tzresult
  val hash_equal : hash -> hash -> bool

  (* commit manipulation (for parents) *)
  val context_parents : context -> Commit_hash.t list Lwt.t

  (* Commit info *)
  val context_info : context -> commit_info
  val context_info_export : commit_info -> ( Int64.t * string * string )
  val context_info_import : ( Int64.t * string * string ) -> commit_info

  (* block header manipulation *)
  val get_context : index -> Block_header.t -> context option Lwt.t
  val set_context :
    info:commit_info -> parents:Commit_hash.t list -> context ->
    Block_header.t ->
    Block_header.t option Lwt.t

  (* for dumping *)
  val context_tree : context -> tree
  val tree_hash : context -> tree -> hash Lwt.t
  val sub_tree : tree -> key -> tree option Lwt.t
  val tree_list : tree -> ( step * [`Contents|`Node] ) list Lwt.t
  val tree_content : tree -> MBytes.t option Lwt.t

  (* for restoring *)
  val make_context : index -> context
  val update_context : context -> tree -> context
  val add_hash : index -> tree -> key -> hash -> tree option Lwt.t
  val add_mbytes : index -> MBytes.t -> tree Lwt.t
  val add_dir : index -> ( step * hash ) list -> tree option Lwt.t

end

module type S = sig
  type index
  type context
  type block_header
  type block_data
  type pruned_block
  type protocol_data

  val dump_contexts_fd :
    index ->
    (block_header * block_data * History_mode.t *
     (block_header -> (pruned_block option * protocol_data option) tzresult Lwt.t)) ->
    fd:Lwt_unix.file_descr -> unit tzresult Lwt.t

  val restore_contexts_fd : index -> Raw_store.t -> fd:Lwt_unix.file_descr ->
    (pruned_block -> Block_hash.t -> unit tzresult Lwt.t) ->
    (block_header option ->
     Block_hash.t -> pruned_block -> unit tzresult Lwt.t) ->
    (block_header * block_data * History_mode.t *
     Block_header.t option * Block_hash.t list * protocol_data list) tzresult Lwt.t

end

type error += System_write_error of string
type error += Bad_hash of string * MBytes.t * MBytes.t
type error += Context_not_found of MBytes.t
type error += System_read_error of string
type error += Inconsistent_snapshot_file
type error += Inconsistent_snapshot_data
type error += Missing_snapshot_data
type error += Invalid_snapshot_version of string * string
type error += Restore_context_failure

let () = begin
  let open Data_encoding in

  register_error_kind `Permanent
    ~id:"Writing_error"
    ~title:"Writing error"
    ~description:"Cannot write in file for context dump"
    ~pp:(fun ppf s ->
        Format.fprintf ppf
          "Unable to write file for context dumping: %s" s
      )
    (obj1 (req "context_dump_no_space" string) )
    (function System_write_error s -> Some s
            | _ -> None)
    (fun s -> System_write_error s);

  register_error_kind `Permanent
    ~id:"Bad_hash"
    ~title:"Bad hash"
    ~description:"Wrong hash given"
    ~pp:(fun ppf ( ty, his, hshould ) ->
        Format.fprintf ppf
          "Wrong hash [%s] given: %s, should be %s"
          ty (MBytes.to_string his) (MBytes.to_string hshould))
    (obj3
       ( req "hash_ty" string )
       ( req "hash_is" bytes )
       ( req "hash_should" bytes ) )
    (function Bad_hash ( ty, his, hshould ) -> Some (ty, his, hshould )
            | _ -> None)
    (fun (ty, his, hshould) -> Bad_hash (ty, his,hshould));

  register_error_kind `Permanent
    ~id:"Context_not_found"
    ~title:"Context not found"
    ~description:"Cannot find context corresponding to hash"
    ~pp:(fun ppf mb ->
        Format.fprintf ppf
          "No context with hash: %s"
          (MBytes.to_string mb))
    (obj1 (req "context_not_found" bytes) )
    (function Context_not_found mb -> Some mb
            | _ -> None)
    (fun mb -> Context_not_found mb);

  register_error_kind `Permanent
    ~id:"System_read_error"
    ~title:"System read error"
    ~description:"Failed to read file"
    ~pp:(fun ppf uerr ->
        Format.fprintf ppf
          "Error while reading file for context dumping: %s" uerr)
    (obj1 (req "system_read_error" string) )
    (function System_read_error e -> Some e
            | _ -> None)
    (fun e -> System_read_error e);

  register_error_kind `Permanent
    ~id:"Inconsistent_snapshot_file"
    ~title:"Inconsistent snapshot file"
    ~description:"Error while opening snapshot file"
    ~pp:(fun ppf () ->
        Format.fprintf ppf
          "Failed to read snapshot file. The provided file is inconsistent.")
    empty
    (function Inconsistent_snapshot_file -> Some () | _ -> None)
    (fun () -> Inconsistent_snapshot_file);

  register_error_kind `Permanent
    ~id:"Inconsistent_snapshot_data"
    ~title:"Inconsistent snapshot data"
    ~description:"The data provided by the snapshot is inconsistent"
    ~pp:(fun ppf () ->
        Format.fprintf ppf
          "The data provided by the snapshot file is inconsistent (context_hash does not correspond for block).")
    empty
    (function Inconsistent_snapshot_data -> Some () | _ -> None)
    (fun () -> Inconsistent_snapshot_data);

  register_error_kind `Permanent
    ~id:"Missing_snapshot_data"
    ~title:"Missing data in imported snapshot"
    ~description:"Mandatory data missing while reaching end of snapshot file."
    ~pp:(fun ppf () ->
        Format.fprintf ppf
          "Mandatory data is missing is the provided snapshot file.")
    empty
    (function Missing_snapshot_data -> Some () | _ -> None)
    (fun () -> Missing_snapshot_data);

  register_error_kind `Permanent
    ~id:"Invalid_snapshot_version"
    ~title:"Invalid snapshot version"
    ~description:"The version of the snapshot to import is not valid"
    ~pp:begin fun ppf (found, expected) ->
      Format.fprintf ppf
        "The snapshot to import has version \"%s\" but \"%s\" was expected."
        found expected end
    (obj2
       (req "found" string)
       (req "expected" string))
    (function Invalid_snapshot_version (found, expected) ->
       Some (found, expected) | _ -> None)
    (fun (found, expected) -> Invalid_snapshot_version (found, expected));

  register_error_kind `Permanent
    ~id:"Restore_context_failure"
    ~title:"Failed to restore context"
    ~description:"Internal error while restoring the context"
    ~pp:(fun ppf () ->
        Format.fprintf ppf
          "Internal error while restoring the context.")
    empty
    (function Restore_context_failure -> Some () | _ -> None)
    (fun () -> Restore_context_failure);

end

module Make (I:Dump_interface) = struct

  type command =
    | Root of {
        block_header: I.Block_header.t ;
        info: I.commit_info ;
        parents: I.Commit_hash.t list ;
        block_data : I.Block_data.t ;
      }
    | Node of (string * I.hash) list
    | Blob of MBytes.t
    | Proot of I.Pruned_block.t
    | Loot of I.Protocol_data.t
    | End

  (* Command encoding. *)

  let blob_encoding =
    let open Data_encoding in
    case ~title:"blob" (Tag (Char.code 'b'))
      bytes
      (function Blob bytes -> Some bytes | _ -> None)
      (function bytes -> Blob bytes)

  let node_encoding =
    let open Data_encoding in
    case ~title:"node" (Tag (Char.code 'd'))
      (list (obj2
               (req "name" string)
               (req "hash" I.hash_encoding)
            ))
      (function Node x -> Some x | _ -> None)
      (function x -> Node x)

  let end_encoding =
    let open Data_encoding in
    case ~title:"end" (Tag (Char.code 'e'))
      empty
      (function End -> Some () | _ -> None)
      (fun () -> End)

  let loot_encoding =
    let open Data_encoding in
    case ~title:"loot" (Tag (Char.code 'l'))
      I.Protocol_data.encoding
      (function
        | Loot protocol_data -> Some protocol_data
        | _ -> None)
      (fun protocol_data ->
         Loot protocol_data)

  let proot_encoding =
    let open Data_encoding in
    case ~title:"proot" (Tag (Char.code 'p'))
      (obj1 (req "pruned_block" I.Pruned_block.encoding))
      (function
        | Proot pruned_block ->
            Some pruned_block
        | _ -> None)
      (fun pruned_block ->
         Proot pruned_block)

  let root_encoding =
    let open Data_encoding in
    case ~title:"root" (Tag (Char.code 'r'))
      (obj4
         (req "block_header" (dynamic_size I.Block_header.encoding))
         (req "info" I.commit_info_encoding)
         (req "parents" (list I.Commit_hash.encoding))
         (req "block_data" I.Block_data.encoding)
      )
      (function
        | Root { block_header ; info ; parents ; block_data } ->
            Some (block_header, info, parents, block_data)
        | _ -> None)
      (fun (block_header, info, parents, block_data) ->
         Root { block_header ; info ; parents ; block_data })

  let command_encoding = Data_encoding.union ~tag_size:`Uint8 [
      blob_encoding ;
      node_encoding ;
      end_encoding ;
      loot_encoding ;
      proot_encoding ;
      root_encoding ;
    ]

  (* IO toolkit. *)

  let rec read_string rbuf ~len =
    let fd, buf, ofs, total = !rbuf in
    if Bytes.length buf - ofs < len then
      let blen = Bytes.length buf - ofs in
      let neu = Bytes.create (blen + 1_000_000) in
      Bytes.blit buf ofs neu 0 blen ;
      Lwt_unix.read fd neu blen 1_000_000 >>= fun bread ->
      total := !total + bread ;
      if bread = 0 then
        fail Inconsistent_snapshot_file
      else
        let neu = if bread <> 1_000_000 then Bytes.sub neu 0 (blen + bread) else neu in
        rbuf := (fd, neu, 0, total) ;
        read_string rbuf ~len
    else
      let res = Bytes.sub_string buf ofs len in
      rbuf := (fd, buf, ofs + len, total) ;
      return res

  let read_mbytes rbuf b =
    read_string rbuf ~len:(MBytes.length b) >>=? fun string ->
    MBytes.blit_of_string string 0 b 0 (MBytes.length b) ;
    return ()

  let set_int64 buf i =
    let b = Bytes.create 8 in
    EndianBytes.BigEndian.set_int64 b 0 i;
    Buffer.add_bytes buf b

  let get_int64 rbuf =
    read_string ~len:8 rbuf >>=? fun s ->
    return @@ EndianString.BigEndian.get_int64 s 0

  let set_mbytes buf b =
    set_int64 buf (Int64.of_int (MBytes.length b)) ;
    Buffer.add_bytes buf (MBytes.to_bytes b)

  let get_mbytes rbuf =
    get_int64 rbuf >>|? Int64.to_int >>=? fun l ->
    let b = MBytes.create l in
    read_mbytes rbuf b >>=? fun () ->
    return b

  (* Getter and setters *)

  let get_command rbuf =
    get_mbytes rbuf >>|? fun bytes ->
    Data_encoding.Binary.of_bytes_exn command_encoding bytes

  let set_root buf block_header info parents block_data =
    let root = Root { block_header ; info ; parents ; block_data ; } in
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding root in
    set_mbytes buf bytes

  let set_node buf contents =
    let bytes =
      Data_encoding.Binary.to_bytes_exn command_encoding (Node contents) in
    set_mbytes buf bytes

  let set_blob buf data =
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding (Blob data) in
    set_mbytes buf bytes

  let set_proot buf pruned_block =
    let proot = Proot pruned_block in
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding proot in
    set_mbytes buf bytes

  let set_loot buf protocol_data =
    let loot = Loot protocol_data in
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding loot in
    set_mbytes buf bytes

  let set_end buf =
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding End in
    set_mbytes buf bytes

  (* Snapshot metadata *)

  (* TODO add more info (e.g. nb context item, nb blocks, etc.) *)
  type snapshot_metadata = {
    version : string ;
    mode : Tezos_shell_services.History_mode.t ;
  }

  let snapshot_metadata_encoding =
    let open Data_encoding in
    conv
      (fun { version ; mode } -> (version, mode))
      (fun (version, mode) -> { version ; mode })
      (obj2
         (req "version" string)
         (req "mode" Tezos_shell_services.History_mode.encoding))

  let write_snapshot_metadata ~mode buf =
    let version = {
      version = current_version ;
      mode = mode ;
    } in
    let bytes =
      Data_encoding.(Binary.to_bytes_exn snapshot_metadata_encoding version) in
    set_mbytes buf bytes

  let read_snapshot_metadata rbuf =
    get_mbytes rbuf >>|? fun bytes ->
    Data_encoding.(Binary.of_bytes_exn snapshot_metadata_encoding) bytes

  let check_version v =
    fail_when
      (v.version <> current_version)
      (Invalid_snapshot_version (v.version, current_version))

  let dump_contexts_fd idx data ~fd =
    (* Dumping *)
    let buf = Buffer.create 1_000_000 in
    let written = ref 0 in

    let flush () =
      let contents = Buffer.contents buf in
      Buffer.clear buf ;
      written := !written + String.length contents ;
      Lwt_utils_unix.write_string fd contents in

    let maybe_flush () =
      if Buffer.length buf > 1_000_000 then flush () else Lwt.return_unit in

    (* Noting the visited hashes *)
    let visited_hash = Hashtbl.create 1000 in
    let visited h = Hashtbl.mem visited_hash h in
    let set_visit h = Hashtbl.add visited_hash h () in

    (* Folding through a node *)
    let fold_tree_path ctxt tree =
      let cpt = ref 0 in
      let rec fold_tree_path ctxt tree =
        I.tree_list tree >>= fun keys ->
        let keys = List.sort (fun (a,_) (b,_) -> String.compare a b) keys in
        Lwt_list.map_s
          begin fun (name, kind) ->
            I.sub_tree tree [name] >>= function
            | None -> assert false
            | Some sub_tree ->
                I.tree_hash ctxt sub_tree >>= fun hash ->
                begin
                  if visited hash then Lwt.return_unit
                  else
                    begin
                      Tezos_stdlib.Utils.display_progress
                        ~refresh_rate:(!cpt, 1_000)
                        "Context: %dK elements, %dMiB written%!"
                        (!cpt / 1_000) (!written / 1_048_576) ;
                      incr cpt ;
                      set_visit hash; (* There cannot be a cycle *)
                      match kind with
                      | `Node ->
                          fold_tree_path ctxt sub_tree
                      | `Contents ->
                          begin I.tree_content sub_tree >>= function
                            | None ->
                                assert false
                            | Some data ->
                                set_blob buf data ;
                                maybe_flush ()
                          end
                    end
                end >>= fun () ->
                Lwt.return (name, hash)
          end
          keys >>= fun sub_keys ->
        set_node buf sub_keys;
        maybe_flush ()
      in
      fold_tree_path ctxt tree
    in
    Lwt.catch begin fun () ->
      let bh, block_data, mode, pruned_iterator = data in
      write_snapshot_metadata ~mode buf ;
      I.get_context idx bh >>= function
      | None ->
          fail @@ Context_not_found (I.Block_header.to_bytes bh)
      | Some ctxt ->
          let tree = I.context_tree ctxt in
          fold_tree_path ctxt tree >>= fun () ->
          Tezos_stdlib.Utils.display_progress_end ();
          I.context_parents ctxt >>= fun parents ->
          set_root buf bh (I.context_info ctxt) parents block_data;
          (* Dump pruned blocks *)
          let dump_pruned cpt pruned =
            Tezos_stdlib.Utils.display_progress
              ~refresh_rate:(cpt, 1_000)
              "History: %dK block, %dMiB written"
              (cpt / 1_000) (!written / 1_048_576) ;
            set_proot buf pruned;
            maybe_flush () in
          let rec aux cpt acc header =
            pruned_iterator header >>=? function
            | (None, None) -> return acc (* assert false *)
            | (None, Some protocol_data) ->
                return (protocol_data :: acc)
            | (Some pred_pruned, Some protocol_data) ->
                dump_pruned cpt pred_pruned >>= fun () ->
                aux (succ cpt) (protocol_data :: acc)
                  (I.Pruned_block.header pred_pruned)
            | (Some pred_pruned, None) ->
                dump_pruned cpt pred_pruned >>= fun () ->
                aux (succ cpt) acc
                  (I.Pruned_block.header pred_pruned)
          in
          let starting_block_header = I.Block_data.header block_data in
          aux 0 [] starting_block_header >>=? fun protocol_datas ->
          (* Dump protocol data *)
          Lwt_list.iter_s (fun proto ->
              set_loot buf proto;
              maybe_flush () ;
            ) protocol_datas >>= fun () ->
          Tezos_stdlib.Utils.display_progress_end ();
          return_unit >>=? fun () ->
          set_end buf;
          flush () >>= fun () ->
          return_unit
    end
      begin function
        | Unix.Unix_error (e,_,_) ->
            fail @@ System_write_error (Unix.error_message e)
        | err -> Lwt.fail err
      end

  (* Restoring *)

  let restore_contexts_fd index store ~fd k_store_pruned_block block_validation =

    let read = ref 0 in
    let rbuf = ref (fd, Bytes.empty, 0, read) in

    (* Editing the repository *)
    let add_blob blob =
      I.add_mbytes index blob >>= fun tree ->
      return tree
    in

    let add_dir keys =
      I.add_dir index keys >>= function
      | None -> fail Restore_context_failure
      | Some tree -> return tree
    in

    let restore history_mode =
      let rec first_pass ctxt cpt =
        Tezos_stdlib.Utils.display_progress
          ~refresh_rate:(cpt, 1_000)
          "Context: %dK elements, %dMiB read"
          (cpt / 1_000) (!read / 1_048_576) ;
        get_command rbuf >>=? function
        | Root { block_header ; info ; parents ; block_data } ->
            begin I.set_context ~info ~parents ctxt block_header >>= function
              | None -> fail Inconsistent_snapshot_data
              | Some block_header ->
                  return (block_header, block_data)
            end
        | Node contents ->
            add_dir contents >>=? fun tree ->
            first_pass (I.update_context ctxt tree) (cpt + 1)
        | Blob data ->
            add_blob data >>=? fun tree ->
            first_pass (I.update_context ctxt tree) (cpt + 1)
        | _ -> fail Inconsistent_snapshot_data in

      let rec second_pass pred_header (rev_block_hashes, protocol_datas) todo cpt =
        Tezos_stdlib.Utils.display_progress
          ~refresh_rate:(cpt, 1_000)
          "Store: %dK elements, %dMiB read"
          (cpt / 1_000) (!read / 1_048_576) ;
        get_command rbuf >>=? function
        | Proot pruned_block ->
            let header = I.Pruned_block.header pruned_block in
            let hash = Block_header.hash header in
            block_validation pred_header hash pruned_block >>=? fun () ->
            begin if (cpt + 1) mod 5_000 = 0 then
                Raw_store.with_atomic_rw store begin fun () ->
                  Error_monad.iter_s begin fun (hash, pruned_block) ->
                    k_store_pruned_block pruned_block hash
                  end ((hash, pruned_block) :: todo)
                end >>=? fun () ->
                second_pass (Some header)
                  (hash :: rev_block_hashes, protocol_datas) [] (cpt + 1)
              else
                second_pass (Some header)
                  (hash :: rev_block_hashes, protocol_datas) ((hash, pruned_block) :: todo) (cpt + 1)
            end
        | Loot protocol_data ->
            Raw_store.with_atomic_rw store begin fun () ->
              Error_monad.iter_s (fun (hash, pruned_block) ->
                  k_store_pruned_block pruned_block hash) todo
            end >>=? fun () ->
            second_pass pred_header (rev_block_hashes, protocol_data :: protocol_datas) todo (cpt + 1)
        | End ->
            return (pred_header, rev_block_hashes, List.rev protocol_datas)
        | _ -> fail Inconsistent_snapshot_data in
      first_pass (I.make_context index) 0 >>=? fun (block_header, block_data) ->
      Tezos_stdlib.Utils.display_progress_end () ;
      second_pass None ([], []) [] 0 >>=? fun (oldest_header_opt, rev_block_hashes, protocol_datas) ->
      Tezos_stdlib.Utils.display_progress_end () ;
      return (block_header,
              block_data,
              history_mode,
              oldest_header_opt,
              rev_block_hashes,
              protocol_datas)
    in
    (* Check snapshot version *)
    read_snapshot_metadata rbuf >>=? fun version ->
    check_version version >>=? fun () ->
    Lwt.catch begin fun () ->
      restore version.mode
    end
      begin function
        | Unix.Unix_error (e,_,_) ->
            fail @@ System_read_error (Unix.error_message e)
        | err -> Lwt.fail err
      end
end
