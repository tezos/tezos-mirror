(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
(* Copyright (c) 2018-2020 Tarides <contact@tarides.com>                     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

include Context_dump_intf

(*****************************************************************************)

type error +=
  | System_write_error of string
  | Context_not_found of Bytes.t
  | System_read_error of string
  | Inconsistent_context_dump
  | Restore_context_failure

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"context_dump.writing_error"
    ~title:"Writing error"
    ~description:"Cannot write in file for context dump"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "Unable to write file for context dumping: %s" s)
    (obj1 (req "context_dump_no_space" string))
    (function System_write_error s -> Some s | _ -> None)
    (fun s -> System_write_error s) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.context_not_found"
    ~title:"Context not found"
    ~description:"Cannot find context corresponding to hash"
    ~pp:(fun ppf mb ->
      Format.fprintf ppf "No context with hash: %s" (Bytes.to_string mb))
    (obj1 (req "context_not_found" bytes))
    (function Context_not_found mb -> Some mb | _ -> None)
    (fun mb -> Context_not_found mb) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.system_read_error"
    ~title:"System read error"
    ~description:"Failed to read file"
    ~pp:(fun ppf uerr ->
      Format.fprintf ppf "Error while reading file for context dumping: %s" uerr)
    (obj1 (req "system_read_error" string))
    (function System_read_error e -> Some e | _ -> None)
    (fun e -> System_read_error e) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.inconsistent_context_dump"
    ~title:"Inconsistent context dump"
    ~description:"Error while reading context dump"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to read context dump. The provided file is inconsistent.")
    empty
    (function Inconsistent_context_dump -> Some () | _ -> None)
    (fun () -> Inconsistent_context_dump) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.restore_context_failure"
    ~title:"Failed to restore context"
    ~description:"Internal error while restoring the context"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Internal error while restoring the context.")
    empty
    (function Restore_context_failure -> Some () | _ -> None)
    (fun () -> Restore_context_failure)

(* IO toolkit. *)

let rec read_string rbuf ~len =
  let open Lwt_result_syntax in
  let (fd, buf, ofs, total) = !rbuf in
  if Bytes.length buf - ofs < len then (
    let blen = Bytes.length buf - ofs in
    let neu = Bytes.create (blen + 1_000_000) in
    Bytes.blit buf ofs neu 0 blen ;
    let*! bread = Lwt_unix.read fd neu blen 1_000_000 in
    total := !total + bread ;
    if bread = 0 then tzfail Inconsistent_context_dump
    else
      let neu =
        if bread <> 1_000_000 then Bytes.sub neu 0 (blen + bread) else neu
      in
      rbuf := (fd, neu, 0, total) ;
      read_string rbuf ~len)
  else
    let res = Bytes.sub_string buf ofs len in
    rbuf := (fd, buf, ofs + len, total) ;
    return res

let read_mbytes rbuf b =
  let open Lwt_result_syntax in
  let* string = read_string rbuf ~len:(Bytes.length b) in
  Bytes.blit_string string 0 b 0 (Bytes.length b) ;
  return ()

let set_int64 buf i =
  let b = Bytes.create 8 in
  EndianBytes.BigEndian.set_int64 b 0 i ;
  Buffer.add_bytes buf b

let get_int64 rbuf =
  let open Lwt_result_syntax in
  let* s = read_string ~len:8 rbuf in
  return @@ EndianString.BigEndian.get_int64 s 0

let flush written context_fd buf =
  let contents = Buffer.contents buf in
  Buffer.clear buf ;
  written := !written + String.length contents ;
  Lwt_utils_unix.write_string context_fd contents

let set_mbytes written context_fd buf b =
  set_int64 buf (Int64.of_int (Bytes.length b)) ;
  Buffer.add_bytes buf b ;
  if Buffer.length buf > 1_000_000 then flush written context_fd buf
  else Lwt.return_unit

module Make_legacy (I : Dump_interface) = struct
  type command =
    | Root
    | Node_seq of (string * I.Kinded_hash.t, error trace) Seq_es.t
    | Blob of bytes
    | Eoc of {info : I.commit_info; parents : I.Commit_hash.t list}
    | Eof

  (* Command encoding. *)

  let get_char rbuf =
    let open Lwt_result_syntax in
    let* s = read_string ~len:1 rbuf in
    return @@ EndianString.BigEndian.get_int8 s 0

  let get_int4 rbuf =
    let open Lwt_result_syntax in
    let* s = read_string ~len:4 rbuf in
    return @@ EndianString.BigEndian.get_int32 s 0

  (* To decode a variable size string we need to: 1/ read the length of the
     string, encoded on 4 bytes; 2/ reset the offset to the beginning of the string
     encoding. *)
  let get_length_and_reset_offset rbuf =
    let open Lwt_result_syntax in
    let* l = get_int4 rbuf in
    let length = Int32.to_int l in
    let (fd, buf, ofs, total) = !rbuf in
    rbuf := (fd, buf, ofs - 4, total) ;
    return (length + 4)

  let read_variable_length_string rbuf =
    let open Lwt_result_syntax in
    let* length_name = get_length_and_reset_offset rbuf in
    let b = Bytes.create length_name in
    let+ () = read_mbytes rbuf b in
    let name = Data_encoding.(Binary.of_bytes_exn string) b in
    (length_name, name)

  let read_fixed_length_hash rbuf =
    let open Lwt_result_syntax in
    let length_hash = 1 + 4 + 32 (*enum + size + hash*) in
    let b = Bytes.create length_hash in
    let+ () = read_mbytes rbuf b in
    let hash = Data_encoding.Binary.of_bytes_exn I.Kinded_hash.encoding b in
    (length_hash, hash)

  let read_seq rbuf total =
    let open Lwt_result_syntax in
    let step i =
      if i >= total then return_none
      else
        let* (length_name, name) = read_variable_length_string rbuf in
        let* (length_hash, hash) = read_fixed_length_hash rbuf in
        let node = (name, hash) in
        let i = i + length_name + length_hash in
        return_some (node, i)
    in
    Seq_es.unfold_es step 0

  let eoc_encoding_raw =
    let open Data_encoding in
    obj2
      (req "info" I.commit_info_encoding)
      (req "parents" (list I.Commit_hash.encoding))

  let get_command rbuf =
    let open Lwt_result_syntax in
    let* t = get_int64 rbuf in
    let total = Int64.to_int t in
    let* t = get_char rbuf in
    let tag = Char.chr t in
    let read_empty () =
      let len = total - 1 in
      let b = Bytes.create len in
      let+ () = read_mbytes rbuf b in
      Data_encoding.Binary.of_bytes_exn Data_encoding.empty b
    in
    match tag with
    | 'r' ->
        let+ () = read_empty () in
        Root
    | 'e' ->
        let+ () = read_empty () in
        Eof
    | 'c' ->
        let len = total - 1 in
        let b = Bytes.create len in
        let+ () = read_mbytes rbuf b in
        let (info, parents) =
          Data_encoding.Binary.of_bytes_exn eoc_encoding_raw b
        in
        Eoc {info; parents}
    | 'b' ->
        let len = total - 1 in
        let b = Bytes.create len in
        let+ () = read_mbytes rbuf b in
        let data = Data_encoding.Binary.of_bytes_exn Data_encoding.bytes b in
        Blob data
    | 'n' ->
        let* s = get_int4 rbuf in
        let list_size = Int32.to_int s in
        let data = read_seq rbuf list_size in
        return (Node_seq data)
    | _ -> tzfail Restore_context_failure

  (* Restoring *)

  let restore_context_fd index ~expected_context_hash ~fd ~nb_context_elements =
    let open Lwt_result_syntax in
    let read = ref 0 in
    let rbuf = ref (fd, Bytes.empty, 0, read) in
    (* Editing the repository *)
    let add_blob t blob =
      let*! tree = I.add_bytes t blob in
      return tree
    in
    let add_dir t keys =
      let* r = I.add_dir t keys in
      match r with
      | None -> tzfail Restore_context_failure
      | Some tree -> return tree
    in
    let restore () =
      let first_pass () =
        let* r = get_command rbuf in
        match r with
        | Root -> return_unit
        | _ -> tzfail Inconsistent_context_dump
      in
      let rec second_pass batch ctxt context_hash notify =
        let*! () = notify () in
        let* c = get_command rbuf in
        match c with
        | Node_seq contents ->
            let* tree = add_dir batch contents in
            second_pass batch (I.update_context ctxt tree) context_hash notify
        | Blob data ->
            let* tree = add_blob batch data in
            second_pass batch (I.update_context ctxt tree) context_hash notify
        | Eoc {info; parents} -> (
            let*! b = I.set_context ~info ~parents ctxt context_hash in
            match b with
            | false -> tzfail Inconsistent_context_dump
            | true -> return_unit)
        | _ -> tzfail Inconsistent_context_dump
      in
      let check_eof () =
        let* e = get_command rbuf in
        match e with
        | Eof -> return_unit
        | _ -> tzfail Inconsistent_context_dump
      in
      let* block_data = first_pass () in
      let* () =
        Animation.display_progress
          ~every:1000
          ~pp_print_step:(fun fmt i ->
            Format.fprintf
              fmt
              "Writing context: %dK/%dK (%d%%) elements, %s read"
              (i / 1_000)
              (nb_context_elements / 1_000)
              (100 * i / nb_context_elements)
              (if !read > 1_048_576 then
               Format.asprintf "%dMiB" (!read / 1_048_576)
              else Format.asprintf "%dKiB" (!read / 1_024)))
          (fun notify ->
            I.batch index (fun batch ->
                second_pass
                  batch
                  (I.make_context index)
                  expected_context_hash
                  notify))
      in
      let* () = check_eof () in
      return block_data
    in
    Lwt.catch
      (fun () -> restore ())
      (function
        | Unix.Unix_error (e, _, _) ->
            tzfail @@ System_read_error (Unix.error_message e)
        | err -> Lwt.fail err)
end

module Make (I : Dump_interface) = struct
  type command =
    | Root
    | Blob of bytes
    | Inode of I.Snapshot.inode
    | Eoc of {info : I.commit_info; parents : I.Commit_hash.t list}
    | Eof

  (* Command encoding. *)

  let blob_encoding =
    let open Data_encoding in
    case
      ~title:"blob"
      (Tag (Char.code 'b'))
      bytes
      (function Blob b -> Some b | _ -> None)
      (function b -> Blob b)

  let inode_encoding =
    let open Data_encoding in
    case
      ~title:"inode"
      (Tag (Char.code 'i'))
      I.Snapshot.encoding
      (function Inode b -> Some b | _ -> None)
      (function b -> Inode b)

  let eof_encoding =
    let open Data_encoding in
    case
      ~title:"eof"
      (Tag (Char.code 'e'))
      empty
      (function Eof -> Some () | _ -> None)
      (fun () -> Eof)

  let root_encoding =
    let open Data_encoding in
    case
      ~title:"root"
      (Tag (Char.code 'r'))
      empty
      (function Root -> Some () | _ -> None)
      (fun () -> Root)

  let eoc_encoding =
    let open Data_encoding in
    case
      ~title:"eoc"
      (Tag (Char.code 'c'))
      (obj2
         (req "info" I.commit_info_encoding)
         (req "parents" (list I.Commit_hash.encoding)))
      (function Eoc {info; parents} -> Some (info, parents) | _ -> None)
      (fun (info, parents) -> Eoc {info; parents})

  let command_encoding =
    Data_encoding.union
      ~tag_size:`Uint8
      [blob_encoding; eoc_encoding; root_encoding; eof_encoding; inode_encoding]

  let get_mbytes rbuf =
    let open Lwt_result_syntax in
    let* size = get_int64 rbuf in
    let b = Bytes.create (Int64.to_int size) in
    let* () = read_mbytes rbuf b in
    return b

  let get_command rbuf =
    let open Lwt_result_syntax in
    let* bytes = get_mbytes rbuf in
    return (Data_encoding.Binary.of_bytes_exn command_encoding bytes)

  (* Getter and setters *)

  let set_root written context_fd buf =
    let root = Root in
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding root in
    set_mbytes written context_fd buf bytes

  let set_tree written context_fd ~notify buf (tree : I.Snapshot.t) =
    let open Lwt_syntax in
    let x =
      match tree with Inode node -> Inode node | Blob blob -> Blob blob
    in
    let s =
      match Data_encoding.Binary.to_bytes command_encoding x with
      | Ok s -> s
      | Error error ->
          Fmt.failwith
            "error write %a"
            Data_encoding.Binary.pp_write_error
            error
    in
    let* () = set_mbytes written context_fd buf s in
    notify ()

  let set_eoc written context_fd buf info parents =
    let eoc = Eoc {info; parents} in
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding eoc in
    set_mbytes written context_fd buf bytes

  let set_end written context_fd buf =
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding Eof in
    set_mbytes written context_fd buf bytes

  let serialize_tree written context_fd ~notify ~on_disk buf tree idx =
    I.tree_iteri_unique
      ~on_disk
      idx
      (fun sub_tree -> set_tree written context_fd ~notify buf sub_tree)
      tree

  let dump_context_fd idx context_hash ~context_fd ~on_disk =
    let open Lwt_result_syntax in
    (* Dumping *)
    let buf = Buffer.create 1_000_000 in
    let written = ref 0 in
    Lwt.catch
      (fun () ->
        let*! o = I.checkout idx context_hash in
        match o with
        | None ->
            (* FIXME: dirty *)
            tzfail @@ Context_not_found (I.Commit_hash.to_bytes context_hash)
        | Some ctxt ->
            Animation.display_progress
              ~every:1000
              ~pp_print_step:(fun fmt i ->
                Format.fprintf
                  fmt
                  "Copying context: %dK elements, %s written"
                  (i / 1000)
                  (if !written > 1_048_576 then
                   Format.asprintf "%dMiB" (!written / 1_048_576)
                  else Format.asprintf "%dKiB" (!written / 1_024)))
              (fun notify ->
                let*! () = set_root written context_fd buf in
                let tree = I.context_tree ctxt in
                let*! elements =
                  serialize_tree
                    written
                    context_fd
                    ~notify
                    ~on_disk
                    buf
                    tree
                    idx
                in
                let parents = I.context_parents ctxt in
                let*! () =
                  set_eoc written context_fd buf (I.context_info ctxt) parents
                in
                let*! () = set_end written context_fd buf in
                let*! () = flush written context_fd buf in
                return elements))
      (function
        | Unix.Unix_error (e, _, _) ->
            tzfail @@ System_write_error (Unix.error_message e)
        | err -> Lwt.fail err)

  (* Restoring *)

  let restore_context_fd index ~expected_context_hash ~fd ~nb_context_elements
      ~in_memory =
    let open Lwt_result_syntax in
    let read = ref 0 in
    let rbuf = ref (fd, Bytes.empty, 0, read) in
    (* Editing the repository *)
    let import_t = I.v_import ~in_memory index in
    let save_inode = I.save_inode index import_t in
    let add_inode i =
      let*! tree = save_inode i in
      match tree with
      | None -> tzfail Restore_context_failure
      | Some tree -> return tree
    in
    let restore () =
      let first_pass () =
        let* r = get_command rbuf in
        match r with
        | Root -> return_unit
        | _ -> tzfail Inconsistent_context_dump
      in
      let rec second_pass batch ctxt context_hash notify =
        let*! () = notify () in
        let* c = get_command rbuf in
        match c with
        | Inode i ->
            let* tree = add_inode (Inode i) in
            second_pass batch (I.update_context ctxt tree) context_hash notify
        | Blob data ->
            let* tree = add_inode (Blob data) in
            second_pass batch (I.update_context ctxt tree) context_hash notify
        | Eoc {info; parents} -> (
            let*! b = I.set_context ~info ~parents ctxt context_hash in
            match b with
            | false -> tzfail Inconsistent_context_dump
            | true -> return_unit)
        | _ -> tzfail Inconsistent_context_dump
      in
      let check_eof () =
        let* e = get_command rbuf in
        match e with
        | Eof ->
            I.close_import import_t ;
            return_unit
        | _ -> tzfail Inconsistent_context_dump
      in
      let* block_data = first_pass () in
      let* () =
        Animation.display_progress
          ~every:1000
          ~pp_print_step:(fun fmt i ->
            Format.fprintf
              fmt
              "Writing context: %dK/%dK (%d%%) elements, %s read"
              (i / 1_000)
              (nb_context_elements / 1_000)
              (100 * i / nb_context_elements)
              (if !read > 1_048_576 then
               Format.asprintf "%dMiB" (!read / 1_048_576)
              else Format.asprintf "%dKiB" (!read / 1_024)))
          (fun notify ->
            I.batch index (fun batch ->
                second_pass
                  batch
                  (I.make_context index)
                  expected_context_hash
                  notify))
      in
      let* () = check_eof () in
      return block_data
    in
    Lwt.catch
      (fun () -> restore ())
      (function
        | Unix.Unix_error (e, _, _) ->
            tzfail @@ System_read_error (Unix.error_message e)
        | err -> Lwt.fail err)
end

(* Legacy errors*)
type error +=
  | Inconsistent_snapshot_file
  | Inconsistent_snapshot_data
  | Invalid_snapshot_version of string * string list

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"context_dump.inconsistent_snapshot_file"
    ~title:"Inconsistent snapshot file"
    ~description:"Error while opening snapshot file"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to read snapshot file. The provided file is inconsistent.")
    empty
    (function Inconsistent_snapshot_file -> Some () | _ -> None)
    (fun () -> Inconsistent_snapshot_file) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.inconsistent_snapshot_data"
    ~title:"Inconsistent snapshot data"
    ~description:"The data provided by the snapshot is inconsistent"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The data provided by the snapshot file is inconsistent (context_hash \
         does not correspond for block).")
    empty
    (function Inconsistent_snapshot_data -> Some () | _ -> None)
    (fun () -> Inconsistent_snapshot_data) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.invalid_snapshot_version"
    ~title:"Invalid snapshot version"
    ~description:"The version of the snapshot to import is not valid"
    ~pp:(fun ppf (found, expected) ->
      Format.fprintf
        ppf
        "The snapshot to import has version \"%s\" but one of %a was expected."
        found
        Format.(
          pp_print_list
            ~pp_sep:(fun ppf () -> fprintf ppf ", ")
            (fun ppf version -> fprintf ppf "\"%s\"" version))
        expected)
    (obj2 (req "found" string) (req "expected" (list string)))
    (function
      | Invalid_snapshot_version (found, expected) -> Some (found, expected)
      | _ -> None)
    (fun (found, expected) -> Invalid_snapshot_version (found, expected))
