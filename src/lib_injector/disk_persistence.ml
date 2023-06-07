(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type error +=
  | Cannot_write_file of string
  | Cannot_create_dir of string
  | Cannot_delete_file of string
  | Cannot_read_file of string
  | Io_error of [`Close | `Open] Lwt_utils_unix.io_error
  | Unix_error of Unix.error
  | Decoding_error of Data_encoding.Binary.read_error

let () =
  register_error_kind
    ~id:"rollups.injector.cannot_write_file"
    ~title:"Cannot write file"
    ~description:"An element for a persistent table could not be written"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "The persistent element %s could not be written" s)
    `Temporary
    Data_encoding.(obj1 (req "file" string))
    (function Cannot_write_file s -> Some s | _ -> None)
    (fun s -> Cannot_write_file s) ;
  register_error_kind
    ~id:"rollups.injector.cannot_create_dir"
    ~title:"Cannot create directory"
    ~description:"Directory for persistent data structure could not be created"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Directory %s for persistent data structure could not be created"
        s)
    `Temporary
    Data_encoding.(obj1 (req "directory" string))
    (function Cannot_create_dir s -> Some s | _ -> None)
    (fun s -> Cannot_create_dir s) ;
  register_error_kind
    ~id:"rollups.injector.cannot_delete_file"
    ~title:"Cannot delete file"
    ~description:"An element for a persistent table could not be deleted"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "The persistent element %s could not be deleted" s)
    `Temporary
    Data_encoding.(obj1 (req "file" string))
    (function Cannot_delete_file s -> Some s | _ -> None)
    (fun s -> Cannot_delete_file s) ;
  register_error_kind
    ~id:"rollups.injector.cannot_read_file"
    ~title:"Cannot read file"
    ~description:"A file for a persistent element could not be read"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "The persistent element %s could not be read" s)
    `Temporary
    Data_encoding.(obj1 (req "file" string))
    (function Cannot_read_file s -> Some s | _ -> None)
    (fun s -> Cannot_read_file s) ;
  register_error_kind
    ~id:"rollups.injector.io_error"
    ~title:"IO error"
    ~description:"IO error"
    ~pp:(fun ppf (_action, unix_code, caller, arg) ->
      Format.fprintf
        ppf
        "IO error in %s(%s): %s)"
        caller
        arg
        (Unix.error_message unix_code))
    `Temporary
    Data_encoding.(
      obj4
        (req "action" (string_enum [("close", `Close); ("open", `Open)]))
        (req "unix_code" Tezos_stdlib_unix.Unix_error.encoding)
        (req "caller" string)
        (req "arg" string))
    (function
      | Io_error Lwt_utils_unix.{action; unix_code; caller; arg} ->
          Some (action, unix_code, caller, arg)
      | _ -> None)
    (fun (action, unix_code, caller, arg) ->
      Io_error Lwt_utils_unix.{action; unix_code; caller; arg}) ;
  register_error_kind
    ~id:"rollups.injector.unix_error"
    ~title:"Unix error"
    ~description:"Unix error"
    ~pp:(fun ppf error ->
      Format.fprintf ppf "Unix error: %s" (Unix.error_message error))
    `Temporary
    Data_encoding.(obj1 (req "error" Tezos_stdlib_unix.Unix_error.encoding))
    (function Unix_error e -> Some e | _ -> None)
    (fun e -> Unix_error e) ;
  register_error_kind
    ~id:"rollups.injector.decoding_error"
    ~title:"Cannot decode file"
    ~description:"A file for a persistent element could not be decoded"
    ~pp:(fun ppf error ->
      Format.fprintf
        ppf
        "Decoding error: %a"
        Data_encoding.Json.pp
        (Data_encoding.Json.construct
           Data_encoding.Binary.read_error_encoding
           error))
    `Permanent
    Data_encoding.(obj1 (req "error" Data_encoding.Binary.read_error_encoding))
    (function Decoding_error e -> Some e | _ -> None)
    (fun e -> Decoding_error e) ;
  ()

module type H = sig
  include Hashtbl.SeededS

  type value

  val name : string

  val string_of_key : key -> string

  val key_of_string : string -> key option

  val value_encoding : value Data_encoding.t
end

let create_dir dir =
  trace (Cannot_create_dir dir)
  @@ protect
  @@ fun () ->
  let open Lwt_result_syntax in
  let*! () = Lwt_utils_unix.create_dir dir in
  return_unit

let read_value file encoding =
  let open Lwt_syntax in
  trace (Cannot_read_file file)
  @@ Lwt.catch
       (fun () ->
         Lwt_io.with_file ~flags:[Unix.O_RDONLY; O_CLOEXEC] ~mode:Input file
         @@ fun channel ->
         let+ bytes = Lwt_io.read channel in
         Result.map_error (fun e -> [Decoding_error e])
         @@ Data_encoding.Binary.of_bytes
              encoding
              (Bytes.unsafe_of_string bytes))
       (function
         | Unix.Unix_error (e, _, _) -> fail (Unix_error e) | e -> fail (Exn e))

let maybe_read_value ~warn file encoding =
  let open Lwt_syntax in
  let* v = read_value file encoding in
  match v with
  | Error e ->
      let+ () = warn file e in
      None
  | Ok v -> return_some v

let write_value file encoding value =
  trace (Cannot_write_file file)
  @@ protect
  @@ fun () ->
  Lwt_result.map_error (fun e -> [Io_error e])
  @@ Lwt_utils_unix.with_open_out ~overwrite:true file
  @@ fun fd ->
  let block_bytes = Data_encoding.Binary.to_bytes_exn encoding value in
  Lwt_utils_unix.write_bytes fd block_bytes

let delete_file file =
  trace (Cannot_delete_file file)
  @@ protect
  @@ fun () ->
  let open Lwt_result_syntax in
  let*! () = Lwt_unix.unlink file in
  return_unit

module Make_table (H : H) = struct
  type key = H.key

  type value = H.value

  type t = {path : string; table : value H.t}

  let filedata t k = Filename.concat t.path (H.string_of_key k)

  let create ~data_dir n =
    let open Lwt_result_syntax in
    let table = H.create n in
    let path = Filename.concat data_dir H.name in
    let+ () = create_dir path in
    {path; table}

  let replace t k v =
    H.replace t.table k v ;
    write_value (filedata t k) H.value_encoding v

  let remove t k =
    H.remove t.table k ;
    delete_file (filedata t k)

  let find t k = H.find t.table k

  let mem t k = H.mem t.table k

  let iter_s f t = H.iter_s f t.table

  let iter_es f t = H.iter_es f t.table

  let fold f t = H.fold f t.table

  let length t = H.length t.table

  let replace_seq t seq =
    H.replace_seq t.table seq ;
    Seq.ES.iter
      (fun (k, v) -> write_value (filedata t k) H.value_encoding v)
      seq

  let load_from_disk ~warn_unreadable ~initial_size ~data_dir ~filter =
    let open Lwt_result_syntax in
    let* t = create ~data_dir initial_size in
    let*! d = Lwt_unix.opendir t.path in
    let rec browse () =
      let*! filename =
        let open Lwt_syntax in
        Lwt.catch
          (fun () ->
            let+ f = Lwt_unix.readdir d in
            Some f)
          (function End_of_file -> return_none | e -> raise e)
      in
      match filename with
      | None -> return_unit
      | Some filename ->
          let* () =
            match H.key_of_string filename with
            | None -> return_unit
            | Some k -> (
                let+ v =
                  match warn_unreadable with
                  | None ->
                      let+ v = read_value (filedata t k) H.value_encoding in
                      Some v
                  | Some warn ->
                      let*! v =
                        maybe_read_value ~warn (filedata t k) H.value_encoding
                      in
                      return v
                in
                match v with
                | None -> ()
                | Some v -> if filter v then H.add t.table k v)
          in
          browse ()
    in
    let+ () = browse () in
    t
end

module Make_queue (N : sig
  val name : string
end)
(K : Tezos_crypto.Intfs.HASH) (V : sig
  type t

  val encoding : t Data_encoding.t
end) =
struct
  module Q = Hash_queue.Make (K) (V)

  type t = {path : string; metadata_path : string; queue : Q.t}

  let counter = ref min_int

  let filedata q k = Filename.concat q.path (K.to_b58check k)

  let filemetadata q k = Filename.concat q.metadata_path (K.to_b58check k)

  let create ~data_dir n =
    let open Lwt_result_syntax in
    let queue = Q.create n in
    let path = Filename.concat data_dir N.name in
    let metadata_path = Filename.concat path "metadata" in
    let* () = create_dir path in
    let+ () = create_dir metadata_path in
    {path; metadata_path; queue}

  let remove q k =
    let open Lwt_result_syntax in
    Q.remove q.queue k ;
    let* () = delete_file (filedata q k)
    and* () = delete_file (filemetadata q k) in
    return_unit

  let create_metadata () =
    let time = Time.System.now () in
    let d, ps = Ptime.to_span time |> Ptime.Span.to_d_ps in
    let c = !counter in
    incr counter ;
    (d, ps, c)

  let metadata_encoding =
    let open Data_encoding in
    conv
      (fun (d, ps, c) -> (Int64.of_int d, ps, Int64.of_int c))
      (fun (d, ps, c) -> (Int64.to_int d, ps, Int64.to_int c))
    @@ tup3 int64 int64 int64

  let replace q k v =
    let open Lwt_result_syntax in
    Q.replace q.queue k v ;
    let* () = write_value (filedata q k) V.encoding v
    and* () =
      write_value (filemetadata q k) metadata_encoding (create_metadata ())
    in
    return_unit

  let fold f q = Q.fold f q.queue

  let length q = Q.length q.queue

  let find_opt q k = Q.find_opt q.queue k

  let load_from_disk ~warn_unreadable ~capacity ~data_dir ~filter =
    let open Lwt_result_syntax in
    let* q = create ~data_dir capacity in
    let*! d = Lwt_unix.opendir q.path in
    let rec browse acc =
      let*! filename =
        let open Lwt_syntax in
        Lwt.catch
          (fun () ->
            let+ f = Lwt_unix.readdir d in
            Some f)
          (function End_of_file -> return_none | e -> raise e)
      in
      match filename with
      | None -> return acc
      | Some filename ->
          let* acc =
            match K.of_b58check_opt filename with
            | None -> return acc
            | Some k -> (
                let+ v_meta =
                  match warn_unreadable with
                  | None ->
                      let* v = read_value (filedata q k) V.encoding
                      and* meta =
                        read_value (filemetadata q k) metadata_encoding
                      in
                      return_some (v, meta)
                  | Some warn ->
                      let open Lwt_syntax in
                      let* v = maybe_read_value ~warn (filedata q k) V.encoding
                      and* meta =
                        maybe_read_value
                          ~warn
                          (filemetadata q k)
                          metadata_encoding
                      in
                      return_ok @@ Option.bind v
                      @@ fun v -> Option.bind meta @@ fun meta -> Some (v, meta)
                in
                match v_meta with
                | None -> acc
                | Some (v, meta) ->
                    if filter v then (k, v, meta) :: acc else acc)
          in
          browse acc
    in
    let* list = browse [] in
    let list =
      List.fast_sort
        (fun (_, _, meta1) (_, _, meta2) -> Stdlib.compare meta1 meta2)
        list
    in
    List.iter (fun (k, v, _) -> Q.replace q.queue k v) list ;
    return q
end
