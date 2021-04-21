(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Rresult

type t = {
  dir : Lmdb.t;
  parent : (Lmdb.rw Lmdb.txn * Lmdb.db * Lmdb.rw Lmdb.cursor) Lwt.key;
}

type key = string list

type value = Bytes.t

type error += Unknown of string list

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"raw_store.unknown"
    ~title:"Missing key in store"
    ~description:"Missing key in store"
    ~pp:(fun ppf keys ->
      Format.fprintf ppf "Missing key in store: %s" (String.concat "/" keys))
    Data_encoding.(obj1 (req "key" (list string)))
    (function Unknown keys -> Some keys | _ -> None)
    (fun keys -> Unknown keys)

let concat = String.concat "/"

let split = String.split_on_char '/'

let lwt_fail_error err = Lwt.fail_with (Lmdb.string_of_error err)

let of_result = function
  | Ok res ->
      Lwt.return res
  | Error err ->
      lwt_fail_error err

let ( >>=? ) v f = match v with Error err -> lwt_fail_error err | Ok v -> f v

let init ?(readonly = false) ?mapsize path =
  if not (Sys.file_exists path) then Unix.mkdir path 0o755 ;
  let sync_flag =
    match Sys.getenv_opt "TEZOS_STORE_SYNC" with
    | None ->
        []
    | Some s -> (
      match String.lowercase_ascii s with
      | "nosync" ->
          [Lmdb.NoSync]
      | "nometasync" ->
          [Lmdb.NoMetaSync]
      | _ ->
          Printf.eprintf
            "Unrecognized TEZOS_STORE_SYNC option : %s\n\
             allowed: nosync nometasync"
            s ;
          [] )
  in
  let readonly_flag = if readonly then [Lmdb.RdOnly] else [] in
  let file_flags = if readonly then 0o444 else 0o644 in
  match
    Lmdb.opendir
      ?mapsize
      ~flags:(sync_flag @ readonly_flag @ [NoTLS; NoMetaSync])
      path
      file_flags
  with
  | Ok dir ->
      return {dir; parent = Lwt.new_key ()}
  | Error err ->
      failwith "%a" Lmdb.pp_error err

let close {dir; _} = Lmdb.closedir dir

let known {dir; parent} key =
  ( match Lwt.get parent with
  | Some (txn, db, _cursor) ->
      Lmdb.mem txn db (concat key)
  | None ->
      Lmdb.with_ro_db dir ~f:(fun txn db -> Lmdb.mem txn db (concat key)) )
  |> of_result

let read_opt {dir; parent} key =
  ( match Lwt.get parent with
  | Some (txn, db, _cursor) ->
      Lmdb.get txn db (concat key) >>| Bigstring.to_bytes
  | None ->
      Lmdb.with_ro_db dir ~f:(fun txn db ->
          Lmdb.get txn db (concat key) >>| Bigstring.to_bytes) )
  |> function
  | Ok v ->
      Lwt.return_some v
  | Error KeyNotFound ->
      Lwt.return_none
  | Error err ->
      lwt_fail_error err

let read {dir; parent} key =
  ( match Lwt.get parent with
  | Some (txn, db, _cursor) ->
      Lmdb.get txn db (concat key) >>| Bigstring.to_bytes
  | None ->
      Lmdb.with_ro_db dir ~f:(fun txn db ->
          Lmdb.get txn db (concat key) >>| Bigstring.to_bytes) )
  |> function Ok v -> return v | Error _err -> fail (Unknown key)

let store {dir; parent} k v =
  let v = Bigstring.of_bytes v in
  ( match Lwt.get parent with
  | Some (txn, db, _cursor) ->
      Lmdb.put txn db (concat k) v
  | None ->
      Lmdb.with_rw_db dir ~f:(fun txn db -> Lmdb.put txn db (concat k) v) )
  |> of_result

let remove {dir; parent} k =
  let remove txn db =
    match Lmdb.del txn db (concat k) with
    | Ok () ->
        Ok ()
    | Error KeyNotFound ->
        Ok ()
    | Error err ->
        Error err
  in
  ( match Lwt.get parent with
  | Some (txn, db, _cursor) ->
      remove txn db
  | None ->
      Lmdb.with_rw_db dir ~f:remove )
  |> of_result

let is_prefix s s' =
  String.(length s <= length s' && compare s (sub s' 0 (length s)) = 0)

let known_dir {dir; parent} k =
  let k = concat k in
  let cursor_fun cursor =
    Lmdb.cursor_at cursor k
    >>= fun () ->
    Lmdb.cursor_get cursor
    >>| fun (first_k, _v) -> is_prefix k (Bigstring.to_string first_k)
  in
  ( match Lwt.get parent with
  | Some (txn, db, _cursor) ->
      Lmdb.with_cursor txn db ~f:cursor_fun
  | None ->
      Lmdb.with_ro_db dir ~f:(fun txn db ->
          Lmdb.with_cursor txn db ~f:cursor_fun) )
  |> of_result

let remove_dir {dir; parent} k =
  let k = concat k in
  let cursor_fun cursor =
    Lmdb.cursor_at cursor k
    >>= fun () ->
    Lmdb.cursor_iter cursor ~f:(fun (kk, _v) ->
        let kk_string = Bigstring.to_string kk in
        if is_prefix k kk_string then Lmdb.cursor_del cursor
        else Error KeyNotFound)
  in
  ( match Lwt.get parent with
  | Some (txn, db, _cursor) ->
      Lmdb.with_cursor txn db ~f:cursor_fun
  | None ->
      Lmdb.with_rw_db dir ~f:(fun txn db ->
          Lmdb.with_cursor txn db ~f:cursor_fun) )
  |> function
  | Error KeyNotFound | Ok () ->
      Lwt.return_unit
  | Error err ->
      lwt_fail_error err

let list_equal l1 l2 len =
  if len < 0 || len > List.length l1 || len > List.length l2 then
    invalid_arg "list_compare: invalid len" ;
  let rec inner l1 l2 len =
    match (len, l1, l2) with
    | (0, _, _) ->
        true
    | (_, [], _) | (_, _, []) ->
        false
    | (_, h1 :: t1, h2 :: t2) ->
        if h1 <> h2 then false else inner t1 t2 (pred len)
  in
  inner l1 l2 len

let is_child ~parent ~child =
  let plen = List.length parent in
  let clen = List.length child in
  clen > plen && list_equal parent child plen

let list_sub l pos len =
  if len < 0 || pos < 0 || pos + len > List.length l then
    invalid_arg "list_sub" ;
  let rec inner (acc, n) = function
    | [] ->
        List.rev acc
    | h :: t ->
        if n = 0 then List.rev acc else inner (h :: acc, pred n) t
  in
  inner ([], len) l

let with_rw_cursor_lwt ?nosync ?nometasync ?flags ?name {dir; parent} ~f =
  let local_parent =
    match Lwt.get parent with
    | None ->
        None
    | Some (txn, _db, _cursor) ->
        Some txn
  in
  Lmdb.create_rw_txn ?nosync ?nometasync ?parent:local_parent dir
  >>=? fun txn ->
  Lmdb.opendb ?flags ?name txn
  >>=? fun db ->
  Lmdb.opencursor txn db
  >>=? fun cursor ->
  Lwt.with_value
    parent
    (Some (txn, db, cursor))
    (fun () ->
      Lwt.try_bind
        (fun () -> f cursor)
        (fun res ->
          Lmdb.cursor_close cursor ;
          Lmdb.commit_txn txn >>=? fun () -> Lwt.return res)
        (fun exn ->
          Lmdb.cursor_close cursor ; Lmdb.abort_txn txn ; Lwt.fail exn))

let cursor_next_lwt cursor acc f =
  match Lmdb.cursor_next cursor with
  | Error KeyNotFound ->
      acc
  | Error err ->
      lwt_fail_error err
  | Ok () ->
      Lwt.bind acc f

let cursor_at_lwt cursor k acc f =
  match Lmdb.cursor_at cursor (concat k) with
  | Error KeyNotFound ->
      acc
  | Error err ->
      lwt_fail_error err
  | Ok () ->
      Lwt.bind acc f

(* assumption: store path segments have only characters different than
   the separator '/', which immediately precedes '0' *)
let zero_char_str = String.make 1 (Char.chr (Char.code '/' + 1))

let next_key_after_subdirs l =
  match List.rev l with
  | [] ->
      [zero_char_str]
  | last :: firsts ->
      List.rev @@ ((last ^ zero_char_str) :: firsts)

module Hashtbl = Hashtbl.MakeSeeded (struct
  type t = string list

  let hash = Hashtbl.seeded_hash

  let equal (d1 : string list) d2 = d1 = d2
end)

type key_or_dir = [`Key of key | `Dir of key]

let fold t k ~init ~f =
  let base_len = List.length k in
  let rec inner ht cursor acc =
    Lmdb.cursor_get cursor
    >>=? fun (kk, _v) ->
    let kk = Bigstring.to_string kk in
    let kk_split = split kk in
    match is_child ~child:kk_split ~parent:k with
    | false ->
        Lwt.return acc
    | true ->
        let cur_len = List.length kk_split in
        if cur_len = succ base_len then
          cursor_next_lwt cursor (f (`Key kk_split) acc) (inner ht cursor)
        else
          let dir = list_sub kk_split 0 (succ base_len) in
          if Hashtbl.mem ht dir then
            cursor_at_lwt
              cursor
              (next_key_after_subdirs dir)
              (Lwt.return acc)
              (inner ht cursor)
          else (
            Hashtbl.add ht dir () ;
            cursor_next_lwt cursor (f (`Dir dir) acc) (inner ht cursor) )
  in
  with_rw_cursor_lwt t ~f:(fun cursor ->
      cursor_at_lwt cursor k (Lwt.return init) (fun acc ->
          let ht = Hashtbl.create 31 in
          inner ht cursor acc))

let fold_keys t k ~init ~f =
  with_rw_cursor_lwt t ~f:(fun cursor ->
      cursor_at_lwt
        cursor
        k
        (Lwt.return init)
        (let rec inner acc =
           Lmdb.cursor_get cursor
           >>=? fun (kk, _v) ->
           let kk = Bigstring.to_string kk in
           let kk_split = split kk in
           match is_child ~child:kk_split ~parent:k with
           | false ->
               Lwt.return acc
           | true ->
               cursor_next_lwt cursor (f kk_split acc) inner
         in
         inner))

let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

let open_with_atomic_rw ?mapsize path f =
  let open Error_monad in
  init ?mapsize path
  >>=? fun state ->
  with_rw_cursor_lwt state ~f:(fun _c -> f state)
  >>=? fun res -> close state ; return res

let with_atomic_rw state f = with_rw_cursor_lwt state ~f:(fun _c -> f ())
