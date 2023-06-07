(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

(** Helpers for loading contexts, saving contexts, writing to contexts, etc.
    Also contains the [Key_map] module, heavily used for preparing benchmarks
    and computing statistics. *)

let assert_ok ~msg = function
  | Ok x -> x
  | Error errs ->
      Format.eprintf "%s:@.%a@." msg pp_print_trace errs ;
      exit 1

let prepare_genesis base_dir =
  let open Lwt_result_syntax in
  let*! index = Tezos_context.Context.init ~readonly:false base_dir in
  let genesis_block =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"
  in
  let* context_hash =
    Tezos_context.Context.commit_genesis
      index
      ~chain_id:(Chain_id.of_block_hash genesis_block)
      ~time:(Time.Protocol.of_seconds 0L)
      ~protocol:Protocol_hash.zero
  in
  let*! o = Tezos_context.Context.checkout index context_hash in
  match o with
  | None -> assert false
  | Some context ->
      let context =
        Tezos_shell_context.Shell_context.wrap_disk_context context
      in
      return (index, context, context_hash)

let commit context =
  let context = Tezos_shell_context.Shell_context.unwrap_disk_context context in
  Tezos_context.Context.commit
    ~time:
      (Time.Protocol.of_seconds
         (Int64.of_int (int_of_float @@ Unix.gettimeofday ())))
    context

let prepare_empty_context base_dir =
  let open Lwt_result_syntax in
  let* index, context, _context_hash = prepare_genesis base_dir in
  let*! context_hash = commit context in
  let*! () = Tezos_context.Context.close index in
  return context_hash

let load_context_from_disk_lwt base_dir context_hash =
  let open Lwt_syntax in
  let* index = Tezos_context.Context.init ~readonly:false base_dir in
  let* o = Tezos_context.Context.checkout index context_hash in
  match o with
  | None -> assert false
  | Some context ->
      Lwt.return
        (Tezos_shell_context.Shell_context.wrap_disk_context context, index)

let load_context_from_disk base_dir context_hash =
  Lwt_main.run (load_context_from_disk_lwt base_dir context_hash)

let with_context ~base_dir ~context_hash f =
  let context, index = load_context_from_disk base_dir context_hash in
  Lwt_main.run
    (let open Lwt_syntax in
    let* res = f context in
    let* () = Tezos_context.Context.close index in
    Lwt.return res)

let prepare_base_dir base_dir =
  Unix.unlink base_dir ;
  Unix.mkdir base_dir 0o700

(* This function updates the context with random bytes at a given depth. *)
let initialize_key rng_state context path storage_size =
  let bytes = Base_samplers.uniform_bytes rng_state ~nbytes:storage_size in
  Tezos_protocol_environment.Context.add context path bytes

let commit_and_reload base_dir index context =
  let open Lwt_syntax in
  let* context_hash = commit context in
  let* () = Tezos_context.Context.close index in
  load_context_from_disk_lwt base_dir context_hash

(** Maps from string lists to bytes. No balancing. A key cannot be a prefix
    or a suffix to another key. *)
module Key_map = struct
  module String_map = String.Map

  type 'a t = Leaf of 'a | Node of 'a t String_map.t

  exception Collision_with_prefix

  exception Collision_with_suffix

  let empty = Node String_map.empty

  let is_empty = function
    | Leaf _ -> false
    | Node map -> String_map.is_empty map

  let rec insert (key : string list) data (tree : 'a t) =
    if is_empty tree then
      match key with
      | [] -> Leaf data
      | seg :: tl ->
          let subtree = insert tl data empty in
          let singleton = String_map.singleton seg subtree in
          Node singleton
    else
      match key with
      | [] -> raise Collision_with_prefix
      | seg :: tl -> (
          match tree with
          | Leaf _ -> raise Collision_with_suffix
          | Node map ->
              let subtree =
                match String_map.find_opt seg map with
                | None -> insert tl data empty
                | Some subtree -> insert tl data subtree
              in
              let map = String_map.add seg subtree map in
              Node map)

  let rec does_not_collide key tree =
    if is_empty tree then `Key_does_not_collide
    else
      match (key, tree) with
      | [], Leaf _ -> `Key_exists
      | _, Leaf _ -> `Key_has_prefix
      | [], Node _ -> `Key_has_suffix
      | seg :: tl, Node map -> (
          match String_map.find_opt seg map with
          | None -> `Key_does_not_collide
          | Some subtree -> does_not_collide tl subtree)

  let rec mem key tree =
    match (key, tree) with
    | [], Leaf _ -> true
    | _, Leaf _ -> false
    | [], Node _ -> false
    | seg :: tl, Node map -> (
        match String_map.find_opt seg map with
        | None -> false
        | Some subtree -> mem tl subtree)

  let rec find_opt key tree =
    match (key, tree) with
    | [], Leaf v -> Some v
    | _ :: _, Leaf _ -> None
    | [], Node _ -> None
    | seg :: tl, Node map -> (
        match String_map.find_opt seg map with
        | None -> None
        | Some subtree -> find_opt tl subtree)

  let rec to_seq path acc tree =
    match tree with
    | Leaf v -> fun () -> Seq.Cons ((List.rev path, v), acc)
    | Node map ->
        String_map.fold
          (fun seg subtree acc -> to_seq (seg :: path) acc subtree)
          map
          acc

  let to_seq tree = to_seq [] Seq.empty tree

  let of_seq seq = Seq.fold_left (fun map (k, v) -> insert k v map) empty seq

  let fold_lwt f m accu =
    Seq.S.fold_left (fun acc (k, v) -> f k v acc) accu (to_seq m)

  let sample_uniform map =
    if is_empty map then None
    else
      let seq = to_seq map in
      let arr = Array.of_seq seq in
      let len = Array.length arr in
      let i = Random.int len in
      Some arr.(i)

  let encoding value_encoding =
    let open Data_encoding in
    conv
      (fun map -> List.of_seq (to_seq map))
      (fun l -> of_seq (List.to_seq l))
      (list (tup2 (list string) value_encoding))
end

let rec take_n n list acc =
  if n = 0 then (List.rev acc, list)
  else
    match list with
    | [] -> Stdlib.invalid_arg "take_n"
    | x :: tl -> take_n (n - 1) tl (x :: acc)

let sample_without_replacement n list =
  let first_n, rest = take_n n list [] in
  let reservoir = Array.of_list first_n in
  let reject = ref [] in
  List.iteri
    (fun index elt ->
      let i = n + index in
      let j = Random.int (i + 1) in
      if j < n then (
        reject := reservoir.(j) :: !reject ;
        reservoir.(j) <- elt)
      else reject := elt :: !reject)
    rest ;
  (Array.to_list reservoir, !reject)

(*
  © 1991, 1992, 2003, 2004, 2005, 2006, 2008, 2009, 2010
  Xavier Leroy and Didier Rémy, inria Rocquencourt.
  The code below is under Creative Commons license. https://ocaml.github.io/ocamlunix/LICENSE
  Copied from https://ocaml.github.io/ocamlunix/ocamlunix.html#sec51
*)

let buffer_size = 8192

let buffer = Bytes.create buffer_size

let file_copy input_name output_name =
  let open Unix in
  let fd_in = openfile input_name [O_RDONLY] 0 in
  let fd_out = openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o660 in
  let rec copy_loop () =
    match read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r ->
        ignore (write fd_out buffer 0 r) ;
        copy_loop ()
  in
  copy_loop () ;
  close fd_in ;
  close fd_out

let set_infos filename infos =
  let open Unix in
  utimes filename infos.st_atime infos.st_mtime ;
  chmod filename infos.st_perm ;
  try chown filename infos.st_uid infos.st_gid
  with Unix_error (EPERM, _, _) -> ()

let iter_dir f dirname =
  let open Unix in
  let d = opendir dirname in
  try
    while true do
      f (readdir d)
    done
  with End_of_file -> closedir d

let rec copy_rec source dest =
  let open Unix in
  let infos = lstat source in
  match infos.st_kind with
  | S_REG ->
      file_copy source dest ;
      set_infos dest infos
  | S_LNK ->
      let link = readlink source in
      symlink link dest
  | S_DIR ->
      mkdir dest 0o755 ;
      iter_dir
        (fun file ->
          if
            file <> Filename.current_dir_name
            && file <> Filename.parent_dir_name
          then
            copy_rec (Filename.concat source file) (Filename.concat dest file))
        source ;
      set_infos dest infos
  | _ -> prerr_endline ("Can't cope with special file " ^ source)
