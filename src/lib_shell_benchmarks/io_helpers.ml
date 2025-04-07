(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

let assert_ok ~msg = function
  | Ok x -> x
  | Error errs ->
      Format.eprintf "%s:@.%a@." msg pp_print_trace errs ;
      exit 1

let prepare_genesis context_dir =
  let open Lwt_result_syntax in
  let*! index = Tezos_context.Context.init ~readonly:false context_dir in
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

let flush context =
  let open Lwt.Syntax in
  let context = Tezos_shell_context.Shell_context.unwrap_disk_context context in
  let+ context = Tezos_context.Context.flush context in
  Tezos_shell_context.Shell_context.wrap_disk_context context

let prepare_empty_context context_dir =
  let open Lwt_result_syntax in
  let* index, context, _context_hash = prepare_genesis context_dir in
  let*! context_hash = commit context in
  let*! () = Tezos_context.Context.close index in
  return context_hash

let purge_disk_cache () =
  Format.eprintf "Purging disk cache@." ;
  let command = "./purge_disk_cache.exe" in
  match Sys.command command with
  | 0 -> ()
  | n ->
      Stdlib.failwith
        (Printf.sprintf "Error: failed to execute %s: code: %d" command n)

let load_context_from_disk_lwt context_dir context_hash =
  let open Lwt_syntax in
  let* index = Tezos_context.Context.init ~readonly:false context_dir in
  let* o = Tezos_context.Context.checkout index context_hash in
  match o with
  | None -> assert false
  | Some context ->
      Lwt.return
        (Tezos_shell_context.Shell_context.wrap_disk_context context, index)

let load_context_from_disk context_dir context_hash =
  Lwt_main.run (load_context_from_disk_lwt context_dir context_hash)

let with_context ~context_dir ~context_hash f =
  let context, index = load_context_from_disk context_dir context_hash in
  Lwt_main.run
    (let open Lwt_syntax in
     let* res = f context in
     let* () = Tezos_context.Context.close index in
     Lwt.return res)

let prepare_context_dir context_dir = Unix.unlink context_dir

let initialize_key rng_state context path storage_size =
  let bytes = Base_samplers.uniform_bytes rng_state ~nbytes:storage_size in
  Tezos_protocol_environment.Context.add context path bytes

let commit_and_reload context_dir index context =
  let open Lwt_syntax in
  let* context_hash = commit context in
  let* () = Tezos_context.Context.close index in
  load_context_from_disk_lwt context_dir context_hash

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

  let rec to_seq :
      String_map.key list ->
      (String_map.key list * 'a) Seq.t ->
      'a t ->
      (String_map.key list * 'a) Seq.t =
   fun path acc tree ->
    match tree with
    | Leaf v -> fun () -> Seq.Cons ((List.rev path, v), acc)
    | Node map ->
        Seq.concat_map (fun (seg, subtree) -> to_seq (seg :: path) acc subtree)
        @@ String_map.to_seq map

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

let split_absolute_path s =
  if Filename.is_relative s then None
  else
    let ss = String.split_no_empty '/' s in
    (* Must not contain . and .. *)
    if List.exists (function "." | ".." -> true | _ -> false) ss then None
    else Some ss

let load_head_block data_dir =
  let chain_id = "NetXdQprcVkpa" in
  let genesis =
    let config =
      let fn_config =
        Printf.sprintf "%s/store/chain_%s/config.json" data_dir chain_id
      in
      let config = In_channel.(with_open_text fn_config input_all) in
      match Data_encoding.Json.from_string config with
      | Error s -> Stdlib.failwith s
      | Ok config ->
          Data_encoding.Json.destruct
            Tezos_store_shared.Store_types.chain_config_encoding
            config
    in
    config.genesis
  in
  let open Lwt_result_syntax in
  let open Tezos_store.Store in
  let* store =
    init
      ~store_dir:(Filename.concat data_dir "store")
      ~context_root_dir:data_dir
      ~allow_testchains:false
      genesis
  in
  let chain_store = main_chain_store store in
  let*! head = Chain.current_head chain_store in
  let*! () = close_store store in
  return (Block.level head, Block.hash head, Block.context_hash head)

module Meminfo = struct
  type t = {
    memTotal : int;
    memFree : int;
    memAvailable : int;
    buffers : int;
    cached : int;
    swapTotal : int;
  }

  let pp ppf {memTotal; memFree; memAvailable; buffers; cached; swapTotal} =
    let open Format in
    fprintf
      ppf
      "{ memTotal: %d; memFree: %d; memAvailable: %d; buffers: %d; cached: %d; \
       swapTotal: %d }"
      memTotal
      memFree
      memAvailable
      buffers
      cached
      swapTotal

  let get () =
    let open In_channel in
    with_open_text "/proc/meminfo" @@ fun ic ->
    let rec loop acc =
      match input_line ic with
      | None -> acc
      | Some s ->
          let acc =
            try
              let n = String.index s ':' in
              let key = String.sub s 0 n in
              let rest =
                let rest = String.(sub s (n + 1) (length s - n - 1)) in
                match String.remove_suffix ~suffix:" kB" rest with
                | Some s -> s
                | _ | (exception _) -> rest
              in
              let kbs = Scanf.sscanf rest " %d" Fun.id in
              match key with
              | "MemTotal" -> {acc with memTotal = kbs}
              | "MemFree" -> {acc with memFree = kbs}
              | "MemAvailable" -> {acc with memAvailable = kbs}
              | "Buffers" -> {acc with buffers = kbs}
              | "Cached" -> {acc with cached = kbs}
              | "SwapTotal" -> {acc with swapTotal = kbs}
              | _ -> acc
            with _ -> acc
          in
          loop acc
    in
    (* We can use [option] type but no worth for it *)
    loop
      {
        memTotal = -1;
        memFree = -1;
        memAvailable = -1;
        buffers = -1;
        cached = -1;
        swapTotal = -1;
      }

  let get_available () =
    let meminfo = get () in
    meminfo.memAvailable
end

module Dummy_memory = struct
  (* Dummy memory is allocated by mmap(2).

     [MemAvailable] decreases immediately when a process allocates
     a new memory block. However, it does not increase immediately
     even when the process frees it.

     [MemAvailable] is immediately updated when the memory is allocated
     by mmap(2) then released by munmap(2).
  *)

  type t

  external alloc : int -> t = "caml_alloc_by_mmap"

  external free : t -> unit = "caml_free_by_mmap"

  let allocate = alloc

  let deallocate = free
end

let with_memory_restriction gib f =
  let blocks = ref ([] : Dummy_memory.t list) in
  let block_size = 1024 * 1024 * 100 (* 100MiB *) in
  let target = int_of_float (gib *. 1024. *. 1024. *. 1024.) in
  let restrict () =
    let rec loop acc =
      let available = Meminfo.get_available () * 1024 in
      let diff = available - target in
      if diff <= -block_size then (
        match acc with
        | [] -> acc
        | block :: acc ->
            Dummy_memory.deallocate block ;
            loop acc)
      else if block_size < diff then
        loop (Dummy_memory.allocate block_size :: acc)
      else acc
    in
    blocks := loop !blocks
  in
  let res = f restrict in
  List.iter Dummy_memory.deallocate !blocks ;
  res

let fill_disk_cache ~rng ~restrict_memory context keys_list =
  let open Lwt.Syntax in
  let nkeys =
    List.fold_left (fun acc keys -> acc + Array.length keys) 0 keys_list
  in
  Format.eprintf "Filling the disk cache...@." ;
  (* Allocate memory to restrict the size of disk cache *)
  let rec fill_cache context = function
    | 0 -> Lwt.return context
    | n ->
        let i = Random.State.int rng nkeys in
        let rec get_ith i = function
          | [] -> invalid_arg "get_ith"
          | keys :: keys_list ->
              let len = Array.length keys in
              if len <= i then get_ith (i - len) keys_list else keys.(i)
        in
        let* _ =
          Tezos_protocol_environment.Context.find
            context
            (fst @@ get_ith i keys_list)
        in
        fill_cache context (n - 1)
  in
  let rec loop context cond n =
    let* context = flush context in
    let* context = fill_cache context 10000 in
    restrict_memory () ;
    let meminfo = Meminfo.get () in
    match cond with
    | `End_in 0 ->
        Format.eprintf "Filled the disk cache: (%d KiB cached)@." meminfo.cached ;
        Lwt.return_unit
    | `End_in m -> loop context (`End_in (m - 1)) (n + 1)
    | `Caching _ when n >= 30 ->
        Format.eprintf
          "Filling the disk cache: enough tried (%d KiB cached)@."
          meminfo.cached ;
        Lwt.return_unit
    | `Caching _
      when meminfo.memAvailable - meminfo.buffers - meminfo.cached
           < 1024_00 (* 100MiB *) ->
        (* Most of the [MemAvailable] is now used for the buffer and cache. *)
        Format.eprintf "Filling the disk cache: reaching a fixed point@." ;
        (* We loop 5 more times *)
        loop context (`End_in 5) (n + 1)
    | `Caching _ -> loop context (`Caching meminfo.cached) (n + 1)
  in
  let* () = loop context (`Caching 0) 0 in
  let meminfo = Meminfo.get () in
  Format.eprintf "%a@." Meminfo.pp meminfo ;
  Lwt.return_unit

let get_head_block_from_context_dir data_dir =
  match Lwt_main.run @@ load_head_block data_dir with
  | Error e ->
      Format.eprintf
        "Error: %a@."
        Tezos_error_monad.Error_monad.pp_print_trace
        e ;
      Format.eprintf "Failed to find a Tezos context at %s@." data_dir ;
      exit 1
  | Ok res -> res
