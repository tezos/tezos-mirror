(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Protocol_client_context

(**

    Errors
    ======

*)

type error += Cannot_find_block of Block_hash.t

let () =
  Sc_rollup_node_errors.register_error_kind
    ~id:"sc_rollup.node.cannot_find_block"
    ~title:"Cannot find block from L1"
    ~description:"A block couldn't be found from the L1 node"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Block with hash %a was not found on the L1 node."
        Block_hash.pp
        hash)
    `Temporary
    Data_encoding.(obj1 (req "hash" Block_hash.encoding))
    (function Cannot_find_block hash -> Some hash | _ -> None)
    (fun hash -> Cannot_find_block hash)

(**

   State
   =====

*)

type header = {
  hash : Block_hash.t;
  level : int32;
  header : Block_header.shell_header;
}

let header_encoding =
  let open Data_encoding in
  conv
    (fun {hash; level = _; header} -> (hash, header))
    (fun (hash, header) -> {hash; level = header.level; header})
    (merge_objs
       (obj1 (req "hash" Block_hash.encoding))
       Block_header.shell_header_encoding)

type head = {hash : Block_hash.t; level : int32}

let head_encoding =
  let open Data_encoding in
  conv
    (fun {hash; level} -> (hash, level))
    (fun (hash, level) -> {hash; level})
    (obj2 (req "hash" Block_hash.encoding) (req "level" Data_encoding.int32))

let head_of_header {hash; level; header = _} = {hash; level}

module Blocks_cache =
  Aches_lwt.Lache.Make_option
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Block_hash))

type blocks_cache = Alpha_block_services.block_info Blocks_cache.t

type headers_cache = Block_header.shell_header Blocks_cache.t

open Octez_crawler.Layer_1

type nonrec t = {
  l1 : t;
  cctxt : Client_context.full;
  blocks_cache : blocks_cache;
      (** Global blocks cache for the smart rollup node. *)
  headers_cache : headers_cache;
      (** Global block headers cache for the smart rollup node. *)
  prefetch_blocks : int;  (** Number of blocks to prefetch by default. *)
}

let start ~name ~reconnection_delay ~l1_blocks_cache_size ?protocols
    ?(prefetch_blocks = l1_blocks_cache_size) cctxt =
  let open Lwt_result_syntax in
  let*? () =
    if prefetch_blocks > l1_blocks_cache_size then
      error_with
        "Blocks to prefetch must be less than the cache size: %d"
        l1_blocks_cache_size
    else Ok ()
  in
  let*! l1 = start ~name ~reconnection_delay ?protocols cctxt in
  let blocks_cache = Blocks_cache.create l1_blocks_cache_size in
  let headers_cache = Blocks_cache.create l1_blocks_cache_size in
  let cctxt = (cctxt :> Client_context.full) in
  return {l1; cctxt; blocks_cache; headers_cache; prefetch_blocks}

let shutdown {l1; _} = shutdown l1

let cache_shell_header {headers_cache; _} hash header =
  Blocks_cache.put headers_cache hash (Lwt.return_some header)

let iter_heads l1_ctxt f =
  iter_heads l1_ctxt.l1 @@ fun (hash, {shell = {level; _} as header; _}) ->
  cache_shell_header l1_ctxt hash header ;
  f {hash; level; header}

let wait_first l1_ctxt =
  let open Lwt_syntax in
  let+ hash, {shell = {level; _} as header; _} = wait_first l1_ctxt.l1 in
  {hash; level; header}

let get_predecessor_opt ?max_read {l1; _} = get_predecessor_opt ?max_read l1

let get_predecessor ?max_read {l1; _} = get_predecessor ?max_read l1

let get_tezos_reorg_for_new_head {l1; _} ?get_old_predecessor old_head new_head
    =
  get_tezos_reorg_for_new_head l1 ?get_old_predecessor old_head new_head

module Internal_for_tests = struct
  let dummy cctxt =
    {
      l1 = Internal_for_tests.dummy cctxt;
      cctxt = (cctxt :> Client_context.full);
      blocks_cache = Blocks_cache.create 1;
      headers_cache = Blocks_cache.create 1;
      prefetch_blocks = 0;
    }
end

(**

   Helpers
   =======

*)

(** [fetch_tezos_block l1_ctxt hash] returns a block shell header of
    [hash]. Looks for the block in the blocks cache first, and fetches it from
    the L1 node otherwise. *)
let fetch_tezos_shell_header {cctxt; blocks_cache; headers_cache; _} hash =
  let open Lwt_syntax in
  trace (Cannot_find_block hash)
  @@
  let errors = ref None in
  let fetch hash =
    let* shell_header =
      Tezos_shell_services.Shell_services.Blocks.Header.shell_header
        cctxt
        ~chain:`Main
        ~block:(`Hash (hash, 0))
        ()
    in
    match shell_header with
    | Error errs ->
        errors := Some errs ;
        return_none
    | Ok shell_header -> return_some shell_header
  in
  let+ shell_header =
    let do_cached_header_fetch () =
      Blocks_cache.bind_or_put headers_cache hash fetch Lwt.return
    in
    match Blocks_cache.bind blocks_cache hash Lwt.return with
    | None -> do_cached_header_fetch ()
    | Some block -> (
        (* There is already a full block being fetched. *)
        let* block in
        match block with
        | None ->
            (* Fetching full block failed. *)
            do_cached_header_fetch ()
        | Some block ->
            (* The full block is already in the cache. *)
            return_some block.header.shell)
  in
  match (shell_header, !errors) with
  | None, None ->
      (* This should not happen if {!find_in_cache} behaves correctly,
         i.e. calls {!fetch} for cache misses. *)
      error_with
        "Fetching Tezos block %a failed unexpectedly"
        Block_hash.pp
        hash
  | None, Some errs -> Error errs
  | Some shell_header, _ -> Ok shell_header

(** [fetch_tezos_block l1_ctxt hash] returns a block info given a block
    hash. Looks for the block in the blocks cache first, and fetches it from the
    L1 node otherwise. *)
let fetch_tezos_block {cctxt; blocks_cache; headers_cache; _} hash =
  let open Lwt_syntax in
  trace (Cannot_find_block hash)
  @@
  let errors = ref None in
  let fetch hash =
    let* block =
      Alpha_block_services.info
        cctxt
        ~chain:`Main
        ~block:(`Hash (hash, 0))
        ~metadata:`Always
        ()
    in
    match block with
    | Error errs ->
        errors := Some errs ;
        return_none
    | Ok block -> return_some block
  in
  let+ block = Blocks_cache.bind_or_put blocks_cache hash fetch Lwt.return in
  match (block, !errors) with
  | None, None ->
      (* This should not happen if {!find_in_cache} behaves correctly,
         i.e. calls {!fetch} for cache misses. *)
      error_with
        "Fetching Tezos block %a failed unexpectedly"
        Block_hash.pp
        hash
  | None, Some errs -> Error errs
  | Some block, _ ->
      Blocks_cache.put headers_cache hash (Lwt.return_some block.header.shell) ;
      Ok block

let make_prefetching_schedule {prefetch_blocks; _} blocks =
  let blocks_with_prefetching, _, first_prefetch =
    List.fold_left
      (fun (acc, nb_prefetch, prefetch) b ->
        let nb_prefetch = nb_prefetch + 1 in
        let prefetch = b :: prefetch in
        if nb_prefetch >= prefetch_blocks then ((b, prefetch) :: acc, 0, [])
        else ((b, []) :: acc, nb_prefetch, prefetch))
      ([], 0, [])
      (List.rev blocks)
  in
  match (blocks_with_prefetching, first_prefetch) with
  | [], _ | _, [] -> blocks_with_prefetching
  | (first, _) :: rest, _ -> (first, first_prefetch) :: rest

let prefetch_tezos_blocks l1_ctxt = function
  | [] -> ()
  | blocks ->
      Lwt.async @@ fun () ->
      List.iter_p
        (fun {hash; _} ->
          let open Lwt_syntax in
          let+ _maybe_block = fetch_tezos_block l1_ctxt hash in
          ())
        blocks
