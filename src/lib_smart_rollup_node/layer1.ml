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

(**

    Errors
    ======

*)

type error += Cannot_find_block of Block_hash.t

let () =
  register_error_kind
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
  Aches_lwt.Lache.Make_result
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Block_hash))

type block = ..

type fetch_block_rpc =
  Client_context.full ->
  ?metadata:[`Always | `Never] ->
  ?chain:Tezos_shell_services.Block_services.chain ->
  ?block:Tezos_shell_services.Block_services.block ->
  unit ->
  block tzresult Lwt.t

type headers_cache = (Block_header.shell_header, tztrace) Blocks_cache.t

type blocks_cache = (block, tztrace) Blocks_cache.t

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

let raw_l1_connection {l1; _} = l1

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

let create ~name ~reconnection_delay ~l1_blocks_cache_size ?protocols
    ?(prefetch_blocks = l1_blocks_cache_size) cctxt =
  let open Result_syntax in
  let* () =
    if prefetch_blocks > l1_blocks_cache_size then
      error_with
        "Blocks to prefetch must be less than the cache size: %d"
        l1_blocks_cache_size
    else Ok ()
  in
  let l1 = create ~name ~reconnection_delay ?protocols cctxt in
  let blocks_cache = Blocks_cache.create l1_blocks_cache_size in
  let headers_cache = Blocks_cache.create l1_blocks_cache_size in
  let cctxt = (cctxt :> Client_context.full) in
  return {l1; cctxt; blocks_cache; headers_cache; prefetch_blocks}

let shutdown {l1; _} = shutdown l1

let cache_shell_header {headers_cache; _} hash header =
  Blocks_cache.put headers_cache hash (Lwt.return_ok header)

let client_context {cctxt; _} = cctxt

let iter_heads ?name l1_ctxt f =
  iter_heads ?name l1_ctxt.l1
  @@ fun (hash, {shell = {level; _} as header; _}) ->
  cache_shell_header l1_ctxt hash header ;
  f {hash; level; header}

let wait_first l1_ctxt =
  let open Lwt_syntax in
  let+ hash, {shell = {level; _} as header; _} = wait_first l1_ctxt.l1 in
  {hash; level; header}

let get_latest_head l1_ctxt =
  Option.map
    (fun (hash, {Tezos_base.Block_header.shell = {level; _} as header; _}) ->
      {hash; level; header})
    (get_latest_head l1_ctxt.l1)

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

(** [fetch_tezos_block cctxt hash] returns a block shell header of
    [hash]. Looks for the block in the blocks cache first, and fetches it from
    the L1 node otherwise. *)
let fetch_tezos_shell_header {cctxt; headers_cache; _} hash =
  trace (Cannot_find_block hash)
  @@
  let fetch hash =
    Tezos_shell_services.Shell_services.Blocks.Header.shell_header
      cctxt
      ~chain:`Main
      ~block:(`Hash (hash, 0))
      ()
  in
  Blocks_cache.bind_or_put headers_cache hash fetch Lwt.return

let fetch_block_no_cache (fetch : fetch_block_rpc) extract_header
    ({cctxt; blocks_cache; _} as l1_ctxt) hash =
  let open Lwt_result_syntax in
  trace (Cannot_find_block hash)
  @@ let* block =
       fetch cctxt ~chain:`Main ~block:(`Hash (hash, 0)) ~metadata:`Always ()
     in
     Blocks_cache.put blocks_cache hash (Lwt.return_ok block) ;
     cache_shell_header l1_ctxt hash (extract_header block) ;
     return block

(** [fetch_tezos_block cctxt fetch extract_header hash] returns a block info
    given a block hash. Looks for the block in the blocks cache first, and
    fetches it from the L1 node otherwise. *)
let fetch_tezos_block (fetch_rpc : fetch_block_rpc) extract_header
    ({cctxt; blocks_cache; _} as l1_ctxt) hash =
  trace (Cannot_find_block hash)
  @@
  let open Lwt_result_syntax in
  let fetch hash =
    let* block =
      fetch_rpc cctxt ~chain:`Main ~block:(`Hash (hash, 0)) ~metadata:`Always ()
    in
    cache_shell_header l1_ctxt hash (extract_header block) ;
    return block
  in
  let*! block = Blocks_cache.bind_or_put blocks_cache hash fetch Lwt.return in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6292
     Consider cleaner ways to "prefetch" tezos blocks:
     - know before where are protocol boundaries
     - prefetch blocks in binary form *)
  let is_of_expected_protocol =
    match block with
    | Error
        (Tezos_rpc_http.RPC_client_errors.(
           Request_failed {error = Unexpected_content _; _})
        :: _) ->
        (* The promise cached failed to parse the block because it was for the
           wrong protocol. *)
        false
    | Error _ ->
        (* The promise cached failed for another reason. *)
        true
    | Ok block -> (
        (* We check if we are able to extract the header, which inherently
           ensures that we are in the correct case of type {!type:block}. *)
        try
          let (_ : Block_header.shell_header) = extract_header block in
          true
        with _ ->
          (* This can happen if the blocks for two protocols have the same
             binary representation. *)
          false)
  in
  if is_of_expected_protocol then Lwt.return block
  else
    (* It is possible for a value stored in the cache to have been parsed
       with the wrong protocol code because:
       1. There is no protocol information in binary blocks
       2. We pre-fetch blocks eagerly.

       If we realize a posteriori that the block we cached is for another
       protocol then we overwrite it with the correctly parsed one.
    *)
    fetch_block_no_cache fetch_rpc extract_header l1_ctxt hash

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

let prefetch_tezos_blocks fetch extract_header l1_ctxt = function
  | [] -> ()
  | blocks ->
      Lwt.async @@ fun () ->
      List.iter_p
        (fun {hash; _} ->
          let open Lwt_syntax in
          let+ _maybe_block =
            fetch_tezos_block fetch extract_header l1_ctxt hash
          in
          ())
        blocks
