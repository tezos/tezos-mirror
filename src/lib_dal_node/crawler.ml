(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

module Crawler_lib = Octez_crawler.Layer_1
module Blocks_cache =
  Aches_lwt.Lache.Make_result
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Block_hash))

type headers_cache = (Block_header.shell_header, tztrace) Blocks_cache.t

type error += Cannot_find_block of Block_hash.t

type block_info = Block_hash.t * Block_header.shell_header

type finalized_heads = {
  stream : block_info Lwt_stream.t;
  stream_push : block_info option -> unit;
  iter_heads_promise : unit tzresult Lwt.t;
}

type t = {
  crawler_lib : Crawler_lib.t;
  headers_cache : headers_cache;  (** Global block headers cache. *)
  cctxt : Tezos_rpc.Context.generic;
  finalized_heads : finalized_heads;
  last_seen_head : block_info option ref;
}

let () =
  register_error_kind
    ~id:"dal.node.cannot_find_block"
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

let cache_shell_header headers_cache hash shell_header =
  Blocks_cache.put headers_cache hash (Lwt.return_ok shell_header)

(* Returns a block shell header of [hash]. Looks for the block in the blocks
   cache first, and fetches it from the L1 node otherwise. *)
let fetch_tezos_shell_header cctxt headers_cache hash =
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

(* This function uses a cache on the crawler_lib side. *)
let get_predecessor crawler_lib hash level =
  Crawler_lib.get_predecessor crawler_lib (hash, level)

(* This function initiates a call to function iter_heads and pushes finalized L1
   heads into the stream whose push function is given. *)
let finalized_heads_monitor ~name ~last_notified_level ~last_seen_head_ref
    crawler_lib cctxt headers_cache stream_push =
  let open Lwt_result_syntax in
  let last_notified_level = ref last_notified_level in
  let rec catch_up_if_needed hash (shell_header : Block_header.shell_header) acc
      =
    if shell_header.level <= !last_notified_level then
      (* This block header is already notified or not targetted by the user. *)
      return acc
    else
      let*! res = get_predecessor crawler_lib hash shell_header.level in
      match res with
      | Error error ->
          let*! () =
            Event.emit_failed_to_fetch_block
              ~type_:"hash"
              ~level:(Int32.pred shell_header.level)
              ~last_notified:!last_notified_level
              ~error
          in
          return acc
      | Ok (pred_hash, _level) -> (
          let*! res = fetch_tezos_shell_header cctxt headers_cache pred_hash in
          match res with
          | Error error ->
              let*! () =
                Event.emit_failed_to_fetch_block
                  ~type_:"hash"
                  ~level:(Int32.pred shell_header.level)
                  ~last_notified:!last_notified_level
                  ~error
              in
              return acc
          | Ok pred_shell_header ->
              catch_up_if_needed
                pred_hash
                pred_shell_header
                ((hash, shell_header) :: acc))
  in
  let process (hash, Block_header.{shell = shell_header; _}) =
    let shell_header_level = shell_header.level in
    let*! () =
      Event.emit_layer1_node_new_head
        ~hash
        ~level:shell_header_level
        ~fitness:shell_header.fitness
    in
    last_seen_head_ref := Some (hash, shell_header) ;
    Dal_metrics.new_layer1_head ~head_level:shell_header_level ;
    cache_shell_header headers_cache hash shell_header ;
    if shell_header_level <= !last_notified_level then return_unit
    else if shell_header_level <= 2l then return_unit
    else
      let* pred_hash, pred_level =
        get_predecessor crawler_lib hash shell_header_level
      in
      let* finalized_hash, finalized_level =
        get_predecessor crawler_lib pred_hash pred_level
      in
      let* finalized_shell_header =
        fetch_tezos_shell_header cctxt headers_cache finalized_hash
      in
      let* delta_finalized =
        catch_up_if_needed finalized_hash finalized_shell_header []
      in
      List.iter (fun header -> stream_push (Some header)) delta_finalized ;
      last_notified_level := finalized_level ;
      return_unit
  in
  let opt = Crawler_lib.get_latest_head crawler_lib in
  let* () =
    match opt with
    | None -> return_unit
    | Some hash_and_header -> process hash_and_header
  in
  Crawler_lib.iter_heads ~name crawler_lib process

let start ~name ~chain ~reconnection_delay ~l1_blocks_cache_size
    ?last_notified_level cctxt =
  let open Lwt_syntax in
  let* crawler_lib = Crawler_lib.start ~name ~chain ~reconnection_delay cctxt in
  let* last_notified_level =
    match last_notified_level with
    | Some l -> return l
    | None ->
        let opt = Crawler_lib.get_latest_head crawler_lib in
        let* _hash, header =
          match opt with
          | Some hash_and_header -> return hash_and_header
          | None -> Crawler_lib.wait_first crawler_lib
        in
        return (Int32.sub header.shell.level 2l)
  in
  let last_notified_level = Int32.max 0l last_notified_level in
  let headers_cache = Blocks_cache.create l1_blocks_cache_size in
  let stream, stream_push = Lwt_stream.create () in
  let last_seen_head_ref = ref None in
  let iter_heads_promise =
    finalized_heads_monitor
      ~name
      ~last_notified_level
      ~last_seen_head_ref
      crawler_lib
      cctxt
      headers_cache
      stream_push
  in
  let finalized_heads = {stream; stream_push; iter_heads_promise} in
  return
    {
      crawler_lib;
      cctxt;
      headers_cache;
      finalized_heads;
      last_seen_head = last_seen_head_ref;
    }

let finalized_heads_stream {finalized_heads; _} =
  Lwt_stream.clone finalized_heads.stream

let shutdown {finalized_heads = {stream_push; iter_heads_promise; _}; _} =
  stream_push None ;
  Lwt.cancel iter_heads_promise

let last_seen_head {last_seen_head; _} = !last_seen_head
