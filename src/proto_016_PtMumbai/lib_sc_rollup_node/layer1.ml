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

type head = {hash : Block_hash.t; level : int32}

let head_encoding =
  Data_encoding.(
    conv
      (fun {hash; level} -> (hash, level))
      (fun (hash, level) -> {hash; level})
      (obj2 (req "hash" Block_hash.encoding) (req "level" Data_encoding.int32)))

module Blocks_cache =
  Aches_lwt.Lache.Make_option
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Block_hash))

type blocks_cache = Alpha_block_services.block_info Blocks_cache.t

(** Global blocks cache for the smart rollup node. *)
let blocks_cache : blocks_cache = Blocks_cache.create 32

include Octez_crawler.Layer_1

let iter_heads l1_ctxt f =
  iter_heads l1_ctxt @@ fun (hash, {shell = {level; _}; _}) -> f {hash; level}

(**

   Helpers
   =======

*)

(** [fetch_tezos_block l1_ctxt hash] returns a block shell header of
    [hash]. Looks for the block in the blocks cache first, and fetches it from
    the L1 node otherwise. *)
let fetch_tezos_shell_header cctxt hash =
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
    let res =
      Blocks_cache.bind blocks_cache hash (function
          | Some block_info -> Lwt.return_some block_info.header.shell
          | None -> Lwt.return_none)
    in
    match res with Some lwt -> lwt | None -> fetch hash
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
let fetch_tezos_block cctxt hash =
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
  | Some block, _ -> Ok block
