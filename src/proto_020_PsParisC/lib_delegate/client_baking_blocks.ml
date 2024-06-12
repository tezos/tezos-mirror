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

open Protocol
open Alpha_context
open Baking_errors

type block_info = {
  hash : Block_hash.t;
  chain_id : Chain_id.t;
  predecessor : Block_hash.t;
  fitness : Bytes.t list;
  timestamp : Time.Protocol.t;
  protocol : Protocol_hash.t;
  next_protocol : Protocol_hash.t;
  proto_level : int;
  level : Raw_level.t;
  context : Context_hash.t;
}

let raw_info cctxt ?(chain = `Main) hash shell_header =
  let open Lwt_result_syntax in
  let block = `Hash (hash, 0) in
  let* chain_id = Shell_services.Chain.chain_id cctxt ~chain () in
  let* {current_protocol = protocol; next_protocol} =
    Shell_services.Blocks.protocols cctxt ~chain ~block ()
  in
  let {
    Tezos_base.Block_header.predecessor;
    fitness;
    timestamp;
    level;
    context;
    proto_level;
    _;
  } =
    shell_header
  in
  match Raw_level.of_int32 level with
  | Ok level ->
      return
        {
          hash;
          chain_id;
          predecessor;
          fitness;
          timestamp;
          protocol;
          next_protocol;
          proto_level;
          level;
          context;
        }
  | Error _ -> failwith "Cannot convert level into int32"

let info cctxt ?(chain = `Main) block =
  let open Lwt_result_syntax in
  let* hash = Shell_services.Blocks.hash cctxt ~chain ~block () in
  let* shell_header =
    Shell_services.Blocks.Header.shell_header cctxt ~chain ~block ()
  in
  raw_info cctxt ~chain hash shell_header

module Block_seen_event = struct
  type t = {
    hash : Block_hash.t;
    header : Tezos_base.Block_header.t;
    occurrence : [`Valid_blocks of Chain_id.t | `Heads];
  }

  let make hash header occurrence = {hash; header; occurrence}

  module Definition = struct
    let section = None

    let name = "block-seen-" ^ Protocol.name

    type nonrec t = t

    let encoding =
      let open Data_encoding in
      let v0_encoding =
        conv
          (function {hash; header; occurrence} -> (hash, occurrence, header))
          (fun (hash, occurrence, header) -> make hash header occurrence)
          (obj3
             (req "hash" Block_hash.encoding)
             (* Occurrence has to come before header, because:
                (Invalid_argument
                   "Cannot merge two objects when the left element is of
                    variable length and the right one of dynamic
                    length. You should use the reverse order, or wrap the
                    second one with Data_encoding.dynamic_size.") *)
             (req
                "occurrence"
                (union
                   [
                     case
                       ~title:"heads"
                       (Tag 0)
                       (obj1 (req "occurrence-kind" (constant "heads")))
                       (function `Heads -> Some () | _ -> None)
                       (fun () -> `Heads);
                     case
                       ~title:"valid-blocks"
                       (Tag 1)
                       (obj2
                          (req "occurrence-kind" (constant "valid-blocks"))
                          (req "chain-id" Chain_id.encoding))
                       (function
                         | `Valid_blocks ch -> Some ((), ch) | _ -> None)
                       (fun ((), ch) -> `Valid_blocks ch);
                   ]))
             (req "header" Tezos_base.Block_header.encoding))
      in
      With_version.(encoding ~name (first_version v0_encoding))

    let pp ~all_fields:_ ~block:_ ppf {hash; _} =
      Format.fprintf ppf "Saw block %a" Block_hash.pp_short hash

    let doc = "Block observed while monitoring a blockchain."

    let level = Internal_event.Info
  end

  module Event = Internal_event.Make (Definition)
end

let monitor_applied_blocks cctxt ?chains ?protocols ~next_protocols () =
  let open Lwt_result_syntax in
  let* block_stream, stop =
    Monitor_services.applied_blocks cctxt ?chains ?protocols ?next_protocols ()
  in
  return
    ( Lwt_stream.map_s
        (fun (chain, block, header, _ops) ->
          let* () =
            Block_seen_event.(
              Event.emit (make block header (`Valid_blocks chain)))
          in
          raw_info
            cctxt
            ~chain:(`Hash chain)
            block
            header.Tezos_base.Block_header.shell)
        block_stream,
      stop )

let monitor_heads cctxt ~next_protocols chain =
  let open Lwt_result_syntax in
  let* block_stream, stop =
    Monitor_services.heads cctxt ?next_protocols chain
  in
  return
    ( Lwt_stream.map_s
        (fun (block, ({Tezos_base.Block_header.shell; _} as header)) ->
          let* () = Block_seen_event.(Event.emit (make block header `Heads)) in
          raw_info cctxt ~chain block shell)
        block_stream,
      stop )

let blocks_from_current_cycle cctxt ?(chain = `Main) block ?(offset = 0l) () =
  let open Lwt_result_syntax in
  let* hash = Shell_services.Blocks.hash cctxt ~chain ~block () in
  let* {level; _} =
    Shell_services.Blocks.Header.shell_header cctxt ~chain ~block ()
  in
  let*! result =
    Plugin.RPC.levels_in_current_cycle cctxt ~offset (chain, block)
  in
  match result with
  | Error (Tezos_rpc.Context.Not_found _ :: _) -> return_nil
  | Error _ as err -> Lwt.return err
  | Ok (first, last) ->
      let length = Int32.to_int (Int32.sub level (Raw_level.to_int32 first)) in
      let* head =
        let* list =
          Shell_services.Blocks.list cctxt ~chain ~heads:[hash] ~length ()
        in
        match list with
        | hd :: _ -> return hd
        | [] ->
            tzfail
              (Unexpected_empty_block_list
                 {
                   chain = Block_services.chain_to_string chain;
                   block_hash = hash;
                   length;
                 })
      in
      let blocks =
        List.drop_n (length - Int32.to_int (Raw_level.diff last first)) head
      in
      if Int32.equal level (Raw_level.to_int32 last) then return (hash :: blocks)
      else return blocks
