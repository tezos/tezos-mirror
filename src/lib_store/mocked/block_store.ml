(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type merge_status = Not_running | Running | Merge_failed of tztrace

type key = Block of (Block_hash.t * int)

module Block_table = Block_hash.Table

module Level_table = Hashtbl.Make (struct
  type t = int32

  let equal = Int32.equal

  let hash = Stdlib.Hashtbl.hash
end)

type block_store = {
  genesis_block : Block_repr.t;
  block_table : (Block_repr.t * Context_hash.t) Block_table.t;
  level_table : (Block_repr.t * Context_hash.t) Level_table.t;
  mutable caboose : Store_types.block_descriptor;
}

type t = block_store

let genesis_block {genesis_block; _} = genesis_block

let get_block_by_key (Block (hash, off)) {block_table; level_table; _} =
  match Block_table.find_opt block_table hash with
  | None -> None
  | Some (block, _) as res ->
      let off = Int32.of_int off in
      if off = 0l then res
      else
        let level = Block_repr.level block in
        let with_off = Int32.sub level off in
        Level_table.find_opt level_table with_off

let mem block_store key =
  Lwt_result_syntax.return
  @@
  match get_block_by_key key block_store with None -> false | Some _ -> true

let read_block ~(read_metadata : bool) block_store key =
  ignore read_metadata ;
  Lwt_result.return (Option.map fst (get_block_by_key key block_store))

let get_hash block_store key =
  let block_opt = get_block_by_key key block_store in
  Lwt_result.return (Option.map (fun (b, _) -> Block_repr.hash b) block_opt)

let read_block_metadata block_store key =
  let open Lwt_result_syntax in
  let* block_opt = read_block ~read_metadata:true block_store key in
  match block_opt with
  | None -> return_none
  | Some block -> return (Block_repr.metadata block)

let resulting_context_hash block_store ~expect_predecessor_context:_ key =
  Lwt_result.return (Option.map snd (get_block_by_key key block_store))

let store_block block_store block resulting_context_hash =
  let level = Block_repr.level block in
  let hash = Block_repr.hash block in
  Block_table.add block_store.block_table hash (block, resulting_context_hash) ;
  Level_table.add block_store.level_table level (block, resulting_context_hash) ;
  Lwt_result_syntax.return_unit

let caboose {caboose; _} = Lwt.return caboose

let create ?block_cache_limit:_ _chain_dir ~genesis_block =
  let open Lwt_result_syntax in
  let block_store =
    {
      genesis_block;
      block_table = Block_table.create 11;
      level_table = Level_table.create 11;
      caboose = (Block_repr.hash genesis_block, Block_repr.level genesis_block);
    }
  in
  let* () =
    store_block block_store genesis_block (Block_repr.context genesis_block)
  in

  return block_store

let close (_block_store : t) = Lwt.return_unit
