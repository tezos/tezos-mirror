(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = {
  applied : string list;
  branch_delayed : string list;
  branch_refused : string list;
  refused : string list;
  outdated : string list;
  unprocessed : string list;
}

(* A comparable type for mempool where classification and ordering
   does not matter. *)
let typ : t Check.typ =
  let open Check in
  let sort = List.sort compare in
  convert
    (fun mempool ->
      sort
        (mempool.applied
        @ sort mempool.branch_delayed
        @ sort mempool.branch_refused
        @ sort mempool.refused @ sort mempool.outdated
        @ sort mempool.unprocessed))
    (list string)

(* A comparable type for mempool where ordering does not matter. *)
let classified_typ : t Check.typ =
  let open Check in
  let sort = List.sort compare in
  convert
    (fun mempool ->
      [
        sort mempool.applied;
        sort mempool.branch_delayed;
        sort mempool.branch_refused;
        sort mempool.refused;
        sort mempool.outdated;
        sort mempool.unprocessed;
      ])
    (list (list string))

let empty =
  {
    applied = [];
    branch_delayed = [];
    branch_refused = [];
    refused = [];
    outdated = [];
    unprocessed = [];
  }

let symmetric_diff left right =
  let diff left right =
    List.(
      filter (fun op -> not (mem op right)) left
      @ filter (fun op -> not (mem op left)) right)
  in
  {
    applied = diff left.applied right.applied;
    branch_delayed = diff left.branch_delayed right.branch_delayed;
    branch_refused = diff left.branch_refused right.branch_refused;
    refused = diff left.refused right.refused;
    outdated = diff left.outdated right.outdated;
    unprocessed = diff left.unprocessed right.unprocessed;
  }

let of_json mempool_json =
  let get_hash op = JSON.(op |-> "hash" |> as_string) in
  let get_hashes classification =
    List.map get_hash JSON.(mempool_json |-> classification |> as_list)
  in
  let applied = get_hashes "applied" in
  let branch_delayed = get_hashes "branch_delayed" in
  let branch_refused = get_hashes "branch_refused" in
  let refused = get_hashes "refused" in
  let outdated = get_hashes "outdated" in
  let unprocessed = get_hashes "unprocessed" in
  {applied; branch_delayed; branch_refused; refused; outdated; unprocessed}

let get_mempool ?endpoint ?hooks ?chain ?(applied = true)
    ?(branch_delayed = true) ?(branch_refused = true) ?(refused = true)
    ?(outdated = true) ?(validation_passes = []) client =
  let* mempool_json =
    RPC.Client.call client ?hooks ?endpoint
    @@ RPC.get_chain_mempool_pending_operations
         ?chain
         ~version:"1"
         ~applied
         ~branch_delayed
         ~branch_refused
         ~refused
         ~outdated
         ~validation_passes
         ()
  in
  return (of_json mempool_json)

let check_mempool ?(applied = []) ?(branch_delayed = []) ?(branch_refused = [])
    ?(refused = []) ?(outdated = []) ?(unprocessed = []) mempool =
  let expected_mempool =
    {applied; branch_delayed; branch_refused; refused; outdated; unprocessed}
  in
  Check.(
    (expected_mempool = mempool)
      classified_typ
      ~error_msg:"Expected mempool %L, got %R")
