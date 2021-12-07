(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module Set = Operation_hash.Set
module Map = Operation_hash.Map

(*
   This type is used for data representing pending operations of the
   prevalidator. The operations of this module (should) maintain the invariant:
   keys (pending) = elements(hashes)
*)

type t = {
  pending : Operation.t Operation_hash.Map.t;
  hashes : Operation_hash.Set.t;
}

let empty = {pending = Map.empty; hashes = Set.empty}

let from_operations pending =
  {
    pending;
    hashes = Map.fold (fun oph _op set -> Set.add oph set) pending Set.empty;
  }

let is_empty {pending = _; hashes} = Set.is_empty hashes

let hashes {pending = _; hashes} = hashes

let operations {pending; hashes = _} = pending

let mem oph {hashes; pending = _} = Set.mem oph hashes

let add oph op {pending; hashes} =
  {pending = Map.add oph op pending; hashes = Set.add oph hashes}

let remove oph {pending; hashes} =
  {pending = Map.remove oph pending; hashes = Set.remove oph hashes}

let cardinal {pending = _; hashes} = Set.cardinal hashes

let fold_es f {pending; hashes = _} = Map.fold_es f pending

let fold f {pending; hashes = _} = Map.fold f pending

let iter f {pending; hashes = _} = Map.iter f pending
