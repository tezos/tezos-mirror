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

module M = struct
  type t = unit

  type key = string list

  type value = Bytes.t

  type tree = |

  module Tree = struct
    let pp _ _ = assert false

    let hash _ = assert false

    let empty _ = assert false

    let equal _ _ = assert false

    let is_empty _ = assert false

    let mem _ _ = assert false

    let kind _ = assert false

    let to_value _ = assert false

    let of_value _ _ = assert false

    let find _ _ = assert false

    let add _ _ _ = assert false

    let remove _ _ = assert false

    let mem_tree _ _ = assert false

    let find_tree _ _ = assert false

    let add_tree _ _ = assert false

    let clear ?depth:_ _ = assert false

    let list _ ?offset:_ ?length:_ _ = assert false

    let fold ?depth:_ _ _ ~order:_ ~init:_ ~f:_ = assert false
  end

  include Tree

  let set_protocol _ _ = assert false

  let get_protocol _ = assert false

  let fork_test_chain _ ~protocol:_ ~expiration:_ = assert false

  let set_hash_version _ _ = assert false

  let get_hash_version _ = assert false
end

open Tezos_protocol_environment
include Environment_context.Register (M)

let empty =
  Context.make ~ops ~ctxt:() ~kind:Context ~equality_witness ~impl_name:"dummy"
