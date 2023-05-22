(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Octez_smart_rollup

type t =
  | Add_messages of {messages : string list}
  | Cement of {rollup : Address.t; commitment : Commitment.Hash.t}
  | Publish of {rollup : Address.t; commitment : Commitment.t}
  | Refute of {
      rollup : Address.t;
      opponent : Signature.Public_key_hash.t;
      refutation : Game.refutation;
    }
  | Timeout of {rollup : Address.t; stakers : Game.index}

let encoding : t Data_encoding.t =
  let open Data_encoding in
  let case tag kind encoding proj inj =
    case
      ~title:kind
      (Tag tag)
      (merge_objs (obj1 (req "kind" (constant kind))) encoding)
      (fun o -> Option.map (fun p -> ((), p)) (proj o))
      (fun ((), p) -> inj p)
  in
  def "sc_rollup_node_l1_operation"
  @@ union
       [
         case
           0
           "add_messages"
           (obj1 (req "message" (list (string' Hex))))
           (function Add_messages {messages} -> Some messages | _ -> None)
           (fun messages -> Add_messages {messages});
         case
           1
           "cement"
           (obj2
              (req "rollup" Address.encoding)
              (req "commitment" Commitment.Hash.encoding))
           (function
             | Cement {rollup; commitment} -> Some (rollup, commitment)
             | _ -> None)
           (fun (rollup, commitment) -> Cement {rollup; commitment});
         case
           2
           "publish"
           (obj2
              (req "rollup" Address.encoding)
              (req "commitment" Commitment.encoding))
           (function
             | Publish {rollup; commitment} -> Some (rollup, commitment)
             | _ -> None)
           (fun (rollup, commitment) -> Publish {rollup; commitment});
         case
           3
           "refute"
           (obj3
              (req "rollup" Address.encoding)
              (req "opponent" Signature.Public_key_hash.encoding)
              (req "refutation" Game.refutation_encoding))
           (function
             | Refute {rollup; opponent; refutation} ->
                 Some (rollup, opponent, refutation)
             | _ -> None)
           (fun (rollup, opponent, refutation) ->
             Refute {rollup; opponent; refutation});
         case
           4
           "timeout"
           (obj2
              (req "rollup" Address.encoding)
              (req "stakers" Game.index_encoding))
           (function
             | Timeout {rollup; stakers} -> Some (rollup, stakers) | _ -> None)
           (fun (rollup, stakers) -> Timeout {rollup; stakers});
       ]

let pp ppf = function
  | Add_messages {messages} ->
      Format.fprintf
        ppf
        "publishing %d messages to smart rollups' inbox"
        (List.length messages)
  | Cement {rollup = _; commitment} when Commitment.Hash.(commitment = zero) ->
      (* We use zero as a default value for protocol alpha which does
         not need the commitment to be specified. *)
      Format.fprintf ppf "cementing cementable commitment"
  | Cement {rollup = _; commitment} ->
      Format.fprintf ppf "cementing commitment %a" Commitment.Hash.pp commitment
  | Publish {rollup = _; commitment = Commitment.{inbox_level; _}} ->
      Format.fprintf ppf "publish commitment for level %ld" inbox_level
  | Refute {rollup = _; opponent; refutation = Start _} ->
      Format.fprintf
        ppf
        "start refutation game against %a"
        Signature.Public_key_hash.pp
        opponent
  | Refute
      {
        rollup = _;
        opponent;
        refutation = Move {step = Dissection (first :: _ as d); _};
      } ->
      let last = List.last first d in
      Format.fprintf
        ppf
        "dissection between ticks %a and %a (against %a)"
        Z.pp_print
        first.tick
        Z.pp_print
        last.tick
        Signature.Public_key_hash.pp
        opponent
  | Refute {rollup = _; opponent; refutation = Move {step = Dissection []; _}}
    ->
      Format.fprintf
        ppf
        "dissection (against %a)"
        Signature.Public_key_hash.pp
        opponent
  | Refute {rollup = _; opponent; refutation = Move {choice; step = Proof _}} ->
      Format.fprintf
        ppf
        "proof for tick %a  (against %a)"
        Z.pp_print
        choice
        Signature.Public_key_hash.pp
        opponent
  | Timeout {rollup = _; stakers = _} -> Format.fprintf ppf "timeout"

let unique = function
  | Add_messages _ | Cement _ -> false
  | Publish _ | Refute _ | Timeout _ -> true
