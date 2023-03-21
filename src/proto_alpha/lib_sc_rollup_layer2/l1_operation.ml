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

open Protocol.Alpha_context

type t =
  | Add_messages of {messages : string list}
  | Cement of {rollup : Sc_rollup.t; commitment : Sc_rollup.Commitment.Hash.t}
  | Publish of {rollup : Sc_rollup.t; commitment : Sc_rollup.Commitment.t}
  | Refute of {
      rollup : Sc_rollup.t;
      opponent : Sc_rollup.Staker.t;
      refutation : Sc_rollup.Game.refutation;
    }
  | Timeout of {rollup : Sc_rollup.t; stakers : Sc_rollup.Game.Index.t}

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
              (req "rollup" Sc_rollup.Address.encoding)
              (req "commitment" Sc_rollup.Commitment.Hash.encoding))
           (function
             | Cement {rollup; commitment} -> Some (rollup, commitment)
             | _ -> None)
           (fun (rollup, commitment) -> Cement {rollup; commitment});
         case
           2
           "publish"
           (obj2
              (req "rollup" Sc_rollup.Address.encoding)
              (req "commitment" Sc_rollup.Commitment.encoding))
           (function
             | Publish {rollup; commitment} -> Some (rollup, commitment)
             | _ -> None)
           (fun (rollup, commitment) -> Publish {rollup; commitment});
         case
           3
           "refute"
           (obj3
              (req "rollup" Sc_rollup.Address.encoding)
              (req "opponent" Sc_rollup.Staker.encoding)
              (req "refutation" Sc_rollup.Game.refutation_encoding))
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
              (req "rollup" Sc_rollup.Address.encoding)
              (req "stakers" Sc_rollup.Game.Index.encoding))
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
  | Cement {rollup = _; commitment} ->
      Format.fprintf
        ppf
        "cementing commitment %a"
        Sc_rollup.Commitment.Hash.pp
        commitment
  | Publish {rollup = _; commitment = Sc_rollup.Commitment.{inbox_level; _}} ->
      Format.fprintf
        ppf
        "publish commitment for level %a"
        Raw_level.pp
        inbox_level
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
        Sc_rollup.Tick.pp
        first.tick
        Sc_rollup.Tick.pp
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
        Sc_rollup.Tick.pp
        choice
        Signature.Public_key_hash.pp
        opponent
  | Timeout {rollup = _; stakers = _} -> Format.fprintf ppf "timeout"

let to_manager_operation : t -> packed_manager_operation = function
  | Add_messages {messages} -> Manager (Sc_rollup_add_messages {messages})
  | Cement {rollup; commitment} ->
      Manager (Sc_rollup_cement {rollup; commitment})
  | Publish {rollup; commitment} ->
      Manager (Sc_rollup_publish {rollup; commitment})
  | Refute {rollup; opponent; refutation} ->
      Manager (Sc_rollup_refute {rollup; opponent; refutation})
  | Timeout {rollup; stakers} -> Manager (Sc_rollup_timeout {rollup; stakers})

let of_manager_operation : type kind. kind manager_operation -> t option =
  function
  | Sc_rollup_add_messages {messages} -> Some (Add_messages {messages})
  | Sc_rollup_cement {rollup; commitment} -> Some (Cement {rollup; commitment})
  | Sc_rollup_publish {rollup; commitment} ->
      Some (Publish {rollup; commitment})
  | Sc_rollup_refute {rollup; opponent; refutation} ->
      Some (Refute {rollup; opponent; refutation})
  | Sc_rollup_timeout {rollup; stakers} -> Some (Timeout {rollup; stakers})
  | _ -> None

let unique = function
  | Add_messages _ -> false
  | Cement _ | Publish _ | Refute _ | Timeout _ -> true
