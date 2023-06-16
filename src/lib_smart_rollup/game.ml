(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

type dissection_chunk = {state_hash : State_hash.t option; tick : Z.t}

type step = Dissection of dissection_chunk list | Proof of string

type refutation =
  | Start of {
      player_commitment_hash : Commitment.Hash.t;
      opponent_commitment_hash : Commitment.Hash.t;
    }
  | Move of {choice : Z.t; step : step}

type index = {
  alice : Signature.Public_key_hash.t;
  bob : Signature.Public_key_hash.t;
}

let make_index a b =
  let alice, bob =
    if Signature.Public_key_hash.(a > b) then (b, a) else (a, b)
  in
  {alice; bob}

let index_encoding =
  let open Data_encoding in
  conv
    (fun {alice; bob} -> (alice, bob))
    (fun (alice, bob) -> make_index alice bob)
    (obj2
       (req "alice" Signature.Public_key_hash.encoding)
       (req "bob" Signature.Public_key_hash.encoding))

let dissection_chunk_encoding =
  let open Data_encoding in
  conv
    (fun {state_hash; tick} -> (state_hash, tick))
    (fun (state_hash, tick) -> {state_hash; tick})
    (obj2 (opt "state" State_hash.encoding) (req "tick" n))

let dissection_encoding = Data_encoding.list dissection_chunk_encoding

let step_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Dissection"
        (Tag 0)
        dissection_encoding
        (function Dissection d -> Some d | _ -> None)
        (fun d -> Dissection d);
      case
        ~title:"Proof"
        (Tag 1)
        (string' Hex)
        (function Proof p -> Some p | _ -> None)
        (fun p -> Proof p);
    ]

let refutation_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Start"
        (Tag 0)
        (obj3
           (req "refutation_kind" (constant "start"))
           (req "player_commitment_hash" Commitment.Hash.encoding)
           (req "opponent_commitment_hash" Commitment.Hash.encoding))
        (function
          | Start {player_commitment_hash; opponent_commitment_hash} ->
              Some ((), player_commitment_hash, opponent_commitment_hash)
          | _ -> None)
        (fun ((), player_commitment_hash, opponent_commitment_hash) ->
          Start {player_commitment_hash; opponent_commitment_hash});
      case
        ~title:"Move"
        (Tag 1)
        (obj3
           (req "refutation_kind" (constant "move"))
           (req "choice" n)
           (req "step" step_encoding))
        (function Move {choice; step} -> Some ((), choice, step) | _ -> None)
        (fun ((), choice, step) -> Move {choice; step});
    ]
