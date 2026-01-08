(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
  head_hash : Tezos_crypto.Hashed.Block_hash.t;
  head_header : Block_header.t;
  history : Tezos_crypto.Hashed.Block_hash.t list;
}

let pp ppf {head_hash; history; _} =
  let repeats = 10 in
  let coef = 2 in
  (* list of hashes *)
  let rec pp_hash_list ppf (h_lst, acc, d, r) =
    match h_lst with
    | [] -> Format.fprintf ppf ""
    | hd :: tl ->
        let new_d = if r > 1 then d else d * coef in
        let new_r = if r > 1 then r - 1 else repeats in
        Format.fprintf
          ppf
          "%a (%i)\n%a"
          Tezos_crypto.Hashed.Block_hash.pp
          hd
          acc
          pp_hash_list
          (tl, acc - d, new_d, new_r)
  in
  Format.fprintf
    ppf
    "%a (head)\n%a"
    Tezos_crypto.Hashed.Block_hash.pp
    head_hash
    pp_hash_list
    (history, -1, 1, repeats - 1)

let pp_short ppf {head_hash; history; _} =
  Format.fprintf
    ppf
    "head: %a, %d predecessors"
    Tezos_crypto.Hashed.Block_hash.pp
    head_hash
    (List.length history)

let encoding_proj {head_header; history; _} = (head_header, history)

let encoding_inj (head_header, history) =
  {head_hash = Block_header.hash head_header; head_header; history}

let encoding =
  let open Data_encoding in
  def "block_locator" ~description:"A sparse block locator à la Bitcoin"
  @@ conv
       encoding_proj
       encoding_inj
       (obj2
          (req "current_head" (dynamic_size Block_header.encoding))
          (req
             "history"
             (Variable.list Tezos_crypto.Hashed.Block_hash.encoding)))

let bounded_encoding ~max_header_size ~max_length () =
  let open Data_encoding in
  conv
    encoding_proj
    encoding_inj
    (obj2
       (req
          "current_head"
          (dynamic_size
             (Block_header.bounded_encoding ~max_size:max_header_size ())))
       (req
          "history"
          (Variable.list ~max_length Tezos_crypto.Hashed.Block_hash.encoding)))

type seed = {sender_id : P2p_peer.Id.t; receiver_id : P2p_peer.Id.t}

(* Random generator for locator steps.

   We draw steps by sequence of 10. The first sequence's steps are of
   length 1 (consecutive). The second sequence's steps are of a random
   length between 1 and 2. The third sequence's steps are of a random
   length between 2 and 4, and so on...

   The sequence is deterministic for a given triple of sender,
   receiver and block hash. *)
module Step : sig
  type state

  val init : seed -> Tezos_crypto.Hashed.Block_hash.t -> state

  val next : state -> int * state
end = struct
  (* (step, counter, seed) .
     The seed is stored in a bigstring and should be mlocked *)
  type state = Int32.t * int * Bytes.t

  let update st b = Tezos_crypto.Hacl.Hash.SHA256.update st b

  let init seed head =
    let open Tezos_crypto.Hacl.Hash in
    let st = SHA256.init () in
    List.iter
      (update st)
      [
        P2p_peer.Id.to_bytes seed.sender_id;
        P2p_peer.Id.to_bytes seed.receiver_id;
        Tezos_crypto.Hashed.Block_hash.to_bytes head;
      ] ;
    (1l, 9, SHA256.finish st)

  let draw seed n =
    ( Int32.rem (TzEndian.get_int32 seed 0) n,
      Tezos_crypto.Hacl.Hash.SHA256.digest seed )

  let next (step, counter, seed) =
    let random_gap, seed =
      if step <= 1l then (0l, seed)
      else draw seed (Int32.succ (Int32.div step 2l))
    in
    let new_state =
      if counter = 0 then (Int32.mul step 2l, 9, seed)
      else (step, counter - 1, seed)
    in
    (Int32.to_int (Int32.sub step random_gap), new_state)
end

let estimated_length seed {head_hash; history; _} =
  let rec loop acc state = function
    | [] -> acc
    | _ :: hist ->
        let step, state = Step.next state in
        loop (acc + step) state hist
  in
  let state = Step.init seed head_hash in
  let step, state = Step.next state in
  loop step state history

let fold ~f ~init {head_hash; history; _} seed =
  let rec loop state acc = function
    | [] | [_] -> acc
    | block :: (pred :: rem as hist) ->
        let step, state = Step.next state in
        let acc = f acc ~block ~pred ~step ~strict_step:(rem <> []) in
        loop state acc hist
  in
  let state = Step.init seed head_hash in
  loop state init (head_hash :: history)

type step = {
  block : Tezos_crypto.Hashed.Block_hash.t;
  predecessor : Tezos_crypto.Hashed.Block_hash.t;
  step : int;
  strict_step : bool;
}

let pp_step ppf step =
  Format.fprintf ppf "%d%s" step.step (if step.strict_step then "" else " max")

let to_steps seed locator =
  fold locator seed ~init:[] ~f:(fun acc ~block ~pred ~step ~strict_step ->
      {block; predecessor = pred; step; strict_step} :: acc)

let fold_truncate ~f ~init ~save_point ~limit {head_hash; history; _} seed =
  let rec loop state step_sum acc = function
    | [] | [_] -> acc
    | block :: (pred :: rem as hist) ->
        let step, state = Step.next state in
        let new_step_sum = step + step_sum in
        if new_step_sum >= limit then
          f acc ~block ~pred:save_point ~step ~strict_step:false
        else
          let acc = f acc ~block ~pred ~step ~strict_step:(rem <> []) in
          loop state new_step_sum acc hist
  in
  let initial_state = Step.init seed head_hash in
  loop initial_state 0 init (head_hash :: history)

let to_steps_truncate ~limit ~save_point seed locator =
  fold_truncate
    locator
    seed
    ~init:[]
    ~save_point
    ~limit
    ~f:(fun acc ~block ~pred ~step ~strict_step ->
      {block; predecessor = pred; step; strict_step} :: acc)

let compute ~get_predecessor ~caboose ~size head_hash head_header seed =
  let open Error_monad.Lwt_syntax in
  let rec loop acc size state current_block_hash =
    if size = 0 then Lwt.return acc
    else
      let step, state = Step.next state in
      let* o = get_predecessor current_block_hash step in
      match o with
      | None ->
          if Tezos_crypto.Hashed.Block_hash.equal caboose current_block_hash
          then Lwt.return acc
          else Lwt.return (caboose :: acc)
      | Some predecessor ->
          if Tezos_crypto.Hashed.Block_hash.equal predecessor current_block_hash
          then
            (* caboose or genesis reached *)
            Lwt.return acc
          else loop (predecessor :: acc) (pred size) state predecessor
  in
  if size <= 0 then Lwt.return {head_hash; head_header; history = []}
  else
    let initial_state = Step.init seed head_hash in
    let* history = loop [] size initial_state head_hash in
    let history = List.rev history in
    Lwt.return {head_hash; head_header; history}

type validity = Unknown | Known_valid | Known_invalid

let unknown_prefix ~is_known locator =
  let open Error_monad.Lwt_syntax in
  let {head_hash; history; _} = locator in
  let rec loop hist acc =
    match hist with
    | [] -> Lwt.return (Unknown, locator)
    | h :: t -> (
        let* k = is_known h in
        match k with
        | Known_valid ->
            Lwt.return
              (Known_valid, {locator with history = List.rev (h :: acc)})
        | Known_invalid ->
            Lwt.return
              (Known_invalid, {locator with history = List.rev (h :: acc)})
        | Unknown -> loop t (h :: acc))
  in
  let* k = is_known head_hash in
  match k with
  | Known_valid -> Lwt.return (Known_valid, {locator with history = []})
  | Known_invalid -> Lwt.return (Known_invalid, {locator with history = []})
  | Unknown -> loop history []

let () = Data_encoding.Registration.register ~pp:pp_short encoding
