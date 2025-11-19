(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type failure = {level : int; message_index : int; message_tick : int64}

let failure_encoding =
  let open Data_encoding in
  conv
    (fun {level; message_index; message_tick} ->
      (level, message_index, message_tick))
    (fun (level, message_index, message_tick) ->
      {level; message_index; message_tick})
    (obj3
       (req "level" int31)
       (req "message_index" int31)
       (req "message_tick" int64))

let compare_failure {level; message_index; message_tick} f2 =
  let open Compare.Int in
  match compare level f2.level with
  | 0 -> (
      match compare message_index f2.message_index with
      | 0 -> Int64.compare message_tick f2.message_tick
      | n -> n)
  | n -> n

type dal_parameters = {
  number_of_slots : int64;
  attestation_lag : int64;
  slot_size : int64;
  page_size : int64;
}

(** See the mli file for the doc. *)
type dal_page = {
  published_level : int32 option;
  slot_index : int option;
  page_index : int option;
  page_payload_strategy : [`Alter | `Flip];
}

type t =
  | Failure of failure list
  | Invalid_dal_parameters of dal_parameters
  | Invalid_dal_page of dal_page

let page_payload_strategy_of_string s =
  match String.lowercase_ascii s with
  | "flip" -> `Flip
  | "alter" -> `Alter
  | s -> Stdlib.failwith ("Unknown DAL content strategy " ^ s)

let page_payload_strategy_to_string strat =
  match strat with `Flip -> "flip" | `Alter -> "alter"

let encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"failure"
        (list failure_encoding)
        (function Failure l -> Some l | _ -> None)
        (fun l -> Failure l);
      case
        (Tag 1)
        ~title:"Invalid_dal_parameters"
        (obj4
           (req "number_of_slots" int64)
           (req "attestation_lag" int64)
           (req "slot_size" int64)
           (req "page_size" int64))
        (function
          | Invalid_dal_parameters
              {number_of_slots; attestation_lag; slot_size; page_size} ->
              Some (number_of_slots, attestation_lag, slot_size, page_size)
          | _ -> None)
        (fun (number_of_slots, attestation_lag, slot_size, page_size) ->
          Invalid_dal_parameters
            {number_of_slots; attestation_lag; slot_size; page_size});
      case
        (Tag 2)
        ~title:"Invalid_dal_page"
        (obj4
           (opt "published_level" int32)
           (opt "slot_index" uint8)
           (opt "page_index" uint16)
           (req "page_payload_strategy" string))
        (function
          | Invalid_dal_page
              {published_level; slot_index; page_index; page_payload_strategy}
            ->
              Some
                ( published_level,
                  slot_index,
                  page_index,
                  page_payload_strategy_to_string page_payload_strategy )
          | _ -> None)
        (fun (published_level, slot_index, page_index, strat) ->
          Invalid_dal_page
            {
              published_level;
              slot_index;
              page_index;
              page_payload_strategy = page_payload_strategy_of_string strat;
            });
    ]

let no_failures = Failure []

let wildcard_or_value f str = match str with "*" -> None | _ -> Some (f str)

let make s =
  let tokens = String.split_on_char ' ' s in
  match tokens with
  | [
   "reveal_dal_parameters";
   number_of_slots;
   attestation_lag;
   slot_size;
   page_size;
  ] ->
      Some
        (Invalid_dal_parameters
           {
             number_of_slots = Int64.of_string number_of_slots;
             attestation_lag = Int64.of_string attestation_lag;
             slot_size = Int64.of_string slot_size;
             page_size = Int64.of_string page_size;
           })
  | ["reveal_dal_page"; published_level; slot_index; page_index; strategy] ->
      Some
        (Invalid_dal_page
           {
             published_level = wildcard_or_value Int32.of_string published_level;
             slot_index = wildcard_or_value int_of_string slot_index;
             page_index = wildcard_or_value int_of_string page_index;
             page_payload_strategy = page_payload_strategy_of_string strategy;
           })
  | _ -> (
      let rec chop = function
        | [] | [""] -> []
        | level :: message_index :: message_tick :: rest ->
            {
              level = int_of_string level;
              message_index = int_of_string message_index;
              message_tick = Int64.of_string message_tick;
            }
            :: chop rest
        | _ -> raise Not_found
      in
      try Some (Failure (chop tokens |> List.sort compare_failure))
      with _ -> None)

let is_failure t ~level ~message_index =
  match t with
  | Invalid_dal_parameters _ | Invalid_dal_page _ -> []
  | Failure failures ->
      List.filter_map
        (fun f ->
          if Compare.Int.(f.level = level && f.message_index = message_index)
          then Some f.message_tick
          else None)
        failures

let is_invalid_dal_parameters = function
  | Failure _ | Invalid_dal_page _ -> None
  | Invalid_dal_parameters parameters -> Some parameters

let is_invalid_dal_page ~published_level ~slot_index ~page_index ~page_size
    ~honest_payload =
  let eq_or_wildcard v = function None -> true | Some w -> v = w in
  function
  | Failure _ | Invalid_dal_parameters _ -> Either.left ()
  | Invalid_dal_page
      {
        published_level = pl;
        slot_index = si;
        page_index = pi;
        page_payload_strategy;
      } ->
      if
        eq_or_wildcard published_level pl
        && eq_or_wildcard slot_index si
        && eq_or_wildcard page_index pi
      then
        Either.right
        @@
        match (page_payload_strategy, honest_payload) with
        | `Flip, Some _ -> None
        | `Flip, None -> Some (Bytes.make page_size 'L')
        | `Alter, None -> None
        | `Alter, Some data ->
            let lll = Bytes.make page_size 'L' in
            let zzz = Bytes.make page_size 'Z' in
            Option.some (if Bytes.equal data lll then zzz else lll)
      else Either.left ()
