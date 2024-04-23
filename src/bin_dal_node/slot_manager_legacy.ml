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

type error +=
  | Merging_failed of string
  | Missing_shards of {provided : int; required : int}
  | Illformed_pages
  | Invalid_number_of_needed_shards of int

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.merge_failed"
    ~title:"Merge failed"
    ~description:"Merging the slot failed"
    ~pp:(fun ppf msg -> Format.fprintf ppf "%s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Merging_failed parameter -> Some parameter | _ -> None)
    (fun parameter -> Merging_failed parameter) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.missing_shards"
    ~title:"Missing shards"
    ~description:"Some shards are missing"
    ~pp:(fun ppf (provided, required) ->
      Format.fprintf
        ppf
        "Some shards are missing, expected at least %d, found %d. Store is \
         invalid."
        provided
        required)
    Data_encoding.(obj2 (req "provided" int31) (req "required" int31))
    (function
      | Missing_shards {provided; required} -> Some (provided, required)
      | _ -> None)
    (fun (provided, required) -> Missing_shards {provided; required}) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.illformed_pages"
    ~title:"Illformed pages"
    ~description:"Illformed pages found in the store"
    ~pp:(fun ppf () -> Format.fprintf ppf "Illformed pages found in the store")
    Data_encoding.unit
    (function Illformed_pages -> Some () | _ -> None)
    (fun () -> Illformed_pages)

type slot = bytes

let polynomial_from_shards cryptobox shards =
  match Cryptobox.polynomial_from_shards cryptobox shards with
  | Ok p -> Ok p
  | Error
      ( `Not_enough_shards msg
      | `Shard_index_out_of_range msg
      | `Invalid_shard_length msg ) ->
      Error [Merging_failed msg]

let polynomial_from_shards_lwt cryptobox shards ~number_of_needed_shards =
  let open Lwt_result_syntax in
  let*? shards =
    Seq_s.take
      ~when_negative_length:
        [Invalid_number_of_needed_shards number_of_needed_shards]
      number_of_needed_shards
      shards
  in
  (* Note: this [seq_of_seq_s] function consumes the input sequence
     and resolves all its promises. It's OK here because we capped the
     size of the input to number_of_needed_shards which is reasonably
     small. *)
  let rec seq_of_seq_s (s : 'a Seq_s.t) : 'a Seq.t Lwt.t =
    let open Lwt_syntax in
    let* n = s () in
    match n with
    | Nil -> return (fun () -> Seq.Nil)
    | Cons (x, s) ->
        let* s = seq_of_seq_s s in
        return (fun () -> Seq.Cons (x, s))
  in
  let*! shards = seq_of_seq_s shards in
  let*? polynomial = polynomial_from_shards cryptobox shards in
  return polynomial

let get_slot cryptobox store commitment =
  let open Lwt_result_syntax in
  let {Cryptobox.number_of_shards; redundancy_factor; _} =
    Cryptobox.parameters cryptobox
  in
  (* First attempt to get the slot from the slot store. *)
  let*! res =
    Store.Slots.find_slot_by_commitment store.Store.slots cryptobox commitment
  in
  match res with
  | Ok (Some slot) -> return slot
  | Ok None | Error (`Decoding_failed _) ->
      (* The slot could not be obtained from the slot store, attempt a
         reconstruction. *)
      let minimal_number_of_shards = number_of_shards / redundancy_factor in
      let rec loop acc shard_id remaining =
        if remaining <= 0 then return acc
        else if shard_id >= number_of_shards then
          let provided = minimal_number_of_shards - remaining in
          tzfail
          @@ Missing_shards {provided; required = minimal_number_of_shards}
        else
          let*! res =
            Store.Shards.read store.Store.shards commitment shard_id
          in
          match res with
          | Ok res -> loop (Seq.cons res acc) (shard_id + 1) (remaining - 1)
          | Error _ -> loop acc (shard_id + 1) remaining
      in
      let* shards = loop Seq.empty 0 minimal_number_of_shards in
      let*? polynomial = polynomial_from_shards cryptobox shards in
      let slot = Cryptobox.polynomial_to_slot cryptobox polynomial in
      (* Store the slot so that next calls don't require a reconstruction. *)
      let* () =
        Store.Slots.add_slot_by_commitment
          store.Store.slots
          cryptobox
          slot
          commitment
        |> Errors.to_tzresult
      in
      let*! () =
        Event.(emit fetched_slot (Bytes.length slot, Seq.length shards))
      in
      return slot

let get_slot_pages cryptobox store commitment =
  let open Lwt_result_syntax in
  let dal_parameters = Cryptobox.parameters cryptobox in
  let* slot = get_slot cryptobox store commitment in
  (* The slot size `Bytes.length slot` should be an exact multiple of `page_size`.
     If this is not the case, we throw an `Illformed_pages` error.
  *)
  (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3900
     Implement `Bytes.chunk_bytes` which returns a list of bytes directly. *)
  let*? pages =
    String.chunk_bytes
      dal_parameters.page_size
      slot
      ~error_on_partial_chunk:(TzTrace.make Illformed_pages)
  in
  return @@ List.map (fun page -> String.to_bytes page) pages
