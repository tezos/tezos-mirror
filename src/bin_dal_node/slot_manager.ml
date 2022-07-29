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
  | Splitting_failed of string
  | Merging_failed of string
  | Invalid_slot_header of string * string
  | Missing_shards
  | Illformed_shard
  | Slot_not_found

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.split_failed"
    ~title:"Split failed"
    ~description:"Splitting the slot failed"
    ~pp:(fun ppf msg -> Format.fprintf ppf "%s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Splitting_failed parameter -> Some parameter | _ -> None)
    (fun parameter -> Splitting_failed parameter) ;
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
    ~id:"dal.node.invalid_slot_header"
    ~title:"Invalid slot_header"
    ~description:"The slot header is not valid"
    ~pp:(fun ppf (msg, com) -> Format.fprintf ppf "%s : %s" msg com)
    Data_encoding.(obj2 (req "msg" string) (req "com" string))
    (function Invalid_slot_header (msg, com) -> Some (msg, com) | _ -> None)
    (fun (msg, com) -> Invalid_slot_header (msg, com)) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.missing_shards"
    ~title:"Missing shards"
    ~description:"Some shards are missing"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Some shards are missing. Store is invalid.")
    Data_encoding.(unit)
    (function Missing_shards -> Some () | _ -> None)
    (fun () -> Missing_shards) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.slot_not_found"
    ~title:"Slot not found"
    ~description:"Slot not found at this slot header"
    ~pp:(fun ppf () -> Format.fprintf ppf "Slot not found on given slot header")
    Data_encoding.(unit)
    (function Slot_not_found -> Some () | _ -> None)
    (fun () -> Slot_not_found) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.illformed_shard"
    ~title:"Illformed shard"
    ~description:"Illformed shard found in the store"
    ~pp:(fun ppf () -> Format.fprintf ppf "Illformed shard found in the store")
    Data_encoding.(unit)
    (function Illformed_shard -> Some () | _ -> None)
    (fun () -> Illformed_shard)

let wrap_encoding_error =
  Result.map_error (fun e ->
      [Tezos_base.Data_encoding_wrapper.Encoding_error e])

let encode enc v = Data_encoding.Binary.to_string enc v |> wrap_encoding_error

let share_path slot_header shard_id = [slot_header; string_of_int shard_id]

let decode_share s =
  Data_encoding.Binary.of_string Cryptobox.share_encoding s
  |> Result.map_error (fun e ->
         [Tezos_base.Data_encoding_wrapper.Decoding_error e])

let save store slot_header shards =
  let open Lwt_result_syntax in
  let slot_header = Cryptobox.Commitment.to_b58check slot_header in
  Cryptobox.IntMap.iter_es
    (fun i share ->
      let path = share_path slot_header i in
      let*? share = encode Cryptobox.share_encoding share in
      let*! metadata = Store.set ~msg:"Share stored" store path share in
      return metadata)
    shards

let split_and_store cb_constants store slot =
  let r =
    let open Result_syntax in
    let* polynomial = Cryptobox.polynomial_from_slot cb_constants slot in
    let commitment = Cryptobox.commit cb_constants polynomial in
    return (polynomial, commitment)
  in
  let open Lwt_result_syntax in
  match r with
  | Ok (polynomial, commitment) ->
      let shards = Cryptobox.shards_from_polynomial cb_constants polynomial in
      let* () = save store commitment shards in
      let*! () =
        Event.(
          emit stored_slot (Bytes.length slot, Cryptobox.IntMap.cardinal shards))
      in
      Lwt.return_ok commitment
  | Error (`Slot_wrong_size msg) -> Lwt.return_error [Splitting_failed msg]

let get_shard store slot_header shard_id =
  let open Lwt_result_syntax in
  let*? slot_header = encode Cryptobox.slot_header_encoding slot_header in
  let* share =
    Lwt.catch
      (fun () ->
        let*! r = Store.get store (share_path slot_header shard_id) in
        return r)
      (function
        | Invalid_argument _ -> fail [Slot_not_found] | e -> fail [Exn e])
  in
  let*? share = decode_share share in
  return Cryptobox.{index = shard_id; share}

let check_shards shards =
  let open Result_syntax in
  if shards = [] then fail [Slot_not_found]
  else if
    Compare.List_length_with.(shards = Cryptobox.Constants.number_of_shards)
  then Ok ()
  else fail [Missing_shards]

let get_slot cb_constants store slot_header =
  let open Lwt_result_syntax in
  let slot_header = Cryptobox.Commitment.to_b58check slot_header in
  let*! shards = Store.list store [slot_header] in
  let*? () = check_shards shards in
  let* shards =
    List.fold_left_es
      (fun shards (i, tree) ->
        let i = int_of_string i in
        let* share =
          match Store.Tree.destruct tree with
          | `Node _ -> fail [Illformed_shard]
          | `Contents (c, _metadata) ->
              protect @@ fun () ->
              let*! share = Store.Tree.Contents.force_exn c in
              return share
        in
        let*? share = decode_share share in
        return (Cryptobox.IntMap.add i share shards))
      Cryptobox.IntMap.empty
      shards
  in
  let*? polynomial =
    match Cryptobox.polynomial_from_shards cb_constants shards with
    | Ok p -> Ok p
    | Error (`Invert_zero msg | `Not_enough_shards msg) ->
        Error [Merging_failed msg]
  in
  let slot = Cryptobox.polynomial_to_bytes cb_constants polynomial in
  let*! () =
    Event.(
      emit fetched_slot (Bytes.length slot, Cryptobox.IntMap.cardinal shards))
  in
  return slot

(* FIXME https://gitlab.com/tezos/tezos/-/issues/3405

   This can work only if a slot never ends with a `\000`. But I am not
   sure in general such thing is required. *)
module Utils = struct
  let trim_x00 b =
    let len = ref 0 in
    let () =
      try
        (* Counts the number of \000 from the end of the bytes *)
        for i = Bytes.length b - 1 downto 0 do
          if Bytes.get b i = '\000' then incr len else raise Exit
        done
      with Exit -> ()
    in
    Bytes.sub b 0 (Bytes.length b - !len)

  let fill_x00 b =
    let len = Bytes.length b in
    Bytes.extend b 0 (Cryptobox.Constants.slot_size - len)
end
