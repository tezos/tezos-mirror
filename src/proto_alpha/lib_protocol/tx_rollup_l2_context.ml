(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

open Tx_rollup_l2_storage_sig
open Tx_rollup_l2_context_sig

let metadata_encoding =
  Data_encoding.(
    conv
      (fun {counter; public_key} -> (counter, public_key))
      (fun (counter, public_key) -> {counter; public_key})
      (obj2 (req "counter" int64) (req "public_key" Bls.Public_key.encoding)))

(** {1 Type-Safe Storage Access and Gas Accounting} *)

(** A value of type ['a key] identifies a value of type ['a] in an
    underlying, untyped storage.

    This GADT is used to enforce type-safety of the abstraction of
    the transactions rollup context. For this abstraction to work,
    it is necessary to ensure that the serialization of values ['a
    key] and ['b key] cannot collide. To that end, we use
    [Data_encoding] (see {!packed_key_encoding}). *)
type _ key =
  | Address_metadata : address_index -> metadata key
  | Address_count : int32 key
  | Address_index : Tx_rollup_l2_address.t -> address_index key
  | Ticket_count : int32 key
  | Ticket_index : Alpha_context.Ticket_hash.t -> ticket_index key
  | Ticket_ledger : ticket_index * address_index -> Tx_rollup_l2_qty.t key

(** A monomorphic version of {!Key}, used for serialization purposes. *)
type packed_key = Key : 'a key -> packed_key

(** The encoding used to serialize keys to be used with an untyped storage. *)
let packed_key_encoding : packed_key Data_encoding.t =
  Data_encoding.(
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"Address_metadata"
          Tx_rollup_l2_address.Indexable.index_encoding
          (function Key (Address_metadata idx) -> Some idx | _ -> None)
          (fun idx -> Key (Address_metadata idx));
        case
          (Tag 1)
          ~title:"Address_count"
          empty
          (function Key Address_count -> Some () | _ -> None)
          (fun () -> Key Address_count);
        case
          (Tag 2)
          ~title:"Address_index"
          Tx_rollup_l2_address.encoding
          (function Key (Address_index addr) -> Some addr | _ -> None)
          (fun addr -> Key (Address_index addr));
        case
          (Tag 3)
          ~title:"Ticket_count"
          empty
          (function Key Ticket_count -> Some () | _ -> None)
          (fun () -> Key Ticket_count);
        case
          (Tag 4)
          ~title:"Ticket_index"
          Alpha_context.Ticket_hash.encoding
          (function Key (Ticket_index ticket) -> Some ticket | _ -> None)
          (fun ticket -> Key (Ticket_index ticket));
        case
          (Tag 5)
          ~title:"Ticket_ledger"
          (tup2
             Ticket_indexable.index_encoding
             Tx_rollup_l2_address.Indexable.index_encoding)
          (function
            | Key (Ticket_ledger (ticket, address)) -> Some (ticket, address)
            | _ -> None)
          (fun (ticket, address) -> Key (Ticket_ledger (ticket, address)));
      ])

(** [value_encoding key] returns the encoding to be used to serialize
    and deserialize values associated to a [key] from and to the
    underlying storage. *)
let value_encoding : type a. a key -> a Data_encoding.t =
  let open Data_encoding in
  function
  | Address_metadata _ -> metadata_encoding
  | Address_count -> int32
  | Address_index _ -> Tx_rollup_l2_address.Indexable.index_encoding
  | Ticket_count -> int32
  | Ticket_index _ -> Ticket_indexable.index_encoding
  | Ticket_ledger _ -> Tx_rollup_l2_qty.encoding

(** {1 Errors} *)

type error += Key_cannot_be_serialized

type error += Value_cannot_be_serialized

type error += Value_cannot_be_deserialized

let () =
  let open Data_encoding in
  (* Key cannot be serialized *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_key_cannot_be_serialized"
    ~title:"Key cannot be serialized"
    ~description:"Tried to serialize an invalid key."
    empty
    (function Key_cannot_be_serialized -> Some () | _ -> None)
    (fun () -> Key_cannot_be_serialized) ;
  (* Value cannot be serialized *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_value_cannot_be_serialized"
    ~title:"Value cannot be serialized"
    ~description:"Tried to serialize an invalid value."
    empty
    (function Value_cannot_be_serialized -> Some () | _ -> None)
    (fun () -> Value_cannot_be_serialized) ;
  (* Value cannot be deserialized *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_value_cannot_be_deserialized"
    ~title:"Value cannot be deserialized"
    ~description:
      "A value has been serialized in the Tx_rollup store, but cannot be \
       deserialized."
    empty
    (function Value_cannot_be_deserialized -> Some () | _ -> None)
    (fun () -> Value_cannot_be_deserialized)

(** {1 The Context Functor} *)

module Make (S : STORAGE) : CONTEXT with type t = S.t and type 'a m = 'a S.m =
struct
  type t = S.t

  type 'a m = 'a S.m

  module Syntax = struct
    include S.Syntax

    let ( let*? ) res f =
      match res with Result.Ok v -> f v | Result.Error error -> fail error

    let fail_unless cond error =
      let open S.Syntax in
      if cond then return () else fail error

    let fail_when cond error =
      let open S.Syntax in
      if cond then fail error else return ()
  end

  let bls_verify : (Bls.Public_key.t * bytes) list -> signature -> bool m =
   fun accounts aggregated_signature ->
    let open Syntax in
    let msgs = List.map (fun (pk, msg) -> (pk, None, msg)) accounts in
    return (Bls.aggregate_check msgs aggregated_signature)

  let unwrap_or : type a. a option -> error -> a S.m =
   fun opt err ->
    match opt with Some x -> S.Syntax.return x | None -> S.Syntax.fail err

  let serialize_key : type a. a key -> bytes m =
   fun key ->
    unwrap_or
      (Data_encoding.Binary.to_bytes_opt packed_key_encoding (Key key))
      Key_cannot_be_serialized

  let serialize_value : type a. a Data_encoding.t -> a -> bytes m =
   fun encoding value ->
    unwrap_or
      (Data_encoding.Binary.to_bytes_opt encoding value)
      Value_cannot_be_serialized

  let deserialize_value : type a. a Data_encoding.t -> bytes -> a m =
   fun encoding value ->
    unwrap_or
      (Data_encoding.Binary.of_bytes_opt encoding value)
      Value_cannot_be_deserialized

  (** [get ctxt key] is a type-safe [get] function. *)
  let get : type a. t -> a key -> a option m =
   fun ctxt key ->
    let open Syntax in
    let value_encoding = value_encoding key in
    let* key = serialize_key key in
    let* value = S.get ctxt key in
    match value with
    | Some value ->
        let* value = deserialize_value value_encoding value in
        return (Some value)
    | None -> return None

  (** [set ctxt key value] is a type-safe [set] function. *)
  let set : type a. t -> a key -> a -> t m =
   fun ctxt key value ->
    let open Syntax in
    let value_encoding = value_encoding key in
    let* key = serialize_key key in
    let* value = serialize_value value_encoding value in
    S.set ctxt key value

  let remove : type a. t -> a key -> t m =
   fun ctxt key ->
    let open Syntax in
    let* key = serialize_key key in
    S.remove ctxt key

  module Address_metadata = struct
    let get ctxt idx = get ctxt (Address_metadata idx)

    let incr_counter ctxt idx =
      let open Syntax in
      let* metadata = get ctxt idx in
      match metadata with
      | Some meta ->
          let new_counter = Int64.succ meta.counter in
          let* () =
            fail_unless
              Compare.Int64.(new_counter >= meta.counter)
              Counter_overflow
          in
          set ctxt (Address_metadata idx) {meta with counter = new_counter}
      | None -> fail (Unknown_address_index idx)

    let init_with_public_key ctxt idx public_key =
      let open Syntax in
      let* metadata = get ctxt idx in
      match metadata with
      | None -> set ctxt (Address_metadata idx) {counter = 0L; public_key}
      | Some _ -> fail (Metadata_already_initialized idx)

    module Internal_for_tests = struct
      let set ctxt idx metadata = set ctxt (Address_metadata idx) metadata
    end
  end

  module Address_index = struct
    let count ctxt =
      let open Syntax in
      let+ count = get ctxt Address_count in
      Option.value ~default:0l count

    let init_counter ctxt = set ctxt Address_count 0l

    let associate_index ctxt addr =
      let open Syntax in
      let* i = count ctxt in
      let new_count = Int32.succ i in
      let* () =
        fail_unless Compare.Int32.(new_count >= i) Too_many_l2_addresses
      in
      (* This can not fail as by construction [count ctxt] is always positive. *)
      let idx = Indexable.index_exn i in
      let* ctxt = set ctxt (Address_index addr) idx in
      let+ ctxt = set ctxt Address_count new_count in
      (ctxt, idx)

    let get ctxt addr = get ctxt (Address_index addr)

    let get_or_associate_index ctxt addr =
      let open Syntax in
      let* index_opt = get ctxt addr in
      match index_opt with
      | Some idx -> return (ctxt, `Existed, idx)
      | None ->
          let+ ctxt, idx = associate_index ctxt addr in
          (ctxt, `Created, idx)

    module Internal_for_tests = struct
      let set_count ctxt count = set ctxt Address_count count
    end
  end

  module Ticket_index = struct
    let count ctxt =
      let open Syntax in
      let+ count = get ctxt Ticket_count in
      Option.value ~default:0l count

    let init_counter ctxt = set ctxt Ticket_count 0l

    let associate_index ctxt ticket =
      let open Syntax in
      let* i = count ctxt in
      let new_count = Int32.succ i in
      let* () =
        fail_unless Compare.Int32.(new_count >= i) Too_many_l2_tickets
      in
      (* This can not fail as by construction [count ctxt] is always positive. *)
      let idx = Indexable.index_exn i in
      let* ctxt = set ctxt (Ticket_index ticket) idx in
      let+ ctxt = set ctxt Ticket_count new_count in
      (ctxt, idx)

    let get ctxt ticket = get ctxt (Ticket_index ticket)

    let get_or_associate_index ctxt ticket =
      let open Syntax in
      let* index_opt = get ctxt ticket in
      match index_opt with
      | Some idx -> return (ctxt, `Existed, idx)
      | None ->
          let+ ctxt, idx = associate_index ctxt ticket in
          (ctxt, `Created, idx)

    module Internal_for_tests = struct
      let set_count ctxt count = set ctxt Ticket_count count
    end
  end

  module Ticket_ledger = struct
    let get_opt ctxt tidx aidx = get ctxt (Ticket_ledger (tidx, aidx))

    let get ctxt tidx aidx =
      let open Syntax in
      let+ res = get_opt ctxt tidx aidx in
      Option.value ~default:Tx_rollup_l2_qty.zero res

    let set ctxt tidx aidx = set ctxt (Ticket_ledger (tidx, aidx))

    let remove ctxt tidx aidx = remove ctxt (Ticket_ledger (tidx, aidx))

    let spend ctxt tidx aidx qty =
      let open Syntax in
      let* src_balance = get ctxt tidx aidx in
      match Tx_rollup_l2_qty.sub src_balance qty with
      | None -> fail Balance_too_low
      | Some remainder when Tx_rollup_l2_qty.(remainder > zero) ->
          set ctxt tidx aidx remainder
      | Some _ -> remove ctxt tidx aidx

    let credit ctxt tidx aidx qty =
      let open Syntax in
      let* balance = get ctxt tidx aidx in
      match Tx_rollup_l2_qty.add balance qty with
      | None -> fail Balance_overflow
      | Some new_balance -> set ctxt tidx aidx new_balance

    module Internal_for_tests = struct
      let get_opt = get_opt
    end
  end
end
