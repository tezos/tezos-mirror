(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** This module provides the operation representation used by the
    prevalidator and its dependencies. It also contains tools for
    parsing an operation into this representation, and updating the
    latter. *)

(** Representation of a parsed operation, used only in the shell. *)
type 'protocol_operation operation = private {
  hash : Operation_hash.t;  (** Hash of an operation. *)
  raw : Operation.t;
      (** Raw representation of an operation (from the point view of the
          shell). *)
  protocol : 'protocol_operation;
      (** Economic protocol specific data of an operation. It is the
          unserialized representation of [raw.protocol_data]. For
          convenience, the type associated to this type may be [unit] if we
          do not have deserialized the operation yet. *)
  signature_checked : bool;
      (** This field is initially [false]. It is set to [true] when the
          operation is successfully validated in any context. While this does
          not guarantee that the operation will still be valid in another
          validation context, it notably means that the signature is
          correct. Therefore, when this field is [true], we can tell the
          protocol to skip signature checks. *)
}

(** Return the operation with the {!signature_checked} field set to [true]. *)
val record_successful_signature_check :
  'protocol_operation operation -> 'protocol_operation operation

(** The purpose of this module type is to provide the [parse]
    function, whose return type depends on the protocol. *)
module type PARSER = sig
  (** Similar to the same type in the protocol,
      see {!Tezos_protocol_environment.PROTOCOL.operation} *)
  type protocol_operation

  (** [parse hash op] reads a usual {!Operation.t} and lifts it to the
      type {!protocol_operation} used by this module. This function is in the
      {!tzresult} monad, because it can return the following errors:

      - {!Validation_errors.Oversized_operation} if the size of the operation
        data within [op] is too large (to protect against DoS attacks), and
      - {!Validation_errors.Parse_error} if serialized data cannot be parsed. *)
  val parse :
    Operation_hash.t -> Operation.t -> protocol_operation operation tzresult
end

(** Create a {!PARSER} tailored to a given protocol. *)
module MakeParser : functor (Proto : Tezos_protocol_environment.PROTOCOL) ->
  PARSER with type protocol_operation = Proto.operation

(**/**)

module Internal_for_tests : sig
  (** Returns the {!Operation.t} underlying an {!operation} *)
  val to_raw : _ operation -> Operation.t

  (** The hash of an {!operation} *)
  val hash_of : _ operation -> Operation_hash.t

  (** A constructor for the [operation] datatype. It by-passes the
      checks done by the [parse] function. *)
  val make_operation : Operation.t -> Operation_hash.t -> 'a -> 'a operation

  (** [safe_binary_of_bytes encoding bytes] parses [bytes] using [encoding].
      Any error happening during parsing becomes {!Parse_error}.

      If one day the functor's signature is simplified, tests could use
      [parse_unsafe] directly rather than relying on this function to
      replace [Proto.operation_data_encoding]. *)
  val safe_binary_of_bytes : 'a Data_encoding.t -> bytes -> 'a tzresult
end
