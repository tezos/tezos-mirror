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

module Slot_index : sig
  type t = int

  (** Encoding on 1 byte, matches
      [Protocol.Alpha_context.Dal.Slot_index.encoding]. *)
  val encoding : t Data_encoding.t
end

module Page_index : sig
  type t = int

  (** Encoding on 2 bytes, matches
      [Protocol.Alpha_context.Dal.Slot.Page.Index.encoding]. *)
  val encoding : t Data_encoding.t
end

(** A slot commitment, same as protocol slot commitments through environment. *)
module Commitment : sig
  type t = Tezos_crypto_dal.Cryptobox.Verifier.commitment

  val encoding : t Data_encoding.t
end

module Slot_header : sig
  (** A slot header. *)
  module V1 : sig
    type id = {published_level : int32; index : Slot_index.t}

    type t = {id : id; commitment : Commitment.t}

    (** Encoding for [V1] matches
      [Protocol.Alpha_context.Dal.Slot.Header.encoding]. *)
    val encoding : t Data_encoding.t
  end

  include Versioned_data.S with type t = V1.t

  include module type of V1 with type id = V1.id and type t = V1.t
end

module Slot_history : sig
  (** V1 are serialized slot histories as they appear first in Mumbai.
      Introduce V2 if encoding in future protocol changes. *)
  module V1 : sig
    type t

    val encoding : t Data_encoding.t
  end

  include Versioned_data.S with type t = V1.t

  include module type of V1 with type t = V1.t
end

module Slot_history_cache : sig
  (** V1 are serialized slot history caches as they appear first in
      Mumbai. Introduce V2 if encoding in future protocol changes. *)
  module V1 : sig
    type t

    val encoding : t Data_encoding.t
  end

  include Versioned_data.S with type t = V1.t

  include module type of V1 with type t = V1.t
end
