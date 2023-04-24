(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = {expected_env : env_version; components : component list}

and component = {
  name : string;
  interface : string option;
  implementation : string;
}

and env_version = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10

val component_encoding : component Data_encoding.t

(** [compare_version va vb] is negative if [va] is a less recent version than
    [vb], positive if [va] is a more recent version than [vb], zero if they are
    the same version.

    In less precise but more intuitive terms,
    [compare_version va vb <op> 0] is the same truthness as [va <op> vb]
    where [<op>] is any comparison operator.

    E.g., [compare_version V0 V1 < 0] is [true]. *)
val compare_version : env_version -> env_version -> int

val env_version_encoding : env_version Data_encoding.t

val pp_ocaml : Format.formatter -> t -> unit

include
  S.HASHABLE
    with type t := t
     and type hash := Tezos_crypto.Hashed.Protocol_hash.t

val of_bytes_exn : Bytes.t -> t

val of_string_exn : String.t -> t

val of_string : String.t -> t option

val bounded_encoding : ?max_size:int -> unit -> t Data_encoding.t

val module_name_of_env_version : env_version -> string

module Meta : sig
  type t = {
    hash : Tezos_crypto.Hashed.Protocol_hash.t option;
    expected_env_version : env_version option;
    modules : string list;
  }

  val encoding : t Data_encoding.t
end
