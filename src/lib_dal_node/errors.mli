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

(** Extension of the open type [error] with the errors that could be raised by
    the DAL node. *)
type error +=
  | Decoding_failed of Types.Store.kind
  | Profile_incompatibility
  | Invalid_slot_index of {slot_index : int; number_of_slots : int}
  | Cryptobox_initialisation_failed of string
  | Not_enough_history of {stored_levels : int; minimal_levels : int}
  | Not_enough_l1_history of {stored_cycles : int; minimal_cycles : int}
  | Amplificator_initialization_failed
  | Wrong_chain_id of {
      current_chain_id : Chain_id.t;
      stored_chain_id : Chain_id.t;
    }
  | Unexpected_slot_status_transition of {
      slot_id : Types.Slot_id.t;
      from_status_opt : Types.header_status option;
      to_status : Types.header_status;
    }
  | Missing_configuration_file of {file : string}

(** The errors below are used to extend tzresult/tztrace monad/errors with Some
    specific errors on which we'd like to match in the DAL node's code. *)

(** We would like to match [`Not_found] as we would want to return 404 HTTP code
    to clients. *)
type not_found = [`Not_found]

(** We will use [`Other] to wrap other {!tztrace} errors in the new
    polymorphic-variants based monad. *)
type other = [`Other of tztrace]

(** [not_found] is an alias for [`Not_found]. *)
val not_found : [> not_found]

(** [decoding_failed kind trace] produces the error trace
    [Decoding_failed kind :: trace] and wraps it with [`Other]. *)
val decoding_failed : Types.Store.kind -> tztrace -> [> other]

(** [other l] wraps the give tztrace [l] in [`Other]. *)
val other : tztrace -> [> other]

(** [other_result r] casts the given value [r] from the ['a tzresult] monad into
    [('a, [> other_error]) result]. *)
val other_result : 'a tzresult -> ('a, [> other]) result

(** [other_lwt_result r] casts the given value [r] from the ['a tzresult Lwt.t]
    monad into [('a, [> other]) result Lwt.t]. *)
val other_lwt_result : 'a tzresult Lwt.t -> ('a, [> other]) result Lwt.t

(** [to_option_tzresult v] transforms the given value [v] to another value in
    the regular ['a option tzresult Lwt.t] monad.

    Asuming the Lwt monad [v] successfully resolves, then:

    - If it yields a value [Ok w], the function returns an Lwt monad whose
    payload is [Ok (Some w)].

    - If it yields a value [Error `Not_found], the function returns an Lwt monad
    whose payload is [Ok (None)] so that {!Tezos_rpc_http} returns 404 HTTP code.

    - Otherwise, it yields a value [Error `Other u]. In this case, the function
    returns an Lwt monad whose payload is [Error u]. *)
val to_option_tzresult :
  ('a, [< not_found | other]) result Lwt.t -> 'a option tzresult Lwt.t

(** [to_tzresult v] is quite similar to {!to_option_tzresult}. Except that the
    [`Not_found] case cannot happen, in which case the use of [option] for the
    non-failing case is not needed. *)
val to_tzresult : ('a, [< other]) result Lwt.t -> 'a tzresult Lwt.t
