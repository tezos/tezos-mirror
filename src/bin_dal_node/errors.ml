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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4622

   Move errors from different DAL modules to this file.
*)

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.decoding_failed"
    ~title:"Decoding failed"
    ~description:"Failure while decoding a value"
    ~pp:(fun ppf data_kind ->
      Format.fprintf
        ppf
        "Error while decoding a %s value"
        (Types.Store.to_string data_kind))
    Data_encoding.(obj1 (req "data_kind" Types.Store.encoding))
    (function Decoding_failed data_kind -> Some data_kind | _ -> None)
    (fun data_kind -> Decoding_failed data_kind) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.profile_incompatibility"
    ~title:"Profile incompatibility"
    ~description:
      "Adding profiles to a node configured with the bootstrap profile is not \
       allowed. This is because bootstrap nodes are incompatible with other \
       profiles."
    Data_encoding.empty
    (function Profile_incompatibility -> Some () | _ -> None)
    (fun () -> Profile_incompatibility) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.invalid_slot_index"
    ~title:"Invalid slot index"
    ~description:"Invalid slot index provided for the producer profile"
    ~pp:(fun ppf (slot_index, number_of_slots) ->
      Format.fprintf
        ppf
        "The slot index (%d) should be smaller than the number of slots (%d)"
        slot_index
        number_of_slots)
    Data_encoding.(obj2 (req "slot_index" int16) (req "number_of_slots" int16))
    (function
      | Invalid_slot_index {slot_index; number_of_slots} ->
          Some (slot_index, number_of_slots)
      | _ -> None)
    (fun (slot_index, number_of_slots) ->
      Invalid_slot_index {slot_index; number_of_slots}) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.cryptobox.initialisation_failed"
    ~title:"Cryptobox initialisation failed"
    ~description:"Unable to initialise the cryptobox parameters"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Unable to initialise the cryptobox parameters. Reason: %s"
        msg)
    Data_encoding.(obj1 (req "error" string))
    (function Cryptobox_initialisation_failed str -> Some str | _ -> None)
    (fun str -> Cryptobox_initialisation_failed str) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.not_enough_history"
    ~title:"Not enough history"
    ~description:"The node does not store sufficiently many levels"
    ~pp:(fun ppf (stored_levels, minimal_levels) ->
      Format.fprintf
        ppf
        "The node's history mode specifies that data for %d levels should be \
         stored, but the minimum required is %d levels."
        stored_levels
        minimal_levels)
    Data_encoding.(
      obj2 (req "stored_levels" int31) (req "minimal_levels" int31))
    (function
      | Not_enough_history {stored_levels; minimal_levels} ->
          Some (stored_levels, minimal_levels)
      | _ -> None)
    (fun (stored_levels, minimal_levels) ->
      Not_enough_history {stored_levels; minimal_levels}) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.not_enough_l1_history"
    ~title:"Not enough L1 history"
    ~description:"The L1 node does not store sufficiently many cycles"
    ~pp:(fun ppf (stored_cycles, minimal_cycles) ->
      Format.fprintf
        ppf
        "The L1 node's history mode stores block data for %d cycles, but the \
         minimum required by the DAL node is %d cycles. Increase the number of \
         cycles the L1 node stores using the CLI argument `--history-mode \
         rolling:%d`."
        stored_cycles
        minimal_cycles
        minimal_cycles)
    Data_encoding.(
      obj2 (req "stored_cycles" int31) (req "minimal_cycles" int31))
    (function
      | Not_enough_l1_history {stored_cycles; minimal_cycles} ->
          Some (stored_cycles, minimal_cycles)
      | _ -> None)
    (fun (stored_cycles, minimal_cycles) ->
      Not_enough_l1_history {stored_cycles; minimal_cycles}) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.amplificator_initialization_failed"
    ~title:"Amplificator initialization failed"
    ~description:"Amplificator initialization failed"
    Data_encoding.empty
    (function Amplificator_initialization_failed -> Some () | _ -> None)
    (fun () -> Amplificator_initialization_failed)

(** This part defines and handles more elaborate errors for the DAL node. *)

(* Specialized errors defined as polymorphic variants. *)

type not_found = [`Not_found]

type other = [`Other of tztrace]

let not_found = `Not_found

(* Helpers to wrap values and tzresult errors in [`Other]. *)

let decoding_failed kind trace = `Other (Decoding_failed kind :: trace)

let other v = `Other v

let other_result v =
  let open Result_syntax in
  match v with Ok v -> return v | Error l -> fail (`Other l)

let other_lwt_result v =
  let open Lwt_syntax in
  let* v in
  return @@ other_result v

(* Helpers to cast the errors into tzresult monad. *)

let error_to_tzresult e =
  let open Lwt_result_syntax in
  match e with `Other e -> fail e

let to_option_tzresult r =
  let open Lwt_result_syntax in
  let*! r in
  match r with
  | Ok s -> return_some s
  | Error `Not_found -> return_none
  | Error (`Other _ as err) -> error_to_tzresult err

let to_tzresult r =
  let open Lwt_result_syntax in
  let*! r in
  match r with Ok s -> return s | Error e -> error_to_tzresult e
