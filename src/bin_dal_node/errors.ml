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

(** Extention of the open type [error] with the errors that could be raised by
    the DAL node. *)
type error +=
  | Decoding_failed of Types.Store.kind
  | Profile_incompatibility
  | Invalid_slot_index of {slot_index : int; number_of_slots : int}

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
      Invalid_slot_index {slot_index; number_of_slots})

(** This part defines and handles more elaborate errors for the DAL node. *)

(* Specialized errors defined as polymorphic variants. *)

type decoding = [`Decoding_failed of Types.Store.kind * tztrace]

type not_found = [`Not_found]

type other = [`Other of tztrace]

(* Helpers to wrap values and tzresult errors in [`Other]. *)

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
  match e with
  | `Decoding_failed (kind, tztrace) ->
      let*! () = Event.(emit decoding_data_failed kind) in
      fail (Decoding_failed kind :: tztrace)
  | `Other e -> fail e
  | `Not_found ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4622

         Move to a defined Not_found error in #4622?
         Currently, using something like
         [tzfail @@ Shard_store.Resource_not_found ""] is
         not well suited as [Resource_not_found]'s arg is understood as
         a path.
      *)
      failwith "Not_found"

let to_option_tzresult ?(none = fun _e -> false) r =
  let open Lwt_result_syntax in
  let*! r in
  match r with
  | Ok s -> return_some s
  | Error err -> if none err then return_none else error_to_tzresult err

let to_tzresult r =
  let open Lwt_result_syntax in
  let*! r in
  match r with Ok s -> return s | Error e -> error_to_tzresult e
