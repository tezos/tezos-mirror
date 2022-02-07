(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
  | Tx_rollup_inbox_does_not_exist of Tx_rollup_repr.t * Raw_level_repr.t
  | Tx_rollup_inbox_size_would_exceed_limit of Tx_rollup_repr.t
  | Tx_rollup_message_size_exceeds_limit

(** [prepare_metadata ctxt rollup state level] prepares the metadata for
    an inbox at [level]. This may involve updating the predecessor's
    successor pointer.  It also returns a new state which point
    the tail of the linked list of inboxes to this inbox. *)
let prepare_metadata :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_state_repr.t ->
    Raw_level_repr.t ->
    (Raw_context.t * Tx_rollup_state_repr.t * Tx_rollup_inbox_repr.metadata)
    tzresult
    Lwt.t =
 fun ctxt rollup state level ->
  Storage.Tx_rollup.Inbox_metadata.find (ctxt, level) rollup
  >>=? fun (ctxt, metadata) ->
  match metadata with
  | Some metadata -> return (ctxt, state, metadata)
  | None ->
      (* First message in inbox: need to update linked list and pending
         inbox count *)
      let predecessor = Tx_rollup_state_repr.last_inbox_level state in
      let new_state = Tx_rollup_state_repr.append_inbox state level in
      (match predecessor with
      | None -> return ctxt
      | Some predecessor_level ->
          Storage.Tx_rollup.Inbox_metadata.get (ctxt, predecessor_level) rollup
          >>=? fun (ctxt, predecessor_metadata) ->
          (* Here, we update the predecessor inbox's successor to point
             to this inbox. *)
          Storage.Tx_rollup.Inbox_metadata.add
            (ctxt, predecessor_level)
            rollup
            {predecessor_metadata with successor = Some level}
          >|=? fun (ctxt, _, _) -> ctxt)
      >>=? fun ctxt ->
      let new_metadata : Tx_rollup_inbox_repr.metadata =
        {cumulated_size = 0; predecessor; successor = None}
      in
      return (ctxt, new_state, new_metadata)

let append_message :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_state_repr.t ->
    Tx_rollup_message_repr.t ->
    (Raw_context.t * Tx_rollup_state_repr.t) tzresult Lwt.t =
 fun ctxt rollup state message ->
  let level = (Raw_context.current_level ctxt).level in
  let message_size = Tx_rollup_message_repr.size message in
  let message_limit =
    Constants_storage.tx_rollup_hard_size_limit_per_message ctxt
  in
  fail_unless
    Compare.Int.(message_size < message_limit)
    Tx_rollup_message_size_exceeds_limit
  >>=? fun () ->
  prepare_metadata ctxt rollup state level
  >>=? fun (ctxt, new_state, new_metadata) ->
  let new_metadata =
    {
      new_metadata with
      cumulated_size = message_size + new_metadata.cumulated_size;
    }
  in
  Storage.Tx_rollup.Inbox_metadata.add (ctxt, level) rollup new_metadata
  >>=? fun (ctxt, _, _) ->
  let new_size = new_metadata.cumulated_size in
  let inbox_limit =
    Constants_storage.tx_rollup_hard_size_limit_per_inbox ctxt
  in
  fail_unless
    Compare.Int.(new_size < inbox_limit)
    (Tx_rollup_inbox_size_would_exceed_limit rollup)
  >>=? fun () ->
  Storage.Tx_rollup.Inbox_rev_contents.find (ctxt, level) rollup
  >>=? fun (ctxt, mcontents) ->
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2408
     Carbonate hashing the message. *)
  Storage.Tx_rollup.Inbox_rev_contents.add
    (ctxt, level)
    rollup
    (Tx_rollup_message_repr.hash message :: Option.value ~default:[] mcontents)
  >>=? fun (ctxt, _, _) -> return (ctxt, new_state)

let get_level :
    Raw_context.t -> [`Current | `Level of Raw_level_repr.t] -> Raw_level_repr.t
    =
 fun ctxt -> function
  | `Current -> (Raw_context.current_level ctxt).level
  | `Level lvl -> lvl

let messages_opt :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_message_repr.hash list option) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  let level = get_level ctxt level in
  Storage.Tx_rollup.Inbox_rev_contents.find (ctxt, level) tx_rollup
  >>=? function
  | (ctxt, Some rev_contents) -> return (ctxt, Some (List.rev rev_contents))
  | (ctxt, None) ->
      (*
        Prior to returning [None], we check whether or not the
        transaction rollup address is valid, to raise the appropriate
        if need be.
       *)
      Tx_rollup_state_storage.assert_exist ctxt tx_rollup >>=? fun ctxt ->
      return (ctxt, None)

let messages :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_message_repr.hash list) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  messages_opt ctxt ~level tx_rollup >>=? function
  | (ctxt, Some messages) -> return (ctxt, messages)
  | (_, None) ->
      fail (Tx_rollup_inbox_does_not_exist (tx_rollup, get_level ctxt level))

let size :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * int) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  let level = get_level ctxt level in
  Storage.Tx_rollup.Inbox_metadata.find (ctxt, level) tx_rollup >>=? function
  | (ctxt, Some {cumulated_size; _}) -> return (ctxt, cumulated_size)
  | (ctxt, None) ->
      (*
        Prior to raising an error related to the missing inbox, we
        check whether or not the transaction rollup address is valid,
        to raise the appropriate if need be.
       *)
      Tx_rollup_state_storage.assert_exist ctxt tx_rollup >>=? fun _ctxt ->
      fail (Tx_rollup_inbox_does_not_exist (tx_rollup, level))

let find :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_inbox_repr.t option) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  let open Tx_rollup_inbox_repr in
  (*
    [messages_opt] checks whether or not [tx_rollup] is valid, so
    we do not have to do it here.
   *)
  messages_opt ctxt ~level tx_rollup >>=? function
  | (ctxt, Some contents) ->
      size ctxt ~level tx_rollup >>=? fun (ctxt, cumulated_size) ->
      return (ctxt, Some {cumulated_size; contents})
  | (ctxt, None) -> return (ctxt, None)

let get :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_inbox_repr.t) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  (*
    [inbox_opt] checks whether or not [tx_rollup] is valid, so we
    don’t have to do it here.
   *)
  find ctxt ~level tx_rollup >>=? function
  | (ctxt, Some res) -> return (ctxt, res)
  | (_, None) ->
      fail (Tx_rollup_inbox_does_not_exist (tx_rollup, get_level ctxt level))

let get_adjacent_levels :
    Raw_context.t ->
    Raw_level_repr.t ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Raw_level_repr.t option * Raw_level_repr.t option) tzresult
    Lwt.t =
 fun ctxt level tx_rollup ->
  Storage.Tx_rollup.Inbox_metadata.find (ctxt, level) tx_rollup >>=? function
  | (ctxt, Some {predecessor; successor; _}) ->
      return (ctxt, predecessor, successor)
  | (_, None) -> fail @@ Tx_rollup_inbox_does_not_exist (tx_rollup, level)

(* Error registration *)

let () =
  let open Data_encoding in
  (* Tx_rollup_inbox_does_not_exist *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_inbox_does_not_exist"
    ~title:"Missing transaction rollup inbox"
    ~description:"The transaction rollup does not have an inbox at this level"
    ~pp:(fun ppf (addr, level) ->
      Format.fprintf
        ppf
        "Transaction rollup %a does not have an inbox at level %a"
        Tx_rollup_repr.pp
        addr
        Raw_level_repr.pp
        level)
    (obj2
       (req "tx_rollup_address" Tx_rollup_repr.encoding)
       (req "raw_level" Raw_level_repr.encoding))
    (function
      | Tx_rollup_inbox_does_not_exist (rollup, level) -> Some (rollup, level)
      | _ -> None)
    (fun (rollup, level) -> Tx_rollup_inbox_does_not_exist (rollup, level)) ;
  register_error_kind
    `Permanent
    ~id:"tx_rollup_inbox_size_would_exceed_limit"
    ~title:"Transaction rollup inbox’s size would exceed the limit"
    ~description:"Transaction rollup inbox’s size would exceed the limit"
    ~pp:(fun ppf addr ->
      Format.fprintf
        ppf
        "Adding the submitted message would make the inbox of %a exceed the \
         authorized limit at this level"
        Tx_rollup_repr.pp
        addr)
    (obj1 (req "tx_rollup_address" Tx_rollup_repr.encoding))
    (function
      | Tx_rollup_inbox_size_would_exceed_limit rollup -> Some rollup
      | _ -> None)
    (fun rollup -> Tx_rollup_inbox_size_would_exceed_limit rollup) ;
  (* Tx_rollup_message_size_exceed_limit *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_message_size_exceeds_limit"
    ~title:"A message submtitted to a transaction rollup inbox exceeds limit"
    ~description:
      "A message submtitted to a transaction rollup inbox exceeds limit"
    empty
    (function Tx_rollup_message_size_exceeds_limit -> Some () | _ -> None)
    (fun () -> Tx_rollup_message_size_exceeds_limit)
