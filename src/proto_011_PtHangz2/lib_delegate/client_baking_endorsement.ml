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

open Protocol
open Alpha_context
open Protocol_client_context
module Events = Delegate_events.Endorsement

let get_signing_slots cctxt ~chain ~block delegate level =
  Plugin.RPC.Endorsing_rights.get
    cctxt
    ~levels:[level]
    ~delegates:[delegate]
    (chain, block)
  >>=? function
  | [{slots; _}] -> return_some slots
  | _ -> return_none

let inject_endorsement (cctxt : #Protocol_client_context.full) ?async ~chain
    ~block hash level delegate_sk delegate_pkh =
  Plugin.RPC.Endorsing_rights.get
    cctxt
    ~levels:[level]
    ~delegates:[delegate_pkh]
    (chain, block)
  >>=? function
  | [{slots = []; _}] | [] | _ :: _ :: _ -> assert false
  | [{slots = slot :: _; _}] ->
      Plugin.RPC.Forge.endorsement cctxt (chain, block) ~branch:hash ~level ()
      >>=? fun bytes ->
      let wallet = (cctxt :> Client_context.wallet) in
      (* Double-check the right to inject an endorsement *)
      let open Client_baking_highwatermarks in
      wallet#with_lock (fun () ->
          Client_baking_files.resolve_location cctxt ~chain `Endorsement
          >>=? fun endorsement_location ->
          may_inject_endorsement
            cctxt
            endorsement_location
            ~delegate:delegate_pkh
            level
          >>=? function
          | true ->
              record_endorsement
                cctxt
                endorsement_location
                ~delegate:delegate_pkh
                level
              >>=? fun () -> return_true
          | false -> return_false)
      >>=? fun is_allowed_to_endorse ->
      if is_allowed_to_endorse then
        Chain_services.chain_id cctxt ~chain () >>=? fun chain_id ->
        Client_keys.append
          cctxt
          delegate_sk
          ~watermark:(Endorsement chain_id)
          bytes
        >>=? fun signed_bytes ->
        (* wrap the legacy endorsement in a slot-bearing wrapper *)
        match
          Data_encoding.Binary.of_bytes_opt
            Alpha_context.Operation.encoding
            signed_bytes
        with
        | Some
            {
              shell;
              protocol_data =
                Operation_data
                  ({contents = Single (Endorsement _); _} as protocol_data);
            } ->
            let wrapped =
              {
                shell;
                protocol_data =
                  Operation_data
                    {
                      contents =
                        Single
                          (Endorsement_with_slot
                             {endorsement = {shell; protocol_data}; slot});
                      signature = None;
                    };
              }
            in
            let wrapped_bytes =
              Data_encoding.Binary.to_bytes_exn
                Alpha_context.Operation.encoding
                wrapped
            in
            Shell_services.Injection.operation cctxt ?async ~chain wrapped_bytes
            >>=? fun oph -> return oph
        | _ -> assert false
      else
        Events.(emit double_endorsement_near_miss) level >>= fun () ->
        fail (Level_previously_endorsed level)

let forge_endorsement (cctxt : #Protocol_client_context.full) ?async ~chain
    ~block ~src_sk src_pk =
  let src_pkh = Signature.Public_key.hash src_pk in
  Alpha_block_services.metadata cctxt ~chain ~block ()
  >>=? fun {protocol_data = {level_info = {level; _}; _}; _} ->
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  inject_endorsement cctxt ?async ~chain ~block hash level src_sk src_pkh
  >>=? fun oph ->
  Client_keys.get_key cctxt src_pkh >>=? fun (name, _pk, _sk) ->
  Events.(emit injected_endorsement) (hash, level, name, oph, src_pkh)
  >>= fun () -> return oph

(** Worker *)

(* because of delegates *)
[@@@ocaml.warning "-30"]

type state = {
  delegates : public_key_hash list;
  delay : int64;
  mutable pending : endorsements option;
}

and endorsements = {
  time : Time.Protocol.t;
  delegates : public_key_hash list;
  block : Client_baking_blocks.block_info;
}

[@@@ocaml.warning "+30"]

let create_state delegates delay = {delegates; delay; pending = None}

let get_delegates cctxt state =
  match state.delegates with
  | [] ->
      Client_keys.get_keys cctxt >>=? fun keys ->
      let delegates = List.map (fun (_, pkh, _, _) -> pkh) keys in
      return Signature.Public_key_hash.Set.(delegates |> of_list |> elements)
  | _ :: _ as delegates -> return delegates

let endorse_for_delegate cctxt block delegate_pkh =
  let {Client_baking_blocks.hash; level; chain_id; _} = block in
  Client_keys.get_key cctxt delegate_pkh >>=? fun (name, _pk, delegate_sk) ->
  Events.(emit endorsing) (hash, name, level) >>= fun () ->
  let chain = `Hash chain_id in
  let block = `Hash (hash, 0) in
  inject_endorsement cctxt ~chain ~block hash level delegate_sk delegate_pkh
  >>=? fun oph ->
  Events.(emit injected_endorsement) (hash, level, name, oph, delegate_pkh)
  >>= fun () -> return_unit

let allowed_to_endorse cctxt bi delegate =
  Client_keys.Public_key_hash.name cctxt delegate >>=? fun name ->
  Events.(emit check_endorsement_ok) (bi.Client_baking_blocks.hash, name)
  >>= fun () ->
  let chain = `Hash bi.chain_id in
  let block = `Hash (bi.hash, 0) in
  let level = bi.level in
  get_signing_slots cctxt ~chain ~block delegate level >>=? function
  | None | Some [] ->
      Events.(emit endorsement_no_slots_found) (bi.hash, name) >>= fun () ->
      return_false
  | Some (_ :: _ as slots) -> (
      Events.(emit endorsement_slots_found) (bi.hash, name, slots) >>= fun () ->
      cctxt#with_lock (fun () ->
          Client_baking_files.resolve_location cctxt ~chain `Endorsement
          >>=? fun endorsement_location ->
          Client_baking_highwatermarks.may_inject_endorsement
            cctxt
            endorsement_location
            ~delegate
            level)
      >>=? function
      | false ->
          Events.(emit previously_endorsed) level >>= fun () -> return_false
      | true -> return_true)

let prepare_endorsement ~(max_past : int64) ()
    (cctxt : #Protocol_client_context.full) state bi =
  let past =
    Time.Protocol.diff
      (Time.System.to_protocol (Time.System.now ()))
      bi.Client_baking_blocks.timestamp
  in
  if past > max_past then
    Events.(emit endorsement_stale_block) bi.hash >>= fun () -> return_unit
  else
    Events.(emit endorsement_got_block) bi.hash >>= fun () ->
    let time =
      Time.Protocol.add
        (Time.System.to_protocol (Time.System.now ()))
        state.delay
    in
    get_delegates cctxt state >>=? fun delegates ->
    List.filter_ep (allowed_to_endorse cctxt bi) delegates >>=? fun delegates ->
    state.pending <- Some {time; block = bi; delegates} ;
    return_unit

let compute_timeout state =
  match state.pending with
  | None -> Lwt_utils.never_ending ()
  | Some {time; block; delegates} -> (
      match Client_baking_scheduling.sleep_until time with
      | None -> Lwt.return (block, delegates)
      | Some timeout ->
          let timespan =
            let timespan =
              Ptime.diff (Time.System.of_protocol_exn time) (Time.System.now ())
            in
            if Ptime.Span.compare timespan Ptime.Span.zero > 0 then timespan
            else Ptime.Span.zero
          in
          Events.(emit wait_before_injecting)
            (Time.System.of_protocol_exn time, timespan)
          >>= fun () ->
          timeout >>= fun () -> Lwt.return (block, delegates))

(* Refuse to endorse block that are more than 20min old.
   1200 is greater than 60 + 40*(p - 1) + 192*4 with p = 10, i.e.
   we wait at least for the maximum delay for blocks with priority 10. *)
let default_max_past = 1200L

let create (cctxt : #Protocol_client_context.full)
    ?(max_past = default_max_past) ~delay delegates block_stream =
  let state_maker _ =
    let state = create_state delegates (Int64.of_int delay) in
    return state
  in
  let timeout_k cctxt state (block, delegates) =
    state.pending <- None ;
    List.iter_es
      (fun delegate ->
        endorse_for_delegate cctxt block delegate >>= function
        | Ok () -> return_unit
        | Error errs ->
            Events.(emit error_while_endorsing) (delegate, errs) >>= fun () ->
            (* We continue anyway *)
            return_unit)
      delegates
  in
  let event_k cctxt state bi =
    state.pending <- None ;
    prepare_endorsement ~max_past () cctxt state bi
  in
  Client_baking_scheduling.main
    ~name:"endorser"
    ~cctxt
    ~stream:block_stream
    ~state_maker
    ~pre_loop:(prepare_endorsement ~max_past ())
    ~compute_timeout
    ~timeout_k
    ~event_k
    ~finalizer:(fun _ -> Lwt.return_unit)
