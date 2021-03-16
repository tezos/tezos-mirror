(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Shell
    Invocation:   dune build @src/lib_shell/runtest
    Subject:      Setup mocks for testing shell, notably state and protocol
                  validation.
*)

open Filename.Infix

(** Basic blocks *)

let genesis_block_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol_hash =
  Protocol_hash.of_b58check_exn
    "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

let genesis_time = Time.Protocol.of_seconds 0L

let genesis_protocol =
  match Registered_protocol.get genesis_protocol_hash with
  | None ->
      assert false
  | Some genesis_protocol ->
      genesis_protocol

module Genesis_proto = (val genesis_protocol)

let state_genesis_block =
  {
    Genesis.time = genesis_time;
    block = genesis_block_hash;
    protocol = genesis_protocol_hash;
  }

let genesis : Genesis.t =
  {
    time = genesis_time;
    block = genesis_block_hash;
    protocol = genesis_protocol_hash;
  }

let chain_id = Chain_id.of_block_hash genesis_block_hash

(** [init_chain base_dir] with working directory [base_dir] returns a new state
    with a single genesis block *)
let init_chain base_dir =
  let store_root = base_dir // "store" in
  let context_root = base_dir // "context" in
  State.init
    ~store_root
    ~context_root
    ~history_mode:Archive
    state_genesis_block
  >>= function
  | Error _ ->
      Stdlib.failwith "read err"
  | Ok (state, chain, index, history_mode) ->
      Lwt.return (state, chain, index, history_mode)

(** [init_mock_p2p] initializes a mock p2p *)
let init_mock_p2p chain_name =
  let open Connection_metadata in
  let peer_metadata_cfg : _ P2p_params.peer_meta_config =
    {
      peer_meta_encoding = Peer_metadata.encoding;
      peer_meta_initial = Peer_metadata.empty;
      score = Peer_metadata.score;
    }
  in
  let message_cfg = Distributed_db_message.cfg chain_name in
  let c_meta = {disable_mempool = true; private_node = true} in
  return (P2p.faked_network message_cfg peer_metadata_cfg c_meta)

(* A mock event sink that records handled events for verifying test
   expectations. *)
module Mock_sink : sig
  include Internal_event.SINK

  (** A [filter] can be applied when asserting the contents of the sink. This
      restricts assertions to events from a specific section or to those that
      have no section. The value [filter] has the following meaning:
       - [filter = None] :: apply no filtering.
       - [filter = Some None] :: only keep events that have no section.
       - [filter = Some (Some s)] :: only keep events with section [s].
   *)
  type filter = Internal_event.Section.t option option

  type event =
    Internal_event.Level.t
    * Internal_event.Section.t option
    * Data_encoding.json

  (** [assert_has_event str filter strict e] asserts that Mock_sink has recorded
     an event [e], and fails with the assertion message [str] if not. If
     [filter] is set, only the events whose section is [filter] are
     searched. If [strict] is set, then [e] must be the only event recorded
     (subject to [filter).  *)
  val assert_has_event :
    string -> ?filter:filter -> ?strict:bool -> event -> unit

  (** [assert_has_events str filter strict es] asserts that Mock_sink has
     recorded the list of events [es], and fails with the assertion message
     [str] if not. If [filter] is set, only the events whose section is
     [filter] are searched. If [strict] is set, then [es] must be the only
     events recorded (subject to [filter).  *)
  val assert_has_events :
    string -> ?filter:filter -> ?strict:bool -> event list -> unit

  (** Clears recorded events, typically called in the initialization of each
   test. *)
  val clear_events : unit -> unit

  (** Returns true if this sink has been activated *)
  val is_activated : unit -> bool

  (** Pretty prints the list of recorded events *)
  val pp_state : ?filter:filter -> unit -> unit

  val get_events : ?filter:filter -> unit -> event list

  val testable_event : event Test_services.testable

  val testable_level : Internal_event.Level.t Test_services.testable

  val testable_section : Internal_event.Section.t Test_services.testable
end = struct
  type filter = Internal_event.Section.t option option

  type event =
    Internal_event.Level.t
    * Internal_event.Section.t option
    * Data_encoding.json

  type event_msg =
    string
    * Internal_event.Level.t
    * Internal_event.Section.t option
    * Data_encoding.json

  type t = unit

  let strip_msg (_, lvl, section, json) = (lvl, section, json)

  let recorded_events : event_msg list ref = ref []

  let activated : bool ref = ref false

  let uri_scheme = "mock-log"

  let configure _ =
    activated := true ;
    return ()

  let is_activated () = !activated

  let close (_ : t) : unit tzresult Lwt.t = return ()

  let handle (type a) (_ : t) m ?section (f : unit -> a) =
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    let ev = f () in
    let (s : string) = Format.asprintf "%a" (M.pp ~short:false) ev in
    let event = Data_encoding.Json.construct M.encoding ev in
    recorded_events := !recorded_events @ [(s, M.level ev, section, event)] ;
    return ()

  (** testing stuff *)

  let get_events_msg ?(filter = None) () : event_msg list =
    match filter with
    | None ->
        !recorded_events
    | Some f ->
        List.filter (fun (_, _, section, _) -> section = f) !recorded_events

  (** [get_events filter] returns the list of recorded events, in chronological
   order. If [filter] is set, only the events whose section is
   [filter] are returned. *)
  let get_events ?(filter = None) () : event list =
    List.map strip_msg (get_events_msg ~filter ())

  let clear_events () : unit = recorded_events := []

  (** Pretty prints the list of recorded events *)
  let pp_state ?(filter = None) () =
    Format.printf "-- State of Mock_sink --\n" ;
    List.iteri
      (fun i (msg, lvl, section, _) ->
        let section_pp =
          match section with
          | None ->
              "[~]"
          | Some section ->
              Format.asprintf "%a" Internal_event.Section.pp section
        in
        let lvl_pp = Internal_event.Level.to_string lvl in
        Format.printf
          "Registered event{%d}:\n   %s (%s): \"%s\"\n"
          i
          section_pp
          lvl_pp
          msg)
      (get_events_msg ~filter ()) ;
    Format.print_flush ()

  (** Testable for event *)
  open Test_services

  let testable_level : Internal_event.Level.t testable =
    let level_pp = Fmt.of_to_string Internal_event.Level.to_string in
    let level_eq l1 l2 = 0 == Internal_event.Level.compare l1 l2 in
    testable level_pp level_eq

  let testable_section : Internal_event.Section.t testable =
    of_pp Internal_event.Section.pp

  let testable_event : event Test_services.testable =
    tuple3 testable_level (option testable_section) json

  let assert_has_events str ?(filter = None) ?(strict = true) (es : event list)
      =
    let open Test_services in
    let events = get_events ~filter () in
    if strict then check (list testable_event) str es events
    else List.iter (fun e -> contains testable_event str e events) es

  let assert_has_event str ?(filter = None) ?(strict = true) (e : event) =
    assert_has_events str ~filter ~strict [e]
end

let mock_sink : Mock_sink.t Internal_event.sink_definition =
  (module Mock_sink : Internal_event.SINK with type t = Mock_sink.t)

(** [with_empty_mock_sink f] executes f after activating or clearing a Mock_sink
    sink.

  Sinks can only be registered and activated once, and not removed thereafter.
 *)
let with_empty_mock_sink (f : unit -> unit Lwt.t) : unit Lwt.t =
  ignore
    ( if not (Mock_sink.is_activated ()) then (
      Internal_event.All_sinks.register mock_sink ;
      Internal_event.All_sinks.activate (Uri.of_string "mock-log://")
      >>= function
      | Ok _ ->
          Lwt.return_unit
      | Error errors ->
          Format.printf
            "Could not initialize mock sink:\n   %a\n"
            pp_print_error
            errors ;
          Format.print_flush () ;
          Lwt.return_unit )
    else (Mock_sink.clear_events () ; Lwt.return_unit) ) ;
  f ()
