(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type liquidity_baking_vote = Off | On | Pass

let liquidity_baking_vote_to_string = function
  | Off -> "off"
  | On -> "on"
  | Pass -> "pass"

let liquidity_baking_vote_of_string_opt = function
  | "off" -> Some Off
  | "on" -> Some On
  | "pass" -> Some Pass
  | _ -> None

module Parameters = struct
  type persistent_state = {
    protocol : Protocol.t;
    delegates : string list;
    runner : Runner.t option;
    base_dir : string;
    node_data_dir : string;
    node_rpc_endpoint : Endpoint.t;
    dal_node_rpc_endpoint : Endpoint.t option;
    dal_node_timeout_percentage : int option;
    mutable pending_ready : unit option Lwt.u list;
    votefile : string option;
    liquidity_baking_toggle_vote : liquidity_baking_vote option;
    force_apply_from_round : int option;
    remote_mode : bool;
    operations_pool : string option;
    minimal_nanotez_per_gas_unit : int option;
    state_recorder : bool;
    node_version_check_bypass : bool;
    node_version_allowed : string option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "baker"

  let default_colors = Log.Color.[|FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let trigger_ready baker value =
  let pending = baker.persistent_state.pending_ready in
  baker.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready baker =
  (match baker.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready baker (Some ())

let handle_raw_stdout baker line =
  if line =~ rex "^Baker .+ for .+ started.$" then set_ready baker

let liquidity_baking_votefile ?path vote =
  let votefile =
    Option.value
      path
      ~default:(Temp.file "liquidity_baking_toggle_votefile.json")
  in
  JSON.encode_to_file_u
    votefile
    (`O
      [
        ( "liquidity_baking_toggle_vote",
          `String (liquidity_baking_vote_to_string vote) );
      ]) ;
  votefile

let create_from_uris ?runner ~protocol
    ?(path = Uses.path (Protocol.baker protocol)) ?name ?color ?event_pipe
    ?(delegates = []) ?votefile ?(liquidity_baking_toggle_vote = Some Pass)
    ?force_apply_from_round ?(remote_mode = false) ?operations_pool
    ?dal_node_rpc_endpoint ?dal_node_timeout_percentage
    ?minimal_nanotez_per_gas_unit ?(state_recorder = false)
    ?(node_version_check_bypass = false) ?node_version_allowed ~base_dir
    ~node_data_dir ~node_rpc_endpoint () =
  let baker =
    create
      ~path
      ?name
      ?color
      ?event_pipe
      ?runner
      {
        protocol;
        delegates;
        runner;
        base_dir;
        node_data_dir;
        node_rpc_endpoint;
        pending_ready = [];
        votefile;
        liquidity_baking_toggle_vote;
        remote_mode;
        force_apply_from_round;
        operations_pool;
        dal_node_rpc_endpoint;
        dal_node_timeout_percentage;
        minimal_nanotez_per_gas_unit;
        state_recorder;
        node_version_check_bypass;
        node_version_allowed;
      }
  in
  on_stdout baker (handle_raw_stdout baker) ;
  baker

let create ?runner ~protocol ?path ?name ?color ?event_pipe ?(delegates = [])
    ?votefile ?(liquidity_baking_toggle_vote = Some Pass)
    ?force_apply_from_round ?(remote_mode = false) ?operations_pool ?dal_node
    ?dal_node_timeout_percentage ?minimal_nanotez_per_gas_unit
    ?(state_recorder = false) ?(node_version_check_bypass = false)
    ?node_version_allowed node client =
  let dal_node_rpc_endpoint = Option.map Dal_node.as_rpc_endpoint dal_node in
  create_from_uris
    ?runner
    ~protocol
    ?path
    ?name
    ?color
    ?event_pipe
    ~delegates
    ?votefile
    ~liquidity_baking_toggle_vote
    ?force_apply_from_round
    ~remote_mode
    ?operations_pool
    ?minimal_nanotez_per_gas_unit
    ?dal_node_rpc_endpoint
    ?dal_node_timeout_percentage
    ~state_recorder
    ~node_version_check_bypass
    ?node_version_allowed
    ~base_dir:(Client.base_dir client)
    ~node_data_dir:(Node.data_dir node)
    ~node_rpc_endpoint:(Node.as_rpc_endpoint node)
    ()

let run ?event_level ?event_sections_levels (baker : t) =
  (match baker.status with
  | Not_running -> ()
  | Running _ -> Test.fail "baker %s is already running" baker.name) ;
  let delegates = baker.persistent_state.delegates in
  let runner = baker.persistent_state.runner in
  let node_data_dir = baker.persistent_state.node_data_dir in
  let base_dir = baker.persistent_state.base_dir in
  let node_addr = Endpoint.as_string baker.persistent_state.node_rpc_endpoint in
  let votefile =
    Cli_arg.optional_arg "votefile" Fun.id baker.persistent_state.votefile
  in
  let liquidity_baking_toggle_vote =
    Cli_arg.optional_arg
      "liquidity-baking-toggle-vote"
      liquidity_baking_vote_to_string
      baker.persistent_state.liquidity_baking_toggle_vote
  in
  let force_apply_from_round =
    (* From Quebec, the flag --force-apply has been replaced by
       --force-apply-from-round, the following maintains back-compatibility with
       ParisC tests. *)
    if
      Protocol.number baker.persistent_state.protocol
      > Protocol.number Protocol.ParisC
    then
      Cli_arg.optional_arg
        "force-apply-from-round"
        string_of_int
        baker.persistent_state.force_apply_from_round
    else
      Cli_arg.optional_switch
        "force-apply"
        (Option.is_some baker.persistent_state.force_apply_from_round)
  in
  let operations_pool =
    Cli_arg.optional_arg
      "operations-pool"
      Fun.id
      baker.persistent_state.operations_pool
  in
  let dal_node_endpoint =
    Cli_arg.optional_arg
      "dal-node"
      Endpoint.as_string
      baker.persistent_state.dal_node_rpc_endpoint
  in
  let dal_node_timeout_percentage =
    Cli_arg.optional_arg
      "dal-node-timeout-percentage"
      string_of_int
      baker.persistent_state.dal_node_timeout_percentage
  in
  let minimal_nanotez_per_gas_unit =
    Cli_arg.optional_arg
      "minimal-nanotez-per-gas-unit"
      (fun nanotez_per_gas -> string_of_int nanotez_per_gas)
      baker.persistent_state.minimal_nanotez_per_gas_unit
  in
  let state_recorder =
    Cli_arg.optional_switch "record-state" baker.persistent_state.state_recorder
  in
  let node_version_check_bypass =
    Cli_arg.optional_switch
      "node-version-check-bypass"
      baker.persistent_state.node_version_check_bypass
  in
  let node_version_allowed =
    Cli_arg.optional_arg
      "node-version-allowed"
      Fun.id
      baker.persistent_state.node_version_allowed
  in
  let run_args =
    if baker.persistent_state.remote_mode then ["remotely"]
    else ["with"; "local"; "node"; node_data_dir]
  in
  let arguments =
    ["--endpoint"; node_addr; "--base-dir"; base_dir; "run"]
    @ run_args @ liquidity_baking_toggle_vote @ votefile
    @ force_apply_from_round @ operations_pool @ dal_node_endpoint
    @ dal_node_timeout_percentage @ delegates @ minimal_nanotez_per_gas_unit
    @ state_recorder @ node_version_check_bypass @ node_version_allowed
  in

  let on_terminate _ =
    (* Cancel all [Ready] event listeners. *)
    trigger_ready baker None ;
    unit
  in
  run
    ?event_level
    ?event_sections_levels
    baker
    {ready = false}
    arguments
    ~on_terminate
    ?runner

let check_event ?where baker name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = baker.name; event = name; where})
  | Some x -> return x

let wait_for_ready baker =
  match baker.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      baker.persistent_state.pending_ready <-
        resolver :: baker.persistent_state.pending_ready ;
      check_event baker "Baker started." promise

let init ?runner ~protocol ?(path = Uses.path (Protocol.baker protocol)) ?name
    ?color ?event_level ?event_pipe ?event_sections_levels ?(delegates = [])
    ?votefile ?liquidity_baking_toggle_vote ?force_apply_from_round ?remote_mode
    ?operations_pool ?dal_node ?dal_node_timeout_percentage
    ?minimal_nanotez_per_gas_unit ?state_recorder ?node_version_check_bypass
    ?node_version_allowed node client =
  let* () = Node.wait_for_ready node in
  let baker =
    create
      ?runner
      ~path
      ~protocol
      ?name
      ?color
      ?event_pipe
      ?votefile
      ?liquidity_baking_toggle_vote
      ?force_apply_from_round
      ?remote_mode
      ?operations_pool
      ?dal_node
      ?dal_node_timeout_percentage
      ?minimal_nanotez_per_gas_unit
      ?state_recorder
      ?node_version_check_bypass
      ?node_version_allowed
      ~delegates
      node
      client
  in
  let* () = run ?event_level ?event_sections_levels baker in
  let* () = wait_for_ready baker in
  return baker

(** Logging helpers for baker events. *)

let log_block_injection ?color baker =
  on_event baker (fun event ->
      if String.equal event.name "block_injected.v0" then
        let open JSON in
        let level = event.value |-> "level" |> as_int in
        let round = event.value |-> "round" |> as_int in
        let delegate = event.value |-> "delegate" |-> "alias" |> as_string in
        Log.info
          ?color
          "[%s] Block injected at level %d round %d for %s."
          (name baker)
          level
          round
          delegate)

(** Short and readable logging of all baker events, for debugging purposes. *)

let encode_mini json =
  Ezjsonm.value_to_string ~minify:true (JSON.unannotate json)

(* Only keep the first 10 characters of hashes to print. *)
let shorten s = try String.sub s 0 10 with Invalid_argument _ -> s

let shorten_json_string json =
  let s = JSON.as_string json in
  if String.length s <= 10 then json
  else JSON.annotate ~origin:"shorten" (Ezjsonm.string (shorten s))

let try_simplify_delegate_object json =
  let open JSON in
  try
    let alias = json |-> "alias" in
    if is_string alias then (
      let obj = as_object json in
      let _ = json |-> "public_key" |> as_string in
      let _ = json |-> "public_key_hash" |> as_string in
      let _ = json |-> "secret_key_uri" |> as_string in
      let _ = json |-> "delegate" |> as_string in
      if List.length obj <> 5 then
        Log.warn "%s: unexpected fields: %s" __LOC__ (encode_mini json) ;
      Some alias)
    else None
  with _ ->
    Log.warn
      "%s: has field alias but not all expected fields for delegate: %s"
      __LOC__
      (encode_mini json) ;
    None

let rec make_readable json =
  let open JSON in
  if is_string json then shorten_json_string json
  else if is_object json then
    match try_simplify_delegate_object json with
    | Some output -> output
    | None -> filter_map_object json make_field_readable
  else json

and make_field_readable field_name value =
  (match field_name with
  | "timestamp" -> (* Usually a string: do not shorten it. *) value
  | "timespan" -> (
      match Option.bind (JSON.as_float_opt value) Ptime.Span.of_float_s with
      | None -> value
      | Some span ->
          let rounded = Ptime.Span.(to_float_s (round ~frac_s:2 span)) in
          JSON.annotate ~origin:"rounded timespan" (Ezjsonm.float rounded))
  | _ -> make_readable value)
  |> Option.some

let encode_readable json = encode_mini (make_readable json)

let show_operations ~loc ?expected_kind ?expected_level_and_round ops =
  let open JSON in
  let rec aux expected_kind expected_level_and_round acc_slots = function
    | [] -> (expected_kind, expected_level_and_round, acc_slots)
    | op :: remaining_ops ->
        let contents = op |-> "contents" |=> 0 in
        let kind = contents |-> "kind" |> as_string in
        let level = contents |-> "level" |> as_int in
        let round = contents |-> "round" |> as_int in
        (match expected_kind with
        | Some expected_kind when not (String.equal expected_kind kind) ->
            Log.warn "%s: expected %s but got %s" loc expected_kind kind ;
            assert false
        | _ -> ()) ;
        (match expected_level_and_round with
        | Some (expected_level, expected_round)
          when not (expected_level = level && expected_round = round) ->
            Log.warn
              "%s expected op with (level %d, round %d) but got (%d, %d)"
              loc
              expected_level
              level
              expected_round
              round ;
            assert false
        | _ -> ()) ;
        let slot = contents |-> "slot" |> as_int in
        aux (Some kind) (Some (level, round)) (slot :: acc_slots) remaining_ops
  in
  match aux expected_kind expected_level_and_round [] ops with
  | _, _, [] -> "(0 operations)"
  | Some kind, Some (level, round), rev_slots ->
      sf
        "(%d %ss on level %d round %d with slots [%s])"
        (List.length ops)
        kind
        level
        round
        (String.concat ";" (List.map string_of_int (List.rev rev_slots)))
  | _ -> assert false

let show_block json =
  let open JSON in
  let prequorum =
    let prequorum = json |-> "prequorum" in
    if is_null prequorum then "None"
    else
      let ops = prequorum |-> "preendorsements" |> as_list in
      let expected_level_and_round =
        (prequorum |-> "level" |> as_int, prequorum |-> "round" |> as_int)
      in
      let expected_kind = "preendorsement" in
      show_operations ~loc:__LOC__ ~expected_kind ~expected_level_and_round ops
  in
  let quorum =
    match json |-> "quorum" |> as_list with
    | [] -> "[]"
    | ops -> show_operations ~loc:__LOC__ ~expected_kind:"endorsement" ops
  in
  sf
    "block={hash=%s, level=%d, round=%d, prequorum=%s, quorum=%s}"
    (json |-> "hash" |> as_string |> shorten)
    (json |-> "shell" |-> "level" |> as_int)
    (json |-> "round" |> as_int)
    prequorum
    quorum

let show_step_event_data json =
  let open JSON in
  try
    let block = json |-> "block" in
    if not (is_null block) then show_block block
    else
      match as_list_opt json with
      | Some ops -> show_operations ~loc:__LOC__ ops
      | None -> encode_readable json
  with _ -> encode_readable json

let log_step_current_phase_exn prefix json =
  let open JSON in
  let phase = json |-> "phase" |> as_string in
  let event_name, event_data_list =
    match json |-> "event" |> as_list with
    | name :: infos -> (as_string name, infos)
    | _ -> assert false
  in
  Log.info
    "%s phase=%s, event=%s %s"
    prefix
    phase
    event_name
    (String.concat " " (List.map show_step_event_data event_data_list))

let log_step_current_phase prefix json =
  try log_step_current_phase_exn prefix json
  with _ -> Log.warn "%s Unexpected format: %s" prefix (JSON.encode json)

let log_shortened_events baker =
  let handler (event : event) =
    let prefix = Format.sprintf "[%s] %s:" baker.name event.name in
    match event.name with
    | "step_current_phase.v0" -> log_step_current_phase prefix event.value
    | _ -> Log.info "%s %s" prefix (encode_readable event.value)
  in
  on_event baker handler
