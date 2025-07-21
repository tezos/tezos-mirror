(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Prerequisite:

   In order to be able to run the following test successfully, you need to make
   sure that your environment is well configured. To do so, have a look at the
   tezt/lib_cloud/README.md documentation.

   Additionally, if you are running the test you must ensure that:
   - DAL_TRUSTED_SETUP_PATH contains the expected data -- this can be done by
     running `./scripts/install_dal_trusted_setup.sh`
   - smart rollup binaries are available -- requires to run `make -f kernels.mk
     build-deps` and `make -f kernels.mk kernel_sdk`
   - floodgate binaries are available -- `make build-floodgate`
*)

module Cryptobox = Dal_common.Cryptobox
module Helpers = Dal_common.Helpers
module Cli = Scenarios_cli
open Agent_kind
open Scenarios_helpers
open Tezos
open Yes_crypto

type configuration = {
  with_dal : bool;
  stake : int list Lwt.t;
  bakers : string list; (* unencrypted secret keys *)
  stake_machine_type : string list option;
  dal_node_producers : int list; (* slot indices *)
  observer_slot_indices : int list;
  observer_pkhs : string list;
  protocol : Protocol.t;
  producer_machine_type : string option;
  (* The first argument is the deconnection frequency, the second is the
     reconnection delay *)
  echo_rollup : bool;
  disconnect : (int * int) option;
  network : Network.t;
  simulate_network : Cli.network_simulation_config;
  snapshot : Snapshot_helpers.t;
  bootstrap : bool;
  teztale : bool;
  memtrace : bool;
  data_dir : string option;
  producer_key : string option;
  fundraiser : string option;
  producers_delay : int;
  blocks_history : int;
  bootstrap_node_identity_file : string option;
  bootstrap_dal_node_identity_file : string option;
  external_rpc : bool;
  disable_shard_validation : bool;
  ignore_pkhs : string list;
  ppx_profiling : bool;
  ppx_profiling_backends : string list;
  network_health_monitoring : bool;
}

type bootstrap = {
  node : Node.t option;
  dal_node : Dal_node.t option;
  node_p2p_endpoint : string;
  node_rpc_endpoint : Endpoint.t;
  dal_node_p2p_endpoint : string option;
  client : Client.t;
}

type t = {
  configuration : configuration;
  cloud : Cloud.t;
  bootstrap : bootstrap;
  some_node_rpc_endpoint : Endpoint.t;
  (* endpoint to be used for get various information about L1; for testnets, it
     is a public endpoint only if no L1 node is run by the scenario, in contrast
     to [bootstrap.node_rpc_endpoint] which is a public endpoint when the
     '--bootstrap' argument is not provided *)
  bakers : Baker_helpers.baker list;
  producers : Dal_node_helpers.producer list;
      (* NOTE: they have the observer profile*)
  observers : Dal_node_helpers.observer list;
  etherlink : Etherlink_helpers.etherlink option;
  echo_rollup : Echo_rollup.operator option;
  time_between_blocks : int;
  parameters : Dal_common.Parameters.t;
  infos : (int, Metrics.per_level_info) Hashtbl.t;
  metrics : (int, Metrics.t) Hashtbl.t;
  disconnection_state : Disconnect.t option;
  first_level : int;
  teztale : Teztale.t option;
  mutable versions : (string, string) Hashtbl.t;
      (* mapping from baker addresses to their octez versions (if known) *)
  otel : string option;
}

module Monitoring_app = struct
  (* time interval in hours at which to submit report *)
  let report_interval = 6

  let pp_delegate fmt delegate_pkh =
    match Hashtbl.find_opt Metrics.aliases delegate_pkh with
    | None -> Format.fprintf fmt "%s" delegate_pkh
    | Some alias ->
        Format.fprintf fmt "%s : %-26s" (String.sub delegate_pkh 0 7) alias

  (** [network_to_image_url network] return an image for each monitored network. *)
  let network_to_image_url : Network.t -> string = function
    | `Rionet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/rionet.png"
    | `Mainnet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/mainnet.png"
    | `Ghostnet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/ghostnet.png"
    | `Nextnet _ | `Seoulnet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/seoulnet.png"
    | `Sandbox | `Weeklynet _ -> "no_image_yet"

  module Format_app = struct
    (* Helper for Slack App message format block-kit
       See: https://api.slack.com/reference/block-kit/
    *)

    let image ~url ~alt =
      let open Ezjsonm in
      `O
        [
          ("type", string "image");
          ("image_url", string url);
          ("alt_text", string alt);
        ]

    (* [section content ?accessory] creates Slack App message blocks
       with an optional accessory.

       The function joins content strings with newlines, formats them
       using mrkdwn, and returns properly structured JSON objects for
       Slack's Block Kit. *)
    let section content ?accessory () =
      let open Ezjsonm in
      if List.is_empty content then []
      else
        let text = String.concat "\n" content in
        [
          `O
            (("type", string "section")
            :: ("text", `O [("type", string "mrkdwn"); ("text", string text)])
            :: Option.fold
                 ~none:[]
                 ~some:(fun data -> [("accessory", data)])
                 accessory);
        ]

    let make_payload ~slack_channel_id ?ts content =
      let open Ezjsonm in
      `O
        (("channel", string slack_channel_id)
        :: ("blocks", `A content)
        :: Option.fold ~none:[] ~some:(fun ts -> [("thread_ts", string ts)]) ts
        )
  end

  let post_message ?ts ~slack_channel_id ~slack_bot_token data =
    let data = Format_app.make_payload ?ts ~slack_channel_id data in
    let slack_endpoint =
      Endpoint.make ~scheme:"https" ~host:"slack.com" ~port:443 ()
    in
    let rpc =
      RPC_core.make
        ~data:(Data data)
        POST
        ["api"; "chat.postMessage"]
        (fun _json -> ())
    in
    let* response =
      RPC_core.call_raw
        ~extra_headers:
          [("Authorization", Format.sprintf "Bearer %s" slack_bot_token)]
        slack_endpoint
        rpc
    in
    let thread_id =
      let open JSON in
      parse ~origin:"DAL.Monitoring_app.chat_postMessage" response.body
      |-> "ts" |> as_string
    in
    return thread_id

  module Tasks = struct
    let endpoint_from_prometheus_query ~query =
      let fail ~uri_part =
        Test.fail
          "DAL.Monitoring_app.Tasks.endpoint_from_prometheus_query: expecting \
           a prometheus %s"
          uri_part
      in
      let uri =
        match Prometheus.get_query_endpoint ~query with
        | None -> fail ~uri_part:"endpoint"
        | Some endpoint -> endpoint
      in
      let scheme, host, port =
        match Uri.(scheme uri, host uri, port uri) with
        | Some scheme, Some host, Some port -> (scheme, host, port)
        | None, _, _ -> fail ~uri_part:"scheme"
        | _, None, _ -> fail ~uri_part:"host"
        | _, _, None -> fail ~uri_part:"port"
      in
      let query_string =
        (* Fixme: warn about `k` being dropped in the second case. We
           need to keep only list of size 1 because of the way RPC_core
           is implemented. Should probably be fixed soon or later. *)
        List.filter_map
          (function k, [v] -> Some (k, v) | _k, _ -> None)
          (Uri.query uri)
      in
      let path = String.split_on_char '/' (Uri.path uri) in
      let endpoint = Endpoint.make ~host ~scheme ~port () in
      (`endpoint endpoint, `query query_string, `path path)

    let fetch ~origin ~query ~decoder =
      let open RPC_core in
      let `endpoint endpoint, `query query_string, `path path =
        endpoint_from_prometheus_query ~query
      in
      let rpc = make ~query_string GET path decoder in
      Lwt.catch
        (fun () ->
          let* response = call_raw endpoint rpc in
          Lwt.return (decode_raw ~origin rpc response.body))
        (fun exn ->
          Log.warn
            "Unexpected error while fetching prometheus query (origin : %s): \
             '%s'"
            origin
            (Printexc.to_string exn) ;
          Lwt.return_none)

    let decoder_prometheus_float json =
      let open JSON in
      let status = json |-> "status" |> as_string in
      if not (String.equal status "success") then (* fixme: warning *)
        None
      else
        let opt =
          json |-> "data" |-> "result" |> as_list |> Fun.flip List.nth_opt 0
        in
        match opt with
        | None -> None
        | Some x ->
            x |-> "value" |> as_list |> Fun.flip List.nth_opt 1
            |> Option.map as_float

    let view_ratio_attested_over_published
        (`attested attested, `published published) =
      let open Format in
      match (attested, published) with
      | Some 0., Some 0. -> None
      | None, None -> None
      | Some attested, Some 0. ->
          let s = sprintf "`unk` (`%d`/`0`)" (Int.of_float attested) in
          Some s
      | Some attested, None ->
          let s = sprintf "`unk` (`%d`/`?`)" (Int.of_float attested) in
          Some s
      | None, Some published ->
          let s = sprintf "`unk` (`?`/`%d`)" (Int.of_float published) in
          Some s
      | Some attested, Some published ->
          let ratio = attested /. published *. 100. in
          let s =
            sprintf
              "`%s` (`%d/%d`)"
              (sprintf "%.2f%%" ratio)
              (Int.of_float attested)
              (Int.of_float published)
          in
          Some s

    let fetch_slot_info ~slot_index =
      let query s =
        Format.sprintf
          "increase(tezt_total_%s_commitments_per_slot{slot_index=\"%d\"}[%dh])"
          s
          slot_index
          report_interval
      in
      let decoder = decoder_prometheus_float in
      let* attested =
        fetch
          ~origin:"fetch_slot_info.attested"
          ~decoder
          ~query:(query "attested")
      in
      let* published =
        fetch
          ~origin:"fetch_slot_info.published"
          ~decoder
          ~query:(query "published")
      in
      Lwt.return
        (`slot_index slot_index, `attested attested, `published published)

    let view_slot_info slot_info =
      let slots_info =
        List.filter_map
          (fun (`slot_index slot_index, attested, published) ->
            view_ratio_attested_over_published (attested, published)
            |> Option.map
                 (Format.sprintf ":black_small_square: `%02d` : %s" slot_index))
          slot_info
      in
      if List.is_empty slots_info then []
      else
        "• Percentage of attested over published DAL commitments per slot:"
        :: slots_info

    let fetch_dal_commitments_total_info () =
      let query s =
        Format.sprintf
          {|increase(tezt_dal_commitments_total{kind="%s"}[%dh])|}
          s
          report_interval
      in
      let decoder = decoder_prometheus_float in
      let* attested =
        fetch
          ~origin:"fetch_dal_commitments_total.attested"
          ~decoder
          ~query:(query "attested")
      in
      let* published =
        fetch
          ~origin:"fetch_dal_commitments_total.published"
          ~decoder
          ~query:(query "published")
      in
      let ratio =
        view_ratio_attested_over_published
          (`attested attested, `published published)
      in
      let ratio_view =
        (Format.sprintf
           "• Percentage of attested over published DAL commitments: %s")
          (Option.value ~default:"unk" ratio)
      in
      let slot_size = 126_944 (* TODO: do not hard-code this *) in
      let bandwidth =
        Option.map
          (fun x ->
            Format.sprintf
              "%.2f"
              (x *. float_of_int slot_size
              /. float_of_int (1024 * report_interval * 3600)))
          attested
      in
      let bandwidth_view =
        Format.sprintf
          "• Bandwidth: %s KiB/s"
          (Option.value ~default:"unk" bandwidth)
      in
      Lwt.return (ratio_view, bandwidth_view)

    let pp_stake fmt stake_ratio =
      Format.fprintf fmt "%.2f%% stake" (stake_ratio *. 100.)

    type baker_attestation_numbers = {
      (* The rate of attested/attestable slots for this baker.
         An attestatble slot is a published slot for which the baker is in
         the DAL committee at attestation level.
      *)
      slot_attestation_rate : float option;
      (* The rate of attestation_with_dal when the baker is in the DAL committee.
         This ratio is especially useful for small bakers which are rarely in
         the DAL committee, hence have very few opportunities to attest slots
         when publication is quite rare.
      *)
      dal_content_rate : float option;
      (* This boolean is true if the baker sent at least one attestation while
         out of the DAL committee. This is useful to detect bakers who attest
         but have a broken DAL setup preventing them to send attestations while
         in DAL committee.
      *)
      attests_while_out_of_dal_committee : bool;
    }

    type baker_infos = {
      address : Metrics.public_key_hash;
      attest_infos : baker_attestation_numbers;
      stake_fraction : float;
    }

    let pp_baker_light fmt {address = PKH delegate_pkh; stake_fraction; _} =
      Format.fprintf
        fmt
        "%a (%a)"
        pp_delegate
        delegate_pkh
        pp_stake
        stake_fraction

    let pp_baker_dal_status fmt baker =
      Format.fprintf
        fmt
        "%a (%s)"
        pp_baker_light
        baker
        (match baker.attest_infos.dal_content_rate with
        | None -> "Never sent attestations while in DAL committee"
        | Some 0. -> "OFF"
        | Some 1. -> "ON"
        | Some x -> Format.sprintf "ACTIVE %.0f%% of the time" (x *. 100.))

    let pp_bakers_all_infos fmt baker =
      Format.fprintf
        fmt
        "%s - %a"
        (Option.fold
           ~none:"Never was in committee when slots were produced"
           ~some:(fun value ->
             let percentage = value *. 100. in
             Format.sprintf "%.2f%%" percentage)
           baker.attest_infos.slot_attestation_rate)
        pp_baker_dal_status
        baker

    let fetch_baker_info ~tz1 ~origin =
      let query =
        Format.sprintf
          "sum_over_time(tezt_dal_commitments_attested{attester=\"%s\"}[%dh])"
          tz1
          report_interval
      in
      let* attested = fetch ~decoder:decoder_prometheus_float ~query ~origin in
      let query =
        Format.sprintf
          "sum_over_time(tezt_dal_commitments_attestable{attester=\"%s\"}[%dh])"
          tz1
          report_interval
      in
      let* attestable =
        fetch ~decoder:decoder_prometheus_float ~query ~origin
      in
      let query =
        Format.sprintf
          "avg_over_time(tezt_dal_attestation_sent{attester=\"%s\"}[%dh])"
          tz1
          report_interval
      in
      let* dal_content_rate =
        fetch ~decoder:decoder_prometheus_float ~query ~origin
      in
      let query =
        Format.sprintf
          "sum_over_time(tezt_attestation_sent_when_out_of_dal_committee{attester=\"%s\"}[6h])"
          tz1
      in
      let* out_attestations =
        fetch ~decoder:decoder_prometheus_float ~query ~origin
      in
      let attests_while_out_of_dal_committee =
        Option.is_some out_attestations
      in
      let slot_attestation_rate =
        match (attested, attestable) with
        | None, _ | _, None | _, Some 0. -> None
        | Some attested, Some attestable -> Some (attested /. attestable)
      in
      return
        {
          slot_attestation_rate;
          dal_content_rate;
          attests_while_out_of_dal_committee;
        }

    let get_current_cycle endpoint =
      let* {cycle; _} =
        RPC_core.call endpoint (RPC.get_chain_block_helper_current_level ())
      in
      Lwt.return cycle

    let get_bakers_with_staking_power endpoint cycle =
      RPC_core.call endpoint (RPC.get_stake_distribution ~cycle ())

    type classified_bakers = {
      mute_bakers : baker_infos list;
      muted_by_dal : baker_infos list;
      dal_zero : baker_infos list;
      dal_on : baker_infos list;
      no_shards : baker_infos list;
      dal_off : baker_infos list;
    }

    let fetch_bakers_info endpoint =
      let* cycle = get_current_cycle endpoint in
      let* bakers = get_bakers_with_staking_power endpoint cycle in
      let total_baking_power =
        List.fold_left
          (fun acc RPC.{baking_power; _} -> acc + baking_power)
          0
          bakers
      in
      let* bakers_info =
        Lwt_list.filter_map_p
          (fun RPC.{delegate; baking_power} ->
            let* attest_infos =
              fetch_baker_info
                ~origin:(Format.sprintf "fetch_baker_info.%s" delegate)
                ~tz1:delegate
            in
            let stake_fraction =
              float_of_int baking_power /. float_of_int total_baking_power
            in
            Lwt.return_some
              {address = PKH delegate; attest_infos; stake_fraction})
          bakers
      in
      let rec classify_bakers already_classified = function
        | [] -> already_classified
        | ({attest_infos; _} as baker) :: tl -> (
            match attest_infos.dal_content_rate with
            | None ->
                if attest_infos.attests_while_out_of_dal_committee then
                  classify_bakers
                    {
                      already_classified with
                      muted_by_dal = baker :: already_classified.muted_by_dal;
                    }
                    tl
                else
                  classify_bakers
                    {
                      already_classified with
                      mute_bakers = baker :: already_classified.mute_bakers;
                    }
                    tl
            | Some 0. ->
                classify_bakers
                  {
                    already_classified with
                    dal_off = baker :: already_classified.dal_off;
                  }
                  tl
            | _ -> (
                match attest_infos.slot_attestation_rate with
                | None ->
                    classify_bakers
                      {
                        already_classified with
                        no_shards = baker :: already_classified.no_shards;
                      }
                      tl
                | Some 0. ->
                    classify_bakers
                      {
                        already_classified with
                        dal_zero = baker :: already_classified.dal_zero;
                      }
                      tl
                | _ ->
                    classify_bakers
                      {
                        already_classified with
                        dal_on = baker :: already_classified.dal_on;
                      }
                      tl))
      in
      let {mute_bakers; muted_by_dal; dal_zero; dal_on; no_shards; dal_off} =
        classify_bakers
          {
            mute_bakers = [];
            muted_by_dal = [];
            dal_zero = [];
            dal_on = [];
            no_shards = [];
            dal_off = [];
          }
          bakers_info
      in
      let ( >> ) cmp1 cmp2 x y =
        match cmp1 x y with 0 -> cmp2 x y | cmp -> cmp
      in
      let stake_descending x y =
        Float.compare y.stake_fraction x.stake_fraction
      in
      let attestation_rate_ascending x y =
        Option.compare
          Float.compare
          x.attest_infos.slot_attestation_rate
          y.attest_infos.slot_attestation_rate
      in
      let dal_mention_perf_ascending x y =
        Option.compare
          Float.compare
          x.attest_infos.dal_content_rate
          y.attest_infos.dal_content_rate
      in
      let mute_bakers = List.sort stake_descending mute_bakers in
      let muted_by_dal = List.sort stake_descending muted_by_dal in
      let dal_zero =
        List.sort (stake_descending >> dal_mention_perf_ascending) dal_zero
      in
      let no_shards =
        List.sort (stake_descending >> dal_mention_perf_ascending) no_shards
      in
      let dal_on =
        List.sort
          (attestation_rate_ascending >> dal_mention_perf_ascending
         >> stake_descending)
          dal_on
      in
      let dal_off = List.sort stake_descending dal_off in
      (* `group_by n l` outputs the list of lists with the same elements as `l`
         but with `n` elements per list (except the last one).
         For instance
         `group_by 4 [a_1, ..., a_10] = [[a_1, ..., a_4], [a_5, ..., a_8],  [a_9, a_10]]`
      *)
      let group_by n =
        let rec bis local_acc main_acc k = function
          | [] -> List.rev (List.rev local_acc :: main_acc)
          | l when k = 0 -> bis [] (List.rev local_acc :: main_acc) n l
          | hd :: tl -> bis (hd :: local_acc) main_acc (k - 1) tl
        in
        bis [] [] n
      in
      let agglomerate_infos bakers =
        let nb, stake =
          List.fold_left
            (fun (nb, stake_acc) {stake_fraction; _} ->
              (nb + 1, stake_acc +. stake_fraction))
            (0, 0.)
            bakers
        in
        Format.sprintf
          "They are %d representing %.2f%% of the stake"
          nb
          (stake *. 100.)
      in
      let encapsulate_in_code_block strings = ("```" :: strings) @ ["```"] in
      let display catch_phrase printer bakers =
        if bakers = [] then []
        else
          [catch_phrase; agglomerate_infos bakers]
          :: List.map
               encapsulate_in_code_block
               (group_by 20 (List.map (Format.asprintf "%a" printer) bakers))
      in
      let display_muted =
        display
          ":alert: *Those bakers never sent attestations.*"
          pp_baker_light
          mute_bakers
      in
      let display_muted_by_DAL =
        display
          ":alert: *Those bakers never sent attestation while in DAL \
           committee, however they sent attestations when they are out of it. \
           This is quite unexpected. They probably have an issue.*"
          pp_baker_light
          muted_by_dal
      in
      let display_zero =
        display
          ":triangular_flag_on_post: *Those bakers have a broken DAL node. \
           They send attestations with a DAL content, but do not attest any \
           slots.*"
          pp_baker_dal_status
          dal_zero
      in
      let display_on =
        display
          ":white_check_mark: *Those bakers sent attestations with a DAL \
           content.*"
          pp_bakers_all_infos
          dal_on
      in
      let display_off =
        display
          ":x: *Those bakers never turned their DAL on.*"
          pp_baker_light
          dal_off
      in
      let display_no_shards =
        display
          ":microscope: *Those bakers seems to have a working DAL node, but \
           never were in committee when slots were produced, hence we cannot \
           say much about how well it works.*"
          pp_baker_light
          no_shards
      in
      Lwt.return
        ((["Details of bakers performance:"] :: display_muted_by_DAL)
        @ display_zero @ display_on @ display_no_shards @ display_off
        @ display_muted)

    let fetch_slots_info () =
      let* data =
        (* fixme: should use protocol parameterized number of slots *)
        Lwt_list.map_p
          (fun slot_index -> fetch_slot_info ~slot_index)
          (List.init 32 Fun.id)
      in
      Lwt.return (view_slot_info data)

    let action ~slack_bot_token ~slack_channel_id ~configuration endpoint () =
      let* endpoint in
      let network = configuration.network in
      let title_info =
        Format.sprintf
          "*DAL report* for the *%s* network over the last %d hours."
          (String.capitalize_ascii (Network.to_string network))
          report_interval
      in
      let* ratio_dal_commitments_total_info, bandwidth_info =
        fetch_dal_commitments_total_info ()
      in
      let* slots_info = fetch_slots_info () in
      let network_overview_info =
        bandwidth_info :: ratio_dal_commitments_total_info :: slots_info
      in
      let data =
        let open Format_app in
        section [title_info] ()
        @ section ["*Network overview*"] ()
        @ section
            network_overview_info
            ~accessory:
              (Format_app.image
                 ~url:(network_to_image_url network)
                 ~alt:(Network.to_string network))
            ()
      in
      let* thread_id = post_message ~slack_channel_id ~slack_bot_token data in
      let* bakers_info = fetch_bakers_info endpoint in
      Lwt_list.iter_s
        (fun to_post ->
          let data =
            let open Format_app in
            section to_post ()
          in
          let* _ts =
            post_message ~ts:thread_id ~slack_channel_id ~slack_bot_token data
          in
          Lwt.return_unit)
        bakers_info

    (* Relies on UTC (Universal Time Coordinated).
       Paris operates on Central European Time (CET), which is UTC+1
       during standard time (winter months). During daylight saving
       time (summer months), Paris switches to Central European Summer
       Time (CEST), which is UTC+2.
    *)
    let register_chronos_task cloud ~configuration endpoint =
      match Cloud.notifier cloud with
      | Notifier_null -> ()
      | Notifier_slack {slack_bot_token; slack_channel_id; _} ->
          let task =
            let action () =
              action
                ~slack_bot_token
                ~slack_channel_id
                ~configuration
                endpoint
                ()
            in
            let tm = Format.sprintf "0 0-23/%d * * *" report_interval in
            Chronos.task ~name:"network-overview" ~tm ~action ()
          in
          Cloud.register_chronos_task cloud task
  end

  module Alert = struct
    let report_lost_dal_rewards ~slack_channel_id ~slack_bot_token ~network
        ~level ~cycle ~lost_dal_rewards =
      let data =
        let header =
          Format.sprintf
            "*[lost-dal-rewards]* On network `%s`, delegates have lost DAL \
             rewards at cycle `%d`, level `%d`. \
             <https://%s.tzkt.io/%d/implicit_operations/dal_attestation_reward \
             |See online>"
            (Network.to_string network)
            cycle
            level
            (Network.to_string network)
            level
        in
        let content =
          List.map
            (fun (`delegate delegate, `change change) ->
              Format.asprintf
                ":black_small_square: %a has missed ~%.1f tez DAL attestation \
                 rewards"
                pp_delegate
                delegate
                (float_of_int change /. 1_000_000.))
            lost_dal_rewards
        in
        Format_app.section (header :: content) ()
      in
      let* _ts = post_message ~slack_channel_id ~slack_bot_token data in
      Lwt.return_unit

    let check_for_lost_dal_rewards t ~metadata =
      match Cloud.notifier t.cloud with
      | Notifier_null -> unit
      | Notifier_slack {slack_channel_id; slack_bot_token; _} ->
          let cycle = JSON.(metadata |-> "level_info" |-> "cycle" |> as_int) in
          let level = JSON.(metadata |-> "level_info" |-> "level" |> as_int) in
          let balance_updates =
            JSON.(metadata |-> "balance_updates" |> as_list)
          in
          let lost_dal_rewards =
            List.filter_map
              (fun balance_update ->
                let category =
                  JSON.(balance_update |-> "category" |> as_string_opt)
                in
                match category with
                | None -> None
                | Some category ->
                    if String.equal category "lost DAL attesting rewards" then
                      let delegate =
                        JSON.(balance_update |-> "delegate" |> as_string)
                      in
                      let change =
                        JSON.(balance_update |-> "change" |> as_int)
                      in
                      Some (`delegate delegate, `change change)
                    else None)
              balance_updates
          in
          if List.is_empty lost_dal_rewards then unit
          else
            let network = t.configuration.network in
            report_lost_dal_rewards
              ~slack_channel_id
              ~slack_bot_token
              ~network
              ~level
              ~cycle
              ~lost_dal_rewards

    let check_for_lost_dal_rewards t ~metadata =
      Lwt.catch
        (fun () -> check_for_lost_dal_rewards t ~metadata)
        (fun exn ->
          Log.warn
            "Monitor_app.Alert.check_for_lost_dal_rewards: unexpected error: \
             '%s'"
            (Printexc.to_string exn) ;
          unit)

    let report_dal_accusations ~slack_channel_id ~slack_bot_token ~network
        ~level ~cycle dal_accusations =
      let data =
        let header =
          Format.sprintf
            "*[dal-accusations]* On network `%s`, delegates have been accused \
             at cycle `%d`, level `%d`."
            (Network.to_string network)
            cycle
            level
        in
        let content =
          List.map
            (fun ( `attestation_level attestation_level,
                   `slot_index slot_index,
                   `delegate delegate,
                   `op_hash hash ) ->
              Format.asprintf
                ":black_small_square: %a for attesting slot index `%d` at \
                 level `%d`. <https://%s.tzkt.io/%s|See online>"
                pp_delegate
                delegate
                slot_index
                attestation_level
                (Network.to_string network)
                hash)
            dal_accusations
        in
        Format_app.section (header :: content) ()
      in
      let* _ts = post_message ~slack_channel_id ~slack_bot_token data in
      Lwt.return_unit

    let check_for_dal_accusations t ~cycle ~level ~operations ~endpoint =
      match Cloud.notifier t.cloud with
      | Notifier_null -> unit
      | Notifier_slack {slack_channel_id; slack_bot_token; _} ->
          let open JSON in
          let accusations =
            operations |> as_list |> Fun.flip List.nth 2 |> as_list
          in
          let dal_entrapments =
            List.filter_map
              (fun accusation ->
                let contents =
                  accusation |-> "contents" |> as_list |> Fun.flip List.nth 0
                in
                match contents |-> "kind" |> as_string_opt with
                | Some "dal_entrapment_evidence" ->
                    let attestation_level =
                      contents |-> "attestation" |-> "operations" |-> "level"
                      |> as_int
                    in
                    let slot_index = contents |-> "slot_index" |> as_int in
                    let shard =
                      contents |-> "shard_with_proof" |-> "shard" |> as_list
                      |> Fun.flip List.nth 0 |> as_int
                    in
                    let hash = accusation |-> "hash" |> as_string in
                    Some
                      ( `attestation_level attestation_level,
                        `shard shard,
                        `slot_index slot_index,
                        `op_hash hash )
                | None | Some _ -> None)
              accusations
          in
          (* todo: optimize to avoid too many RPC calls (?) *)
          let* dal_accusations =
            Lwt_list.map_p
              (fun ( `attestation_level attestation_level,
                     `shard shard,
                     `slot_index slot_index,
                     `op_hash hash ) ->
                let* json =
                  RPC_core.call
                    endpoint
                    (RPC.get_chain_block_context_dal_shards
                       ~level:attestation_level
                       ())
                in
                let assignment =
                  let open JSON in
                  List.find_opt
                    (fun assignment ->
                      let indexes =
                        assignment |-> "indexes" |> as_list |> List.map as_int
                      in
                      List.mem shard indexes)
                    (json |> as_list)
                in
                let delegate =
                  Option.fold
                    ~none:"should_not_happen"
                    ~some:(fun x -> x |-> "delegate" |> as_string)
                    assignment
                in
                return
                  ( `attestation_level attestation_level,
                    `slot_index slot_index,
                    `delegate delegate,
                    `op_hash hash ))
              dal_entrapments
          in
          if List.is_empty dal_accusations then unit
          else
            let network = t.configuration.network in
            report_dal_accusations
              ~slack_channel_id
              ~slack_bot_token
              ~network
              ~cycle
              ~level
              dal_accusations

    let check_for_dal_accusations t ~cycle ~level ~operations ~endpoint =
      Lwt.catch
        (fun () ->
          check_for_dal_accusations t ~cycle ~level ~operations ~endpoint)
        (fun exn ->
          Log.warn
            "Monitor_app.Alert.check_for_dal_accusations: unexpected error: \
             '%s'"
            (Printexc.to_string exn) ;
          unit)

    type attestation_transition = Stopped_attesting | Restarted_attesting

    let report_attestation_transition ~slack_channel_id ~slack_bot_token
        ~network ~level ~pkh ~transition ~attestation_percentage =
      let data =
        let header =
          Format.asprintf
            (match transition with
            | Restarted_attesting ->
                "*[dal-attester-is-back]* On network `%s` at level `%d`, \
                 delegate `%a` who was under the reward threshold is again \
                 above threshold. New attestation rate is %.2f%%."
            | Stopped_attesting ->
                "*[dal-attester-dropped]* On network `%s` at level `%d`, \
                 delegate `%a` DAL attestation rate dropped under the reward \
                 threshold. New attestation rate is %.2f%%.")
            (Network.to_string network)
            level
            pp_delegate
            pkh
            attestation_percentage
        in
        Format_app.section [header] ()
      in
      let* _ts = post_message ~slack_channel_id ~slack_bot_token data in
      Lwt.return_unit

    let check_for_recently_missed_a_lot =
      (* TODO: This correspond to the number of bakers which activated DAL on mainnet.
         We expect more bakers to run the DAL, this initial size might be then increased
         to avoid rescaling of the hash table.
      *)
      let prev_was_enough = Hashtbl.create 50 in
      let to_treat_delegates = ref [] in
      fun t ~level ~metadata ->
        match Cloud.notifier t.cloud with
        | Notifier_null -> unit
        | Notifier_slack {slack_bot_token; slack_channel_id; _} -> (
            let cycle_position =
              JSON.(metadata |-> "level_info" |-> "cycle_position" |> as_int)
            in
            (* We do not run this function during the first 20 levels of a cycle,
               since the attestation rate are not relevant when so few levels happened.
               Especially since 0 attestation out of 0 attestable slots is considered
               as 100% attestation rate. *)
            if cycle_position < 20 then unit
            else
              let endpoint = t.some_node_rpc_endpoint in
              let treat_delegate pkh =
                let* infos =
                  RPC_core.call endpoint
                  @@ RPC.get_chain_block_context_delegate pkh
                in
                let dal_participation = JSON.(infos |-> "dal_participation") in
                let attest_enough =
                  JSON.(
                    dal_participation |-> "sufficient_dal_participation"
                    |> as_bool)
                in
                match Hashtbl.find_opt prev_was_enough pkh with
                | None ->
                    let () = Hashtbl.add prev_was_enough pkh attest_enough in
                    unit
                | Some prev_status ->
                    if prev_status = attest_enough then unit
                    else
                      let attestable =
                        JSON.(
                          dal_participation |-> "delegate_attestable_dal_slots"
                          |> as_float)
                      in
                      (* If no slots was attestatble, the value of [attest_enough]
                         is [true], but it is quite meaningless, since we do not want
                          to state that the DAL node is working well again simply
                          because there was nothing to attest. *)
                      if attestable = 0. then unit
                      else (
                        Hashtbl.add prev_was_enough pkh attest_enough ;
                        let network = t.configuration.network in
                        let attested =
                          JSON.(
                            dal_participation |-> "delegate_attested_dal_slots"
                            |> as_float)
                        in
                        let attestation_percentage =
                          100. *. attested /. attestable
                        in
                        let transition =
                          if attest_enough then Restarted_attesting
                          else Stopped_attesting
                        in
                        report_attestation_transition
                          ~slack_channel_id
                          ~slack_bot_token
                          ~network
                          ~level
                          ~pkh
                          ~transition
                          ~attestation_percentage)
              in
              let refill_delegates_to_treat () =
                let query_string = [("active", "true")] in
                let* delegates =
                  RPC_core.call endpoint
                  @@ RPC.get_chain_block_context_delegates ~query_string ()
                in
                to_treat_delegates := delegates ;
                unit
              in
              (* To avoid spawning a high number of RPCs simultaneously, we treat 2 delegates at each level. *)
              let to_treat_now, treat_later =
                Tezos_stdlib.TzList.split_n 2 !to_treat_delegates
              in
              let* () = Lwt_list.iter_p treat_delegate to_treat_now in
              match treat_later with
              | [] -> refill_delegates_to_treat ()
              | remaining_delegates ->
                  to_treat_delegates := remaining_delegates ;
                  unit)

    let check_for_recently_missed_a_lot t ~level ~metadata =
      Lwt.catch
        (fun () -> check_for_recently_missed_a_lot t ~level ~metadata)
        (fun exn ->
          Log.warn
            "Monitor_app.Alert.check_for_recently_missed_a_lot: unexpected \
             error: '%s'"
            (Printexc.to_string exn) ;
          unit)
  end
end

let get_infos_per_level t ~level ~metadata =
  let open Dal_node_helpers in
  let open Metrics in
  let client = t.bootstrap.client in
  let endpoint = t.some_node_rpc_endpoint in
  let etherlink_operators =
    match t.etherlink with
    | None -> []
    | Some setup -> setup.operator.account :: setup.operator.batching_operators
  in
  let cycle = JSON.(metadata |-> "level_info" |-> "cycle" |> as_int) in
  let* operations =
    let block = string_of_int level in
    RPC_core.call endpoint @@ RPC.get_chain_block_operations ~block ()
  in
  let attested_commitments =
    JSON.(metadata |-> "dal_attestation" |> as_string |> Z.of_string)
  in
  let manager_operation_batches = JSON.(operations |=> 3 |> as_list) in
  let is_published_commitment operation =
    JSON.(operation |-> "kind" |> as_string = "dal_publish_commitment")
  in
  let get_commitment operation =
    JSON.(operation |-> "slot_header" |-> "commitment" |> as_string)
  in
  let get_publisher operation = JSON.(operation |-> "source" |> as_string) in
  let commitment_info operation =
    let commitment = get_commitment operation in
    let publisher_pkh = get_publisher operation in
    {commitment; publisher_pkh}
  in
  let get_slot_index operation =
    JSON.(operation |-> "slot_header" |-> "slot_index" |> as_int)
  in
  let published_commitments =
    manager_operation_batches |> List.to_seq
    |> Seq.concat_map (fun batch ->
           JSON.(batch |-> "contents" |> as_list |> List.to_seq))
    |> Seq.filter is_published_commitment
    |> Seq.map (fun operation ->
           (get_slot_index operation, commitment_info operation))
    |> Hashtbl.of_seq
  in
  let consensus_operations = JSON.(operations |=> 0 |> as_list) in
  let get_dal_attestations operation =
    let contents = JSON.(operation |-> "contents" |=> 0) in
    let kind = JSON.(contents |-> "kind" |> as_string) in
    match kind with
    | "attestation_with_dal" ->
        let pkh = JSON.(contents |-> "metadata" |-> "delegate" |> as_string) in
        let slot = JSON.(contents |-> "slot" |> as_int) in
        let dal =
          if slot >= 512 then Out_of_committee
          else
            With_DAL
              JSON.(contents |-> "dal_attestation" |> as_string |> Z.of_string)
        in
        [(PKH pkh, dal)]
    | "attestations_aggregate" ->
        let metadata_committee =
          JSON.(contents |-> "metadata" |-> "committee" |> as_list)
        in
        let committee_info = JSON.(contents |-> "committee" |> as_list) in
        List.map2
          (fun member_info committee_meta ->
            let slot = JSON.(member_info |-> "slot" |> as_int) in
            let dal =
              if slot >= 512 then Out_of_committee
              else
                let json = JSON.(member_info |-> "dal_attestation") in
                if JSON.is_null json then Without_DAL
                else With_DAL (json |> JSON.as_string |> Z.of_string)
            in
            let pkh = JSON.(committee_meta |-> "delegate" |> as_string) in
            (PKH pkh, dal))
          committee_info
          metadata_committee
    | _ -> []
  in
  let* attestation_rights =
    RPC_core.call endpoint
    @@ RPC.get_chain_block_helper_attestation_rights ~level ()
  in
  (* We fill the [attestations] table with [Expected_to_DAL_attest] when a baker is in the DAL committee. *)
  let attestations =
    JSON.(attestation_rights |-> "delegates" |> as_list |> List.to_seq)
    |> Seq.filter_map (fun delegate ->
           let slot = JSON.(delegate |-> "first_slot" |> as_int) in
           if slot < 512 then
             let pkh = PKH JSON.(delegate |-> "delegate" |> as_string) in
             Some (pkh, Expected_to_DAL_attest)
           else None)
    |> Hashtbl.of_seq
  in
  (* And then update the [attestations] table with the attestations actually received. *)
  let () =
    consensus_operations
    |> List.iter (fun operation ->
           let dal_attestations = get_dal_attestations operation in
           List.iter
             (fun (pkh, dal_status) ->
               Hashtbl.replace attestations pkh dal_status)
             dal_attestations)
  in
  let* etherlink_operator_balance_sum =
    Etherlink_helpers.total_operator_balance
      ~client
      ~operators:etherlink_operators
  in
  (* None of these actions are performed if `--dal-slack-webhook` is
     not provided. *)
  let* () =
    if t.configuration.network_health_monitoring then
      let open Monitoring_app.Alert in
      let* () =
        check_for_dal_accusations
          t
          ~cycle
          ~level
          ~operations
          ~endpoint:t.some_node_rpc_endpoint
      in
      let* () = check_for_recently_missed_a_lot t ~level ~metadata in
      unit
    else Lwt.return_unit
  in
  Lwt.return
    {
      level;
      published_commitments;
      attestations;
      attested_commitments;
      etherlink_operator_balance_sum;
    }

let init_teztale (configuration : configuration) cloud agent =
  if configuration.teztale then init_teztale cloud agent |> Lwt.map Option.some
  else Lwt.return_none

let init_public_network cloud (configuration : configuration)
    etherlink_configuration teztale agent network =
  toplog "Init public network" ;
  let* bootstrap =
    match agent with
    | None ->
        let () = toplog "No agent given" in
        let node = None in
        let dal_node = None in
        let node_p2p_endpoint = Network.default_bootstrap network in
        let dal_node_p2p_endpoint =
          Some (Network.default_dal_bootstrap network)
        in
        let node_rpc_endpoint = Network.public_rpc_endpoint network in
        let () = toplog "Creating the client" in
        let client =
          Client.create ~endpoint:(Foreign_endpoint node_rpc_endpoint) ()
        in
        let bootstrap =
          {
            node;
            dal_node;
            node_p2p_endpoint;
            node_rpc_endpoint;
            dal_node_p2p_endpoint;
            client;
          }
        in
        Lwt.return bootstrap
    | Some agent ->
        let () = toplog "Some agent given (%s)" (Agent.name agent) in
        let () = toplog "Initializing the bootstrap node agent" in
        let with_yes_crypto =
          should_enable_yes_crypto configuration.simulate_network
        in
        let* node =
          Node_helpers.init
            ?identity_file:configuration.bootstrap_node_identity_file
            ~rpc_external:configuration.external_rpc
            ~name:"bootstrap-node"
            configuration.network
            ~with_yes_crypto
            ~snapshot:configuration.snapshot
            ~ppx_profiling:configuration.ppx_profiling
            cloud
            agent
        in
        let* dal_node =
          if not configuration.with_dal then Lwt.return_none
          else
            let disable_shard_validation =
              configuration.disable_shard_validation
            in
            let* dal_node =
              Dal_node.Agent.create
                ~name:"bootstrap-dal-node"
                cloud
                agent
                ~node
                ~disable_shard_validation
            in
            let* () =
              Dal_node.init_config
                ~expected_pow:26.
                ~bootstrap_profile:true
                dal_node
            in
            let* () =
              Dal_node_helpers.may_copy_dal_node_identity_file
                agent
                dal_node
                configuration.bootstrap_dal_node_identity_file
            in
            let* () = Node.wait_for_ready node in
            let otel = Cloud.open_telemetry_endpoint cloud in
            let* () =
              Dal_node.Agent.run
                ~prometheus:Tezt_cloud_cli.prometheus
                ?otel
                ~memtrace:configuration.memtrace
                ~event_level:`Notice
                ~disable_shard_validation
                ~ppx_profiling:configuration.ppx_profiling
                ~ppx_profiling_backends:configuration.ppx_profiling_backends
                dal_node
            in
            Lwt.return_some dal_node
        in
        let* () =
          add_prometheus_source cloud agent "bootstrap" ?dal_node ~node
        in
        let client = Client.create ~endpoint:(Node node) () in
        let node_rpc_endpoint =
          Endpoint.make
            ~scheme:"http"
            ~host:
              (match Agent.point agent with
              | None -> "127.0.0.1"
              | Some point -> fst point)
            ~port:(Node.rpc_port node)
            ()
        in
        let bootstrap =
          {
            node = Some node;
            dal_node;
            node_p2p_endpoint = Node.point_str node;
            node_rpc_endpoint;
            dal_node_p2p_endpoint = Option.map Dal_node.point_str dal_node;
            client;
          }
        in
        let* () =
          match teztale with
          | None -> Lwt.return_unit
          | Some teztale ->
              Teztale.add_archiver
                teztale
                cloud
                agent
                ~node_name:(Node.name node)
                ~node_port:(Node.rpc_port node)
        in
        Lwt.return bootstrap
  in
  let () = toplog "Initializing the bakers" in
  let* stake = configuration.stake in
  let* baker_accounts =
    Lwt_list.mapi_s
      (fun i _stake ->
        let* delegates =
          (* We assume that a baker holds only one key. *)
          Client.stresstest_gen_keys
            ~alias_prefix:(Format.sprintf "baker-%d" i)
            1
            bootstrap.client
        in
        List.map
          (fun delegate -> Baker_helpers.{delegate; consensus_key = None})
          delegates
        |> return)
      stake
  in
  let* producer_accounts =
    Dal_node_helpers.init_producer_accounts
      ~client:bootstrap.client
      ~producer_key:configuration.producer_key
      ~dal_node_producers:configuration.dal_node_producers
  in
  let* etherlink_rollup_operator_key, etherlink_batching_operator_keys =
    Etherlink_helpers.init_etherlink_operators
      ~client:bootstrap.client
      etherlink_configuration
  in
  let* echo_rollup_key =
    Echo_rollup.init_echo_rollup_account
      ~client:bootstrap.client
      ~echo_rollup:configuration.echo_rollup
      ~alias_prefix:"echo_operator"
  in
  let accounts_to_fund =
    (if configuration.producer_key = None then
       List.map (fun producer -> (producer, 10 * 1_000_000)) producer_accounts
     else
       (* When a producer key has been given, it is assumed to have money and should not be funded. *)
       [])
    @ List.map
        (fun operator -> (operator, 11_000 * 1_000_000))
        etherlink_rollup_operator_key
    @ List.map
        (fun batcher -> (batcher, 10 * 1_000_000))
        etherlink_batching_operator_keys
    @ Option.fold
        ~none:[]
        ~some:(fun operator -> [(operator, 11_000 * 1_000_000)])
        echo_rollup_key
  in
  let* () =
    Dal_node_helpers.fund_producers_accounts
      ~client:bootstrap.client
      ~fundraiser:configuration.fundraiser
      accounts_to_fund
  in
  let etherlink_rollup_operator_key =
    match etherlink_rollup_operator_key with key :: _ -> Some key | [] -> None
  in
  Lwt.return
    ( bootstrap,
      baker_accounts,
      producer_accounts,
      etherlink_rollup_operator_key,
      etherlink_batching_operator_keys,
      echo_rollup_key )

let round_robin_split m lst =
  assert (m > 0) ;
  let buckets = Array.make m [] in
  List.iteri
    (fun idx x ->
      let bucket = idx mod m in
      buckets.(bucket) <- x :: buckets.(bucket))
    (List.rev lst) ;
  Array.to_list buckets |> List.rev

let init_sandbox_and_activate_protocol cloud (configuration : configuration)
    ?(etherlink_configuration :
       Etherlink_helpers.etherlink_configuration option) agent =
  let dal_bootstrap_node_net_port = Agent.next_available_port agent in
  let dal_config : Cryptobox.Config.t =
    {
      activated = true;
      bootstrap_peers =
        [Format.asprintf "127.0.0.1:%d" dal_bootstrap_node_net_port];
    }
  in
  let name = "bootstrap-node" in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let env, with_yes_crypto =
    may_set_yes_crypto_env configuration.simulate_network
  in
  let* bootstrap_node =
    Node_helpers.init
      ?env
      ?data_dir
      ?identity_file:configuration.bootstrap_node_identity_file
      ~rpc_external:configuration.external_rpc
      ~dal_config
      ~name
      configuration.network
      ~with_yes_crypto
      ~snapshot:configuration.snapshot
      ~ppx_profiling:configuration.ppx_profiling
      cloud
      agent
  in
  let* () = Node.wait_for_ready bootstrap_node in
  let* () = init_explorus cloud bootstrap_node in
  let* dal_bootstrap_node =
    if configuration.with_dal then
      let* dal_node =
        Dal_node.Agent.create
          ~net_port:dal_bootstrap_node_net_port
          ~name:"bootstrap-dal-node"
          cloud
          agent
          ~node:bootstrap_node
          ~disable_shard_validation:configuration.disable_shard_validation
      in
      Lwt.return_some dal_node
    else Lwt.return_none
  in
  let* stake = configuration.stake in
  let* client =
    if configuration.simulate_network = Disabled then
      Client.init ~endpoint:(Node bootstrap_node) ()
    else
      let* client =
        Client.Agent.create
          ~name:(Tezt_cloud.Agent.name agent ^ "-client")
          ~endpoint:(Client.Node bootstrap_node)
          agent
      in
      let* yes_wallet = Node_helpers.yes_wallet agent in
      let* snapshot_network =
        match configuration.snapshot with
        | Docker_embedded path | Local_file path ->
            let* network =
              Snapshot_helpers.get_snapshot_info_network bootstrap_node path
            in
            (* Yes-wallet requires the config url for protocol-specific test
               networks.*)
            let network =
              match network with
              | ("ghostnet" | "mainnet" | "sandbox") as s -> s
              | s ->
                  (* We assume that unknown networks are protocol specific ones. *)
                  "https://teztnets.com/" ^ s
            in
            Lwt.return network
        | Url _url ->
            (* FIXME: We can overcome this by either downloading the snapshot, or by
               retrieving the name of the network from the URL. I am not sure how much
               of a priority this is. *)
            Lwt.fail_with "URL snapshot not available for sandbox case"
        | No_snapshot -> Lwt.return "ghostnet"
      in
      let* _filename =
        Yes_wallet.create_from_context
          ~node:bootstrap_node
          ~client
          ~network:snapshot_network
          yes_wallet
      in
      return client
  in
  (* [delegate_accounts] stands for the list of delegates keys (registered
     baking account) that will be used by the producers daemons. These producers
     need accounts with funds in order to inject DAL publish commitment
     operations. *)
  let* delegate_accounts =
    match configuration.simulate_network with
    | Scatter (selected_bakers_count, baker_daemons_count) ->
        let* all_delegates_aliases =
          Client.list_known_addresses client |> Lwt.map (List.map fst)
        in
        let selected_delegates =
          Tezos_stdlib.TzList.take_n selected_bakers_count all_delegates_aliases
        in
        let* delegates =
          Lwt_list.map_s
            (fun alias -> Client.show_address ~alias client)
            selected_delegates
        in
        round_robin_split baker_daemons_count delegates |> Lwt.return
    | Map
        ( selected_bakers_count,
          single_key_baker_daemons_count,
          multiple_keys_baker_daemons_count ) ->
        let* all_delegates_aliases =
          Client.list_known_addresses client |> Lwt.map (List.map fst)
        in
        let selected_delegates_aliases =
          Tezos_stdlib.TzList.take_n selected_bakers_count all_delegates_aliases
        in
        let single_key_bakers_aliases, remaining_baker_aliases =
          Tezos_stdlib.TzList.split_n
            single_key_baker_daemons_count
            selected_delegates_aliases
        in
        let* single_key_bakers =
          Lwt_list.map_s
            (fun alias ->
              let* a = Client.show_address ~alias client in
              Lwt.return [a])
            single_key_bakers_aliases
        in
        let* remaining_bakers =
          let* r =
            Lwt_list.map_s
              (fun alias -> Client.show_address ~alias client)
              remaining_baker_aliases
          in
          round_robin_split multiple_keys_baker_daemons_count r |> Lwt.return
        in
        Lwt.return (single_key_bakers @ remaining_bakers)
    | Disabled ->
        Lwt_list.mapi_s
          (fun i _stake ->
            (* We assume that a baker holds only one key. *)
            Client.stresstest_gen_keys
              ~alias_prefix:(Format.sprintf "baker-%d" i)
              1
              client)
          stake
  in
  (* [baker_accounts] stands for the list of [baker_account] that are actually
     used for baking. Meaning that if a baker uses a consensus key, the
     [baker_account.consensus_key] will hold the associated consensus key. *)
  let* baker_accounts =
    match configuration.simulate_network with
    | Disabled ->
        (* Generated baker accounts are not using any consensus key. *)
        List.map
          (fun l ->
            List.map
              (fun delegate -> Baker_helpers.{delegate; consensus_key = None})
              l)
          delegate_accounts
        |> return
    | Map _ | Scatter _ ->
        (* Substitute consensus pkh with delegate pkh *)
        let* yw = Node_helpers.yes_wallet agent in
        let* ckm = Yes_wallet.load_consensus_key_mapping yw ~client in
        List.map
          (fun l ->
            List.map
              (fun (delegate : Account.key) ->
                try
                  let ck =
                    List.find
                      (fun {Yes_wallet.public_key_hash; _} ->
                        public_key_hash = delegate.public_key_hash)
                      ckm
                  in
                  let consensus_key =
                    Some
                      {
                        Account.alias = delegate.alias;
                        public_key_hash = ck.consensus_public_key_hash;
                        public_key = ck.consensus_public_key;
                        secret_key =
                          (* That's ok, because we're using yes-crypto. *)
                          Account.Unencrypted
                            Tezos_crypto.Signature.(to_b58check zero);
                      }
                  in
                  Baker_helpers.{delegate; consensus_key}
                with Not_found -> {delegate; consensus_key = None})
              l)
          delegate_accounts
        |> return
  in
  List.iteri
    (fun i l ->
      toplog
        "Baker agent %d will run for the following delegates: %a"
        i
        (Format.pp_print_list
           ~pp_sep:(fun out () -> Format.fprintf out ",")
           (fun fmt Baker_helpers.{delegate; _} ->
             Format.fprintf fmt "%s" delegate.alias))
        l)
    baker_accounts ;
  let* producer_accounts =
    match configuration.simulate_network with
    | Scatter _ | Map _ ->
        Tezos_stdlib.TzList.take_n
          (List.length configuration.dal_node_producers)
          (List.flatten delegate_accounts)
        |> return
    | Disabled ->
        Client.stresstest_gen_keys
          ~alias_prefix:"dal_producer"
          (List.length configuration.dal_node_producers)
          client
  in
  let* etherlink_rollup_operator_key, etherlink_batching_operator_keys =
    Etherlink_helpers.init_etherlink_operators ~client etherlink_configuration
  in
  let* echo_rollup_key =
    Echo_rollup.init_echo_rollup_account
      ~client
      ~echo_rollup:configuration.echo_rollup
      ~alias_prefix:"echo_rollup_key"
  in
  let* () =
    if configuration.simulate_network = Disabled then
      let* parameter_file =
        let base =
          Either.right (configuration.protocol, Some Protocol.Constants_mainnet)
        in
        let bootstrap_accounts =
          List.mapi
            (fun i Baker_helpers.{delegate; _} ->
              (delegate, Some (List.nth stake i * 1_000_000_000_000)))
            (List.flatten baker_accounts)
        in
        let additional_bootstrap_accounts =
          List.map
            (fun key -> (key, Some 1_000_000_000_000, false))
            (producer_accounts @ etherlink_rollup_operator_key
           @ etherlink_batching_operator_keys
            @ Option.fold ~none:[] ~some:(fun k -> [k]) echo_rollup_key)
        in
        let overrides = [] in
        Protocol.write_parameter_file
          ~bootstrap_accounts
          ~additional_bootstrap_accounts
          ~base
          overrides
      in
      let* () =
        Client.activate_protocol_and_wait
          ~timestamp:Client.Now
          ~parameter_file
          ~protocol:configuration.protocol
          client
      in
      Lwt.return_unit
    else Lwt.return_unit
  in
  let etherlink_rollup_operator_key =
    match etherlink_rollup_operator_key with [key] -> Some key | _ -> None
  in
  let* () =
    match dal_bootstrap_node with
    | None -> Lwt.return_unit
    | Some dal_bootstrap_node ->
        let* () =
          Dal_node.init_config
            ~expected_pow:0.
            ~bootstrap_profile:true
            dal_bootstrap_node
        in
        let otel = Cloud.open_telemetry_endpoint cloud in
        let* () =
          Dal_node_helpers.may_copy_dal_node_identity_file
            agent
            dal_bootstrap_node
            configuration.bootstrap_dal_node_identity_file
        in
        Dal_node.Agent.run
          ~prometheus:Tezt_cloud_cli.prometheus
          ?otel
          ~memtrace:configuration.memtrace
          ~event_level:`Notice
          ~disable_shard_validation:configuration.disable_shard_validation
          ~ppx_profiling:configuration.ppx_profiling
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          dal_bootstrap_node
  in
  let* () =
    add_prometheus_source
      ~node:bootstrap_node
      ?dal_node:dal_bootstrap_node
      cloud
      agent
      "bootstrap"
  in
  let node_rpc_endpoint =
    Endpoint.make
      ~scheme:"http"
      ~host:
        (match Agent.point agent with
        | None -> "127.0.0.1"
        | Some point -> fst point)
      ~port:(Node.rpc_port bootstrap_node)
      ()
  in
  let (bootstrap : bootstrap) =
    {
      node = Some bootstrap_node;
      dal_node = dal_bootstrap_node;
      node_p2p_endpoint = Node.point_str bootstrap_node;
      node_rpc_endpoint;
      dal_node_p2p_endpoint = Option.map Dal_node.point_str dal_bootstrap_node;
      client;
    }
  in
  Lwt.return
    ( bootstrap,
      baker_accounts,
      producer_accounts,
      etherlink_rollup_operator_key,
      etherlink_batching_operator_keys,
      echo_rollup_key )

let obtain_some_node_rpc_endpoint agent network (bootstrap : bootstrap)
    (bakers : Baker_helpers.baker list)
    (producers : Dal_node_helpers.producer list)
    (observers : Dal_node_helpers.observer list) etherlink =
  match (agent, network) with
  | None, #Network.public -> (
      match (bakers, producers, observers, etherlink) with
      | baker :: _, _, _, _ -> Node.as_rpc_endpoint baker.node
      | [], producer :: _, _, _ -> Node.as_rpc_endpoint producer.node
      | [], [], observer :: _, _ -> Node.as_rpc_endpoint observer.node
      | [], [], [], Some etherlink ->
          Node.as_rpc_endpoint etherlink.Etherlink_helpers.operator.node
      | [], [], [], None -> bootstrap.node_rpc_endpoint)
  | _ -> bootstrap.node_rpc_endpoint

let init ~(configuration : configuration) etherlink_configuration cloud
    next_agent =
  let () = toplog "Init" in
  let () = toplog "Agents preparation" in
  let* bootstrap_agent =
    if configuration.bootstrap then
      let name = name_of Bootstrap in
      let* agent = next_agent ~name in
      Lwt.return_some agent
    else Lwt.return_none
  in
  let* producers_agents =
    Lwt_list.map_s
      (fun slot_index ->
        let name = name_of (Producer slot_index) in
        let* agent = next_agent ~name in
        return (agent, slot_index))
      configuration.dal_node_producers
  in
  let* observers_slot_index_agents =
    Lwt_list.map_s
      (fun slot_index ->
        let name = name_of (Observer (`Index slot_index)) in
        let* agent = next_agent ~name in
        return (`Slot_index slot_index, agent))
      configuration.observer_slot_indices
  in
  let* observers_bakers_agents =
    Lwt_list.map_s
      (fun pkh ->
        let name = name_of (Observer (`Pkh pkh)) in
        let* agent = next_agent ~name in
        return (`Pkh pkh, agent))
      configuration.observer_pkhs
  in
  let* teztale =
    match bootstrap_agent with
    | None -> Lwt.return_none
    | Some agent -> init_teztale configuration cloud agent
  in
  let* ( bootstrap,
         baker_accounts,
         producer_accounts,
         etherlink_rollup_operator_key,
         etherlink_batching_operator_keys,
         echo_rollup_key ) =
    match configuration.network with
    | `Sandbox ->
        let bootstrap_agent = Option.get bootstrap_agent in
        init_sandbox_and_activate_protocol
          cloud
          configuration
          ?etherlink_configuration
          bootstrap_agent
    | #Network.public ->
        let network = Network.to_public configuration.network in
        init_public_network
          cloud
          configuration
          etherlink_configuration
          teztale
          bootstrap_agent
          network
  in
  let* bakers =
    Baker_helpers.init_bakers
      ~bakers:configuration.bakers
      ~stake:configuration.stake
      ~data_dir:configuration.data_dir
      ~simulate_network:configuration.simulate_network
      ~external_rpc:configuration.external_rpc
      ~network:configuration.network
      ~snapshot:configuration.snapshot
      ~ppx_profiling:configuration.ppx_profiling
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~memtrace:configuration.memtrace
      ~with_dal:configuration.with_dal
      ~disable_shard_validation:configuration.disable_shard_validation
      ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
      ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
      cloud
      teztale
      ~baker_accounts
      next_agent
  in
  let () = toplog "Init: initializing producers and observers" in
  let* producers =
    Lwt_list.mapi_p
      (fun i ((agent, slot_index), account) ->
        Dal_node_helpers.init_producer
          cloud
          ~data_dir:configuration.data_dir
          ~simulate_network:configuration.simulate_network
          ~external_rpc:configuration.external_rpc
          ~network:configuration.network
          ~snapshot:configuration.snapshot
          ~memtrace:configuration.memtrace
          ~ppx_profiling:configuration.ppx_profiling
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          ~ignore_pkhs:configuration.ignore_pkhs
          ~disable_shard_validation:configuration.disable_shard_validation
          ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
          ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
          teztale
          account
          i
          slot_index
          agent)
      (List.combine producers_agents producer_accounts)
  and* observers =
    Lwt_list.mapi_p
      (fun i (topic, agent) ->
        Dal_node_helpers.init_observer
          cloud
          ~data_dir:configuration.data_dir
          ~simulate_network:configuration.simulate_network
          ~external_rpc:configuration.external_rpc
          ~network:configuration.network
          ~snapshot:configuration.snapshot
          ~memtrace:configuration.memtrace
          ~ppx_profiling:configuration.ppx_profiling
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          ~disable_shard_validation:configuration.disable_shard_validation
          ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
          ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
          teztale
          ~topic
          i
          agent)
      (observers_slot_index_agents @ observers_bakers_agents)
  in
  let () = toplog "Init: all producers and observers have been initialized" in
  let* echo_rollup =
    Echo_rollup.init_echo_rollup
      cloud
      ~data_dir:configuration.data_dir
      ~simulate_network:configuration.simulate_network
      ~external_rpc:configuration.external_rpc
      ~network:configuration.network
      ~snapshot:configuration.snapshot
      ~ppx_profiling:configuration.ppx_profiling
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~memtrace:configuration.memtrace
      ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
      ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
      ~next_agent
      producers
      echo_rollup_key
  in
  let* etherlink =
    Etherlink_helpers.init_etherlink
      ~data_dir:configuration.data_dir
      ~simulate_network:configuration.simulate_network
      ~external_rpc:configuration.external_rpc
      ~network:configuration.network
      ~snapshot:configuration.snapshot
      ~ppx_profiling:configuration.ppx_profiling
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~memtrace:configuration.memtrace
      ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
      ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
      ~next_agent
      ~cloud
      etherlink_rollup_operator_key
      etherlink_batching_operator_keys
      etherlink_configuration
  in
  let some_node_rpc_endpoint =
    obtain_some_node_rpc_endpoint
      bootstrap_agent
      configuration.network
      bootstrap
      bakers
      producers
      observers
      etherlink
  in
  let* constants =
    RPC_core.call
      some_node_rpc_endpoint
      (RPC.get_chain_block_context_constants_parametric ())
  in
  let time_between_blocks =
    JSON.(constants |-> "minimal_block_delay" |> as_int)
  in
  let* parameters =
    Dal_common.Parameters.from_endpoint some_node_rpc_endpoint
  in
  let infos = Hashtbl.create 101 in
  let metrics = Hashtbl.create 101 in
  let* first_level =
    match configuration.network with
    | `Sandbox -> (
        match configuration.simulate_network with
        | Disabled -> Lwt.return 1
        | Scatter _ | Map _ -> Network.get_level some_node_rpc_endpoint)
    | _ -> Network.get_level some_node_rpc_endpoint
  in
  Hashtbl.replace metrics first_level Metrics.default ;
  let disconnection_state =
    Option.map Disconnect.init configuration.disconnect
  in
  let* init_aliases =
    let accounts =
      let open Baker_helpers in
      List.concat_map
        (fun ({accounts; _} : baker) ->
          List.map (fun {delegate; _} -> delegate) accounts)
        bakers
    in
    Network.aliases ~accounts configuration.network
  in
  let* versions = Network.versions configuration.network in
  Metrics.merge_aliases init_aliases ;
  let versions = Option.value ~default:(Hashtbl.create 0) versions in
  let otel = Cloud.open_telemetry_endpoint cloud in
  (* Adds monitoring for all agents for octez-dal-node and octez-node
     TODO: monitor only specific agents for specific binaries *)
  Lwt.return
    {
      cloud;
      configuration;
      bootstrap;
      some_node_rpc_endpoint;
      bakers;
      producers;
      observers;
      echo_rollup;
      etherlink;
      time_between_blocks;
      parameters;
      infos;
      metrics;
      disconnection_state;
      first_level;
      teztale;
      versions;
      otel;
    }

let wait_for_level t level =
  match t.bootstrap.node with
  | None ->
      let rec loop () =
        let* head_level = Network.get_level t.some_node_rpc_endpoint in
        if head_level >= level then Lwt.return_unit
        else
          let* () = Lwt_unix.sleep 4. in
          loop ()
      in
      loop ()
  | Some node ->
      let* _ = Node.wait_for_level node level in
      Lwt.return_unit

let clean_up t level =
  Hashtbl.remove t.infos level ;
  Hashtbl.remove t.metrics level

let update_bakers_infos t =
  let open Baker_helpers in
  let* new_aliases =
    let accounts =
      List.(
        concat_map
          (fun ({accounts; _} : baker) ->
            List.map (fun {delegate; _} -> delegate) accounts)
          t.bakers)
    in
    Network.aliases ~accounts t.configuration.network
  in
  let* versions = Network.versions t.configuration.network in
  Metrics.merge_aliases new_aliases ;
  let versions = Option.value ~default:t.versions versions in
  t.versions <- versions ;
  Lwt.return_unit

let on_new_level t level ~metadata =
  toplog "Start processing level %d" level ;
  clean_up t (level - t.configuration.blocks_history) ;
  let* () =
    if level mod 1_000 = 0 then update_bakers_infos t else Lwt.return_unit
  in
  let* infos_per_level = get_infos_per_level t ~level ~metadata in
  toplog "Level %d's info processed" level ;
  Hashtbl.replace t.infos level infos_per_level ;
  let metrics =
    Metrics.get
      ~first_level:t.first_level
      ~attestation_lag:t.parameters.attestation_lag
      ~dal_node_producers:t.configuration.dal_node_producers
      ~number_of_slots:t.parameters.number_of_slots
      ~infos:t.infos
      infos_per_level
      (Hashtbl.find t.metrics (level - 1))
  in
  Metrics.pp ~bakers:t.bakers metrics ;
  Metrics.push ~versions:t.versions ~cloud:t.cloud metrics ;
  Hashtbl.replace t.metrics level metrics ;
  let* t =
    match t.disconnection_state with
    | Some disconnection_state when t.configuration.with_dal ->
        let nb_bakers = List.length t.bakers in
        let* disconnection_state =
          Disconnect.disconnect disconnection_state level (fun b ->
              let baker_to_disconnect = List.nth t.bakers (b mod nb_bakers) in
              (* Invariant: Option.get don't fail because t.configuration.dal is true *)
              let dal_node = baker_to_disconnect.dal_node |> Option.get in
              Dal_node.Agent.terminate dal_node)
        in
        let* disconnection_state =
          Disconnect.reconnect disconnection_state level (fun b ->
              let baker_to_reconnect = List.nth t.bakers (b mod nb_bakers) in
              (* Invariant: Option.get don't fail because t.configuration.dal is true *)
              let dal_node = baker_to_reconnect.dal_node |> Option.get in
              Dal_node.Agent.run
                ~prometheus:Tezt_cloud_cli.prometheus
                ?otel:t.otel
                ~memtrace:t.configuration.memtrace
                ~ppx_profiling:t.configuration.ppx_profiling
                ~ppx_profiling_backends:t.configuration.ppx_profiling_backends
                dal_node)
        in
        Lwt.return {t with disconnection_state = Some disconnection_state}
    | _ -> Lwt.return t
  in
  toplog "Level %d processed" level ;
  Lwt.return t

let on_new_cycle t ~level =
  let endpoint = t.some_node_rpc_endpoint in
  let last_block_of_prev_cycle = string_of_int (level - 1) in
  let* metadata =
    RPC_core.call endpoint
    @@ RPC.get_chain_block_metadata_raw ~block:last_block_of_prev_cycle ()
  in
  (* This action is performed only if `--dal-slack-webhook` is provided. *)
  if t.configuration.network_health_monitoring then
    Monitoring_app.Alert.check_for_lost_dal_rewards t ~metadata
  else Lwt.return_unit

let on_new_block t ~level =
  let* () = wait_for_level t level in
  let endpoint = t.some_node_rpc_endpoint in
  let block = string_of_int level in
  let* metadata =
    RPC_core.call endpoint @@ RPC.get_chain_block_metadata_raw ~block ()
  in
  let cycle_position =
    JSON.(metadata |-> "level_info" |-> "cycle_position" |> as_int)
  in
  let is_new_cycle = cycle_position = 0 in
  let* () = if is_new_cycle then on_new_cycle t ~level else unit in
  on_new_level t level ~metadata

let rec loop t level =
  let p = on_new_block t ~level in
  let _p2 =
    if Dal_node_helpers.producers_not_ready ~producers:t.producers then (
      toplog "Producers not ready for level %d" level ;
      Lwt.return_unit)
    else
      Seq.ints 0
      |> Seq.take (List.length t.configuration.dal_node_producers)
      |> Seq.map (fun i ->
             Dal_node_helpers.produce_slot
               ~client:t.bootstrap.client
               ~producers:t.producers
               ~network:t.configuration.network
               ~producer_key:t.configuration.producer_key
               ~some_node_rpc_endpoint:t.some_node_rpc_endpoint
               ~producers_delay:t.configuration.producers_delay
               ~slot_size:t.parameters.cryptobox.slot_size
               level
               i)
      |> List.of_seq |> Lwt.join
  in
  let* t = p in
  loop t (level + 1)

let yes_wallet_exe = Uses.path Constant.yes_wallet

let parse_stake_arg ~stake_arg ~simulation_arg =
  let open Network in
  match simulation_arg with
  | Cli.Disabled -> (
      match stake_arg with
      | Custom distrib -> return distrib
      | Mimic {network; max_nb_bakers} ->
          let network_string =
            match network with
            | `Mainnet | `Ghostnet | `Rionet | `Seoulnet -> to_string network
            | _ ->
                failwith
                  (Format.sprintf
                     "Cannot get stake distribution for %s"
                     (to_string network))
          in
          let endpoint =
            Endpoint.make ~host:"rpc.tzkt.io" ~scheme:"https" ~port:443 ()
          in
          let decoder json = JSON.(json |-> "cycle" |> as_int) in
          let rpc =
            RPC_core.(
              make
                GET
                [
                  network_string;
                  "chains";
                  "main";
                  "blocks";
                  "head";
                  "helpers";
                  "current_level";
                ]
                decoder)
          in
          let* response = RPC_core.call_raw endpoint rpc in
          let cycle =
            RPC_core.decode_raw ~origin:"Network.cycle" rpc response.body
          in
          let get_stake_in_ktez stake =
            JSON.(
              (stake |-> "frozen" |> as_int) + (stake |-> "delegated" |> as_int))
            / 1_000_000_000
          in
          let decoder json =
            json |> JSON.as_list
            |> List.map (fun json_account ->
                   let active_stake = JSON.(json_account |-> "active_stake") in
                   get_stake_in_ktez active_stake)
          in
          let rpc =
            RPC_core.(
              make
                GET
                [
                  network_string;
                  "chains";
                  "main";
                  "blocks";
                  "head";
                  "context";
                  "raw";
                  "json";
                  "cycle";
                  string_of_int cycle;
                  "selected_stake_distribution";
                ]
                decoder)
          in
          let* response = RPC_core.call_raw endpoint rpc in
          let distribution =
            RPC_core.decode_raw ~origin:"Network.cycle" rpc response.body
          in
          let distribution =
            match max_nb_bakers with
            | None -> distribution
            | Some n -> Tezos_stdlib.TzList.take_n n distribution
          in
          return distribution)
  | Scatter _ | Map _ -> (
      match stake_arg with
      | Custom [] ->
          (* As simulate_network and stake are mutually exclusive, only empty
             stake option is allowed. *)
          Lwt.return []
      | _ ->
          Test.fail
            "Options --simulate and --stake are mutually exclusive. We cannot \
             set baker stake while using baking power of bakers from a \
             simulated network.")

let register (module Cli : Scenarios_cli.Dal) =
  let simulate_network = Cli.simulate_network in
  let stake =
    parse_stake_arg ~stake_arg:Cli.stake ~simulation_arg:simulate_network
  in
  let configuration, etherlink_configuration =
    let stake_machine_type = Cli.stake_machine_type in
    let dal_node_producers =
      let last_index = ref (-1) in
      List.init Cli.producers (fun i ->
          match List.nth_opt Cli.dal_producers_slot_indices i with
          | None ->
              incr last_index ;
              !last_index
          | Some index ->
              last_index := index ;
              index)
    in
    let observer_slot_indices = Cli.observer_slot_indices in
    let observer_pkhs = Cli.observer_pkhs in
    let protocol = Cli.protocol in
    let producer_machine_type = Cli.producer_machine_type in
    let etherlink = Cli.etherlink in
    let etherlink_sequencer = Cli.etherlink_sequencer in
    let etherlink_producers = Cli.etherlink_producers in
    let echo_rollup = Cli.echo_rollup in
    let disconnect = Cli.disconnect in
    let network = Cli.network in
    let snapshot = Cli.snapshot in
    let bootstrap = Cli.bootstrap in
    let etherlink_dal_slots = Cli.etherlink_dal_slots in
    let teztale = Cli.teztale in
    let memtrace = Cli.memtrace in
    let data_dir = Cli.data_dir in
    let producer_key = Cli.producer_key in
    let producers_delay = Cli.producers_delay in
    let ignore_pkhs = Cli.ignore_pkhs in
    let tezlink = Cli.tezlink in
    let fundraiser =
      Option.fold
        ~none:(Sys.getenv_opt "TEZT_CLOUD_FUNDRAISER")
        ~some:Option.some
        Cli.fundraiser
    in
    let etherlink_chain_id = Cli.etherlink_chain_id in
    let etherlink =
      if etherlink then
        Some
          Etherlink_helpers.
            {
              etherlink_sequencer;
              etherlink_producers;
              etherlink_dal_slots;
              chain_id = etherlink_chain_id;
              tezlink;
            }
      else None
    in
    let blocks_history = Cli.blocks_history in
    let bootstrap_node_identity_file = Cli.bootstrap_node_identity_file in
    let bootstrap_dal_node_identity_file =
      Cli.bootstrap_dal_node_identity_file
    in
    let with_dal = Cli.with_dal in
    let bakers = Cli.bakers in
    let external_rpc = Cli.node_external_rpc_server in
    let disable_shard_validation = Cli.disable_shard_validation in
    let ppx_profiling = Cli.ppx_profiling in
    let ppx_profiling_backends = Cli.ppx_profiling_backends in
    let network_health_monitoring = Cli.enable_network_health_monitoring in
    let t =
      {
        with_dal;
        bakers;
        stake;
        stake_machine_type;
        dal_node_producers;
        observer_slot_indices;
        observer_pkhs;
        protocol;
        producer_machine_type;
        echo_rollup;
        disconnect;
        network;
        simulate_network;
        snapshot;
        bootstrap;
        teztale;
        memtrace;
        data_dir;
        fundraiser;
        producer_key;
        producers_delay;
        blocks_history;
        bootstrap_node_identity_file;
        bootstrap_dal_node_identity_file;
        external_rpc;
        disable_shard_validation;
        ignore_pkhs;
        ppx_profiling;
        ppx_profiling_backends;
        network_health_monitoring;
      }
    in
    (t, etherlink)
  in
  toplog "Parsing CLI done" ;
  let baker_daemon_count =
    match simulate_network with
    | Scenarios_cli.Disabled -> 0
    | Scatter (_selected_baker_count, baker_daemon_count) -> baker_daemon_count
    | Map
        ( _selected_baker_count,
          single_baker_daemon_count,
          multiple_baker_daemon_count ) ->
        single_baker_daemon_count + multiple_baker_daemon_count
  in
  let vms =
    let* stake = configuration.stake in
    return
    @@ List.concat
         [
           (if configuration.bootstrap then [Bootstrap] else []);
           List.mapi (fun i _ -> Baker i) stake;
           List.init baker_daemon_count (fun i -> Baker i);
           List.map (fun i -> Producer i) configuration.dal_node_producers;
           List.map
             (fun index -> Observer (`Index index))
             configuration.observer_slot_indices;
           List.map (fun pkh -> Observer (`Pkh pkh)) configuration.observer_pkhs;
           (if etherlink_configuration <> None then [Etherlink_operator] else []);
           (match etherlink_configuration with
           | None | Some {etherlink_dal_slots = []; _} -> []
           | Some {etherlink_dal_slots = [_]; _} -> [Etherlink_dal_operator]
           | Some {etherlink_dal_slots; _} ->
               Reverse_proxy :: Etherlink_dal_operator
               :: List.map
                    (fun slot_index -> Etherlink_dal_observer {slot_index})
                    etherlink_dal_slots);
           (match etherlink_configuration with
           | None -> []
           | Some {etherlink_producers; _} ->
               List.init etherlink_producers (fun i -> Etherlink_producer i));
           (if configuration.echo_rollup then
              Echo_rollup_operator :: Reverse_proxy
              :: List.map
                   (fun slot_index -> Echo_rollup_dal_observer {slot_index})
                   configuration.dal_node_producers
            else []);
         ]
  in
  let docker_image =
    Option.map
      (fun tag -> Agent.Configuration.Octez_release {tag})
      Cli.octez_release
  in
  let default_vm_configuration ~name =
    Agent.Configuration.make ?docker_image ~name ()
  in
  let vms =
    let* vms in
    return
    @@ List.map
         (fun agent_kind ->
           let name = name_of agent_kind in
           match agent_kind with
           | Bootstrap -> default_vm_configuration ~name
           | Baker i -> (
               match configuration.stake_machine_type with
               | None -> default_vm_configuration ~name
               | Some list -> (
                   try
                     let machine_type = List.nth list i in
                     Agent.Configuration.make
                       ?docker_image
                       ~machine_type
                       ~name
                       ()
                   with _ -> default_vm_configuration ~name))
           | Producer _ ->
               let machine_type = configuration.producer_machine_type in
               Agent.Configuration.make ?docker_image ?machine_type ~name ()
           | Observer _ | Etherlink_dal_operator | Etherlink_dal_observer _
           | Echo_rollup_dal_observer _ ->
               Agent.Configuration.make ?docker_image ~name ()
           | Echo_rollup_operator -> default_vm_configuration ~name
           | Etherlink_operator -> default_vm_configuration ~name
           | Etherlink_producer _ -> default_vm_configuration ~name
           | Reverse_proxy -> default_vm_configuration ~name)
         vms
  in
  let endpoint, resolver_endpoint = Lwt.wait () in
  Cloud.register
  (* docker images are pushed before executing the test in case binaries are modified locally. This way we always use the latest ones. *)
    ~vms
    ~proxy_files:
      ([
         yes_wallet_exe;
         Format.asprintf
           "src/%s/parameters/mainnet-parameters.json"
           (Protocol.directory configuration.protocol);
         "octez-node";
       ]
      @ (if Cli.refresh_binaries then
           [
             "octez-dal-node";
             "octez-client";
             Tezt_wrapper.Uses.path Constant.octez_agnostic_baker;
           ]
           @ (if Cli.etherlink then
                ["evm_kernel.wasm"; "octez-evm-node"; "octez-smart-rollup-node"]
              else [])
           @
           if Cli.teztale then
             ["octez-teztale-archiver"; "octez-teztale-server"]
           else []
         else [])
      @ Option.fold
          ~none:[]
          ~some:(fun x -> [x])
          configuration.bootstrap_node_identity_file
      @ Option.fold
          ~none:[]
          ~some:(fun x -> [x])
          configuration.bootstrap_dal_node_identity_file)
    ~proxy_args:
      (match configuration.fundraiser with
      | None -> []
      | Some fundraiser_key -> ["--fundraiser"; fundraiser_key])
    ~__FILE__
    ~title:"DAL node benchmark"
    ~tags:[]
    (fun cloud ->
      toplog "Creating the agents" ;
      if
        (not configuration.with_dal)
        && List.length configuration.dal_node_producers > 0
        && configuration.network = `Sandbox
      then
        Log.warn
          "You are running in sandboxed mode with some DAL producers but with \
           DAL deactivated for bakers. DAL network can't work properly. This \
           is probably a configuration issue." ;
      let agents = Cloud.agents cloud in
      let next_agent ~name =
        let agent =
          match List.find_opt (fun agent -> Agent.name agent = name) agents with
          | None ->
              if Cli.proxy_localhost then List.hd agents
              else Test.fail ~__LOC__ "Agent not found: %s" name
          | Some agent -> agent
        in
        Lwt.return agent
      in
      if configuration.network_health_monitoring then
        Monitoring_app.Tasks.register_chronos_task cloud ~configuration endpoint ;
      let* t = init ~configuration etherlink_configuration cloud next_agent in
      Lwt.wakeup resolver_endpoint t.some_node_rpc_endpoint ;
      toplog "Starting main loop" ;
      loop t (t.first_level + 1))
