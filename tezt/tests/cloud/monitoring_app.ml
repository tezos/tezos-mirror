(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* time interval in hours at which to submit report *)
let report_interval = 6

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

let encapsulate_in_code_block strings = ("```" :: strings) @ ["```"]

let pp_delegate fmt delegate_pkh =
  match Hashtbl.find_opt Metrics.aliases delegate_pkh with
  | None -> Format.fprintf fmt "%s" delegate_pkh
  | Some alias ->
      Format.fprintf fmt "%s : %-26s" (String.sub delegate_pkh 0 7) alias

(** [network_to_image_url network] return an image for each monitored network. *)
let network_to_image_url : Network.t -> string = function
  | `Mainnet ->
      "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/mainnet.png"
  | `Shadownet ->
      "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/shadownet.png"
  | `Ghostnet ->
      "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/ghostnet.png"
  | `Seoulnet ->
      "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/seoulnet.png"
  | `Nextnet _ | `Tallinnnet ->
      "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/tallinnnet.png"
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
      :: Option.fold ~none:[] ~some:(fun ts -> [("thread_ts", string ts)]) ts)
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

module Prometheus = struct
  let endpoint_from_prometheus_query ~query =
    let fail ~uri_part =
      Test.fail
        "DAL.Monitoring_app.Tasks.endpoint_from_prometheus_query: expecting a \
         prometheus %s"
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
          "Unexpected error while fetching prometheus query (origin : %s): '%s'"
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
end

module Baker_helpers = struct
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
    let open Prometheus in
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
    let* attestable = fetch ~decoder:decoder_prometheus_float ~query ~origin in
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
    let attests_while_out_of_dal_committee = Option.is_some out_attestations in
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

  let get_current_baker_infos =
    (* [current_bakers] contains the current cycle as well as the list of baker
       infos for all bakers.
       When trying to update it, we first look if the cycle changed, and compute
       the new one only if it did. *)
    let current_cycle = ref (-1) in
    let current_bakers = ref [] in
    fun endpoint ->
      let* cycle = get_current_cycle endpoint in
      let* () =
        if cycle = !current_cycle then unit
        else
          let* bakers = get_bakers_with_staking_power endpoint cycle in
          let total_baking_power =
            List.fold_left
              (fun acc RPC.{delegate = _; staked; weighted_delegated} ->
                acc + staked + weighted_delegated)
              0
              bakers
          in
          let* bakers_info =
            Lwt_list.map_s
              (fun RPC.{delegate; staked; weighted_delegated} ->
                let* attest_infos =
                  fetch_baker_info
                    ~origin:(Format.sprintf "fetch_baker_info.%s" delegate)
                    ~tz1:delegate
                in
                let baking_power = staked + weighted_delegated in
                let stake_fraction =
                  float_of_int baking_power /. float_of_int total_baking_power
                in
                return {address = PKH delegate; attest_infos; stake_fraction})
              bakers
          in
          current_cycle := cycle ;
          current_bakers := bakers_info ;
          unit
      in
      return !current_bakers
end

module Tasks = struct
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
    let open Prometheus in
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
    Lwt.return (`slot_index slot_index, `attested attested, `published published)

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
    let open Prometheus in
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
    let slot_size =
      126_944
      (* TODO: do not hard-code this *)
    in
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

  type classified_bakers = {
    mute_bakers : Baker_helpers.baker_infos list;
    muted_by_dal : Baker_helpers.baker_infos list;
    dal_zero : Baker_helpers.baker_infos list;
    dal_on : Baker_helpers.baker_infos list;
    no_shards : Baker_helpers.baker_infos list;
    dal_off : Baker_helpers.baker_infos list;
  }

  let fetch_bakers_info endpoint =
    let open Baker_helpers in
    let* bakers_info = get_current_baker_infos endpoint in
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
        ":alert: *Those bakers never sent attestation while in DAL committee, \
         however they sent attestations when they are out of it. This is quite \
         unexpected. They probably have an issue.*"
        pp_baker_light
        muted_by_dal
    in
    let display_zero =
      display
        ":triangular_flag_on_post: *Those bakers have a broken DAL node. They \
         send attestations with a DAL content, but do not attest any slots.*"
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
         never were in committee when slots were produced, hence we cannot say \
         much about how well it works.*"
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

  let action ~slack_bot_token ~slack_channel_id ~network endpoint () =
    let* endpoint in
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
  let register_chronos_task cloud ~network endpoint =
    match Cloud.notifier cloud with
    | Notifier_null -> ()
    | Notifier_slack {slack_bot_token; slack_channel_id; _} ->
        let task =
          let action () =
            action ~slack_bot_token ~slack_channel_id ~network endpoint ()
          in
          let tm = Format.sprintf "0 0-23/%d * * *" report_interval in
          Chronos.task ~name:"network-overview" ~tm ~action ()
        in
        Cloud.register_chronos_task cloud task
end

module Alert = struct
  let report_lost_dal_rewards ~slack_channel_id ~slack_bot_token ~network ~level
      ~cycle ~lost_dal_rewards =
    let data =
      let header =
        Format.sprintf
          "*[lost-dal-rewards]* On network `%s`, %d delegates have lost DAL \
           rewards at cycle `%d`, level `%d`. \
           <https://%s.tzkt.io/%d/implicit_operations/dal_attestation_reward \
           |See online>"
          (Network.to_string network)
          (List.length lost_dal_rewards)
          cycle
          level
          (Network.to_string network)
          level
      in
      Format_app.section [header] ()
    in
    let* ts = post_message ~slack_channel_id ~slack_bot_token data in
    (* Lost DAL rewards are sorted to get the biggest amount first. *)
    let lost_dal_rewards =
      List.sort
        (fun (_, `change c1) (_, `change c2) -> -Int.compare c1 c2)
        lost_dal_rewards
    in
    let lost_dal_rewards =
      List.map
        encapsulate_in_code_block
        (group_by
           15
           (List.map
              (fun (`delegate delegate, `change change) ->
                Format.asprintf
                  "%a has missed ~%.2f tez"
                  pp_delegate
                  delegate
                  (float_of_int change /. 1_000_000.))
              lost_dal_rewards))
    in
    Lwt_list.iter_s
      (fun to_post ->
        let data =
          let open Format_app in
          section to_post ()
        in
        let* _ts = post_message ~ts ~slack_channel_id ~slack_bot_token data in
        unit)
      lost_dal_rewards

  let check_for_lost_dal_rewards ~cloud ~network ~metadata =
    match Cloud.notifier cloud with
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
                    let change = JSON.(balance_update |-> "change" |> as_int) in
                    Some (`delegate delegate, `change change)
                  else None)
            balance_updates
        in
        if List.is_empty lost_dal_rewards then unit
        else
          report_lost_dal_rewards
            ~slack_channel_id
            ~slack_bot_token
            ~network
            ~level
            ~cycle
            ~lost_dal_rewards

  let check_for_lost_dal_rewards ~cloud ~network ~metadata =
    Lwt.catch
      (fun () -> check_for_lost_dal_rewards ~cloud ~network ~metadata)
      (fun exn ->
        Log.warn
          "Monitor_app.Alert.check_for_lost_dal_rewards: unexpected error: '%s'"
          (Printexc.to_string exn) ;
        unit)

  let report_dal_accusations ~slack_channel_id ~slack_bot_token ~network ~level
      ~cycle dal_accusations =
    let data =
      let header =
        Format.sprintf
          "*[dal-accusations]* On network `%s`, delegates have been accused at \
           cycle `%d`, level `%d`."
          (Network.to_string network)
          cycle
          level
      in
      let content =
        List.map
          (fun ( `attestation_level attestation_level,
                 `slot_index slot_index,
                 `delegate delegate,
                 `op_hash hash )
             ->
            Format.asprintf
              ":black_small_square: %a for attesting slot index `%d` at level \
               `%d`. <https://%s.tzkt.io/%s|See online>"
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

  let check_for_dal_accusations ~cloud ~network ~cycle ~level ~operations
      ~endpoint =
    match Cloud.notifier cloud with
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
                   `op_hash hash )
               ->
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
          report_dal_accusations
            ~slack_channel_id
            ~slack_bot_token
            ~network
            ~cycle
            ~level
            dal_accusations

  let check_for_dal_accusations ~cloud ~network ~cycle ~level ~operations
      ~endpoint =
    Lwt.catch
      (fun () ->
        check_for_dal_accusations
          ~cloud
          ~network
          ~cycle
          ~level
          ~operations
          ~endpoint)
      (fun exn ->
        Log.warn
          "Monitor_app.Alert.check_for_dal_accusations: unexpected error: '%s'"
          (Printexc.to_string exn) ;
        unit)

  type attestation_transition = Stopped_attesting | Restarted_attesting

  let report_attestation_transition ~slack_channel_id ~slack_bot_token ~network
      ~level ~baker ~transition ~attestation_percentage =
    let data =
      let header =
        Format.asprintf
          (match transition with
          | Restarted_attesting ->
              "*[dal-attester-is-back]* On network `%s` at level `%d`, \
               delegate `%a` who was under the reward threshold is again above \
               threshold. New attestation rate is %.2f%%."
          | Stopped_attesting ->
              "*[dal-attester-dropped]* On network `%s` at level `%d`, \
               delegate `%a` DAL attestation rate dropped under the reward \
               threshold. New attestation rate is %.2f%%.")
          (Network.to_string network)
          level
          Baker_helpers.pp_baker_light
          baker
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
    fun ~cloud ~endpoint ~network ~level ~metadata ->
      match Cloud.notifier cloud with
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
            let treat_delegate baker =
              let (PKH pkh) = baker.Baker_helpers.address in
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
                        ~baker
                        ~transition
                        ~attestation_percentage)
            in
            let refill_delegates_to_treat () =
              let* bakers = Baker_helpers.get_current_baker_infos endpoint in
              to_treat_delegates := bakers ;
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

  let check_for_recently_missed_a_lot ~cloud ~endpoint ~network ~level ~metadata
      =
    Lwt.catch
      (fun () ->
        check_for_recently_missed_a_lot
          ~cloud
          ~endpoint
          ~network
          ~level
          ~metadata)
      (fun exn ->
        Log.warn
          "Monitor_app.Alert.check_for_recently_missed_a_lot: unexpected \
           error: '%s'"
          (Printexc.to_string exn) ;
        unit)

  let report_funds_are_getting_short ~cloud ~network ~fee ~pkh ~balance =
    match Cloud.notifier cloud with
    | Notifier_null -> unit
    | Notifier_slack {slack_channel_id; slack_bot_token; _} ->
        let data =
          let content =
            Format.sprintf
              "*[producer-is-getting-broke]* On network `%s`, producer `%s` \
               only has %dµꜩ remaining. With current price of %dµꜩ per slot \
               produced, it can only publish %d more slots before being left \
               speechless."
              (Network.to_string network)
              pkh
              balance
              fee
              (balance / fee)
          in
          Format_app.section [content] ()
        in
        let* _ts = post_message ~slack_channel_id ~slack_bot_token data in
        Lwt.return_unit

  let report_funds_are_getting_short ~cloud ~network ~fee ~pkh ~balance =
    Lwt.catch
      (fun () ->
        report_funds_are_getting_short ~cloud ~network ~fee ~pkh ~balance)
      (fun exn ->
        Log.warn
          "Monitor_app.Alert.report_funds_are_getting_short: unexpected error: \
           '%s'"
          (Printexc.to_string exn) ;
        unit)
end
