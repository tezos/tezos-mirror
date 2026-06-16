(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Opentelemetry

module Event = struct
  include Internal_event.Simple

  let section = ["opentelemetry"]

  let new_instance_id =
    declare_1
      ~section
      ~name:"opentelemetry_new_instance_id"
      ~msg:"new Opentelemetry instance id generated ({instance_id})"
      ~level:Notice
      ("instance_id", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let enabled =
    declare_2
      ~section
      ~name:"opentelemetry_enabled"
      ~msg:
        "Opentelemetry profiling is enabled in the namespace \
         {service_namespace} for instance id {instance_id}"
      ~level:Notice
      ("service_namespace", Data_encoding.string)
      ("instance_id", Data_encoding.string)
      ~pp1:Format.pp_print_string
      ~pp2:Format.pp_print_string
end

let instance_id_filename = "telemetry_id"

let setup ~data_dir ~service_namespace ~service_name ~version ?level ?sections
    {
      Opentelemetry_config.enable;
      instance_id;
      environment;
      config;
      gc_telemetry;
    } =
  let open Lwt_result_syntax in
  let no_clean_up = return Lwt.return in
  if enable then (
    let*! instance_id =
      match instance_id with
      | Some id -> Lwt.return id
      | None ->
          let file = Filename.concat data_dir instance_id_filename in
          let*! exists = Lwt_unix.file_exists file in
          if exists then Lwt_utils_unix.read_file file
          else
            let _, _, id = Tezos_crypto.Crypto_box.random_keypair () in
            let sid = Tezos_crypto.Crypto_box.Public_key_hash.to_b58check id in
            let*! () = Lwt_utils_unix.create_file file sid in
            let*! () = Event.(emit new_instance_id) sid in
            Lwt.return sid
    in
    Globals.service_name := service_name ;
    Globals.service_namespace := Some service_namespace ;
    Globals.service_instance_id := Some instance_id ;
    (* For datadog integration: see
       https://docs.datadoghq.com/getting_started/tagging/unified_service_tagging *)
    Globals.add_global_attribute "service.version" (`String version) ;
    (match environment with
    | None -> ()
    | Some env ->
        Globals.add_global_attribute "deployment.environment" (`String env)) ;
    Opentelemetry_ambient_context.set_storage_provider
      (Opentelemetry_ambient_context_lwt.storage ()) ;
    Opentelemetry_client_cohttp_lwt.setup ~enable ~config () ;
    if gc_telemetry.enable then
      Gc.instrument
        ?filter:gc_telemetry.filter
        ?min_duration_ms:gc_telemetry.min_duration_ms
        () ;
    match Opentelemetry.Collector.get_backend () with
    | None -> no_clean_up
    | Some (module Backend) ->
        let* () = Events.activate ?level ?sections () in
        let*! () = Event.(emit enabled) (service_namespace, instance_id) in
        return (fun () ->
            let clean_up_done, clean_up_done_resolver = Lwt.task () in
            let on_done () = Lwt.wakeup_later clean_up_done_resolver () in
            Backend.cleanup ~on_done () ;
            clean_up_done))
  else no_clean_up

let is_enabled () = Option.is_some (Opentelemetry.Collector.get_backend ())
