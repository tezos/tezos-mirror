(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Cmdliner
module Proxy_server_config = Tezos_proxy_server_config.Proxy_server_config

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4025
   Remove backwards compatible Tezos symlinks. *)
let () =
  (* warn_if_argv0_name_not_octez *)
  let executable_name = Filename.basename Sys.argv.(0) in
  let prefix = "tezos-" in
  if TzString.has_prefix executable_name ~prefix then
    let expected_name =
      let len_prefix = String.length prefix in
      "octez-"
      ^ String.sub
          executable_name
          len_prefix
          (String.length executable_name - len_prefix)
    in
    Format.eprintf
      "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
       The executable with name @{<kwd>%s@} has been renamed to @{<kwd>%s@}. \
       The name @{<kwd>%s@} is now@,\
       deprecated, and it will be removed in a future release. Please update@,\
       your scripts to use the new name.@]@\n\
       @."
      executable_name
      expected_name
      executable_name
  else ()

let name = "octez-proxy-server"

let config : string option Term.t =
  let doc =
    "The configuration file. Fields (see corresponding options): endpoint \
     (string), rpc_addr (string), rpc_tls (string), sym_block_caching_time \
     (int), and data_dir (string)."
  in
  let docv = "CONFIG" in
  Arg.(value & opt (some string) None & info ["c"; "config"] ~docv ~doc)

let endpoint : string option Term.t =
  let doc = "The node to do requests to for obtaining data when needed." in
  let docv = "ENDPOINT" in
  Arg.(value & opt (some string) None & info ["E"; "endpoint"] ~docv ~doc)

let log_requests =
  let doc = "Log all requests to the node." in
  let docv = "LOG_REQUESTS" in
  Arg.(value & flag & info ["l"; "log-requests"] ~docv ~doc)

let rpc_addr : string option Term.t =
  let doc =
    "The TCP socket at which this RPC server instance can be reached."
  in
  let docv = "RPC_ADDR" in
  Arg.(value & opt (some string) None & info ["rpc-addr"] ~docv ~doc)

let rpc_tls : string option Term.t =
  let doc =
    "Enable TLS for this RPC server with the provided certificate and key."
  in
  let docv = "RPC_TLS" in
  Arg.(value & opt (some string) None & info ["rpc-tls"] ~docv ~doc)

let data_dir : string option Term.t =
  let doc =
    "Path to the data-dir of a running octez-node, for reading the `context` \
     subdirectory to obtain data instead of using the ../raw/bytes RPC (hereby \
     reducing the node's IO)."
  in
  let docv = "DATA_DIR" in
  Arg.(value & opt (some string) None & info ["d"; "data-dir"] ~docv ~doc)

let sym_block_caching_time : Ptime.span option Term.t =
  let doc =
    "The duration (in seconds) during which data for a symbolic block \
     identifier (like head) is kept. Smaller values increase the endpoint's \
     load but give more up-to-date data. If omitted, defaulted to \
     time_between_blocks (and causes extra RPCs to the endpoint to retrieve \
     this value)."
  in
  let docv = "SYM_BLOCK_CACHING_TIME" in
  Term.(app @@ const @@ Option.map @@ Ptime.Span.of_int_s)
    Arg.(
      value & opt (some int) None & info ["sym-block-caching-time"] ~docv ~doc)

let load_config_from_file (config_file : string) =
  let open Lwt_result_syntax in
  let* json = Lwt_utils_unix.Json.read_file config_file in
  let open Proxy_server_config in
  match destruct_config json with
  | CannotDeserialize ->
      failwith
        "%s cannot be parsed. To fix it, take inspiration from a valid file, \
         like this one :%s"
        config_file
        example_config
  | Invalid msg -> failwith "%s" msg
  | Valid config_from_file -> return config_from_file

(** [get_runtime] unions the arguments coming from the config file
    and the command line, and translates the result to
    a value of type [Proxy_server_config.runtime]. *)
let get_runtime config_from_file config_args =
  let open Lwt_result_syntax in
  let open Proxy_server_config in
  let config =
    match config_from_file with
    | None ->
        (* No config file: all data comes from command line arguments *)
        config_args
    | Some config_from_file ->
        (* Config file specified: override it with command line arguments *)
        Proxy_server_config.union_right_bias config_from_file config_args
  in
  match to_runtime config with
  | Error msg -> failwith "%s" msg
  | Ok runtime -> return runtime

let main_promise (config_file : string option)
    (config_args : Proxy_server_config.t) (log_requests : bool) :
    int tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* (config_from_file : Proxy_server_config.t option) =
    Option.map_es load_config_from_file config_file
  in
  let open Proxy_server_config in
  let* {
         endpoint;
         rpc_server_address;
         rpc_server_port;
         rpc_server_tls;
         sym_block_caching_time;
         data_dir = data_dir_opt;
       } =
    get_runtime config_from_file config_args
  in
  let open Tezos_rpc_http in
  let open Tezos_rpc_http_client_unix in
  let logger =
    if log_requests then RPC_client_unix.full_logger Format.err_formatter
    else RPC_client_unix.null_logger
  in
  let rpc_config : RPC_client_unix.config =
    {media_type = Media_type.Command_line.Any; endpoint; logger}
  in
  let printer =
    let logger channel msg : unit Lwt.t =
      if channel = "stderr" then
        let*! () = Lwt_io.eprintf "%s" msg in
        Lwt_io.(flush stderr)
      else
        let*! () = Lwt_io.printf "%s" msg in
        Lwt_io.(flush stdout)
    in
    new Tezos_client_base.Client_context.simple_printer logger
  in
  let http_ctxt =
    new RPC_client_unix.http_ctxt rpc_config Media_type.all_media_types
  in
  (* This should probably be extracted into Lwt_result or similar. *)
  let lift_lwt (a : 'a Lwt.t) : 'a tzresult Lwt.t =
    let*! r = a in
    return r
  in
  (* The context index, which we try to read if and only if the --data-dir *)
  (* argument has been passed. *)
  let* (context_index : Tezos_context.Context.index option) =
    Option.map_es
      (fun data_dir ->
        let context_path = Filename.concat data_dir "context" in
        Lwt.catch
          (fun () ->
            lift_lwt
              (Tezos_shell_context.Proxy_delegate_maker.make_index
                 ~context_path))
          (function
            | Index_unix.Private.Raw.Not_written ->
                failwith
                  "error reading data-dir: %s is not a valid context directory"
                  context_path
            | Unix.Unix_error (Unix.EBADF, _, _) ->
                failwith
                  "error reading data-dir: %s does not exist"
                  context_path
            | e ->
                failwith
                  "unexpected error %s while reading context directory %s"
                  (Printexc.to_string e)
                  context_path))
      data_dir_opt
  in
  (* Now we build the function that the proxy server can use to build a proxy
     delegate later, using [Tezos_shell_context.Proxy_delegate_maker.*] functions.
     This lets it not depend directly on [Tezos_shell_context]: if it did, it
     would break compilation of octez-client to JavaScript. *)
  let on_disk_proxy_builder =
    Option.map
      (fun index ctx_hash ->
        (* Sync first so we don't observe a stale state. *)
        let*! () = Tezos_context.Context.sync index in
        Tezos_shell_context.Proxy_delegate_maker.of_index ~index ctx_hash)
      context_index
  in
  let dir =
    let sleep = Lwt_unix.sleep in
    Tezos_proxy.Proxy_services.build_directory
      printer
      http_ctxt
      (Tezos_proxy.Proxy_services.Proxy_server
         {sleep; sym_block_caching_time; on_disk_proxy_builder})
      None
  in
  let server_args : Proxy_server_main_run.args =
    {
      address = rpc_server_address;
      port = rpc_server_port;
      tls_cert_and_key = rpc_server_tls;
      forwarding_endpoint = endpoint;
    }
  in
  Proxy_server_main_run.run dir server_args

let main (config_file : string option) (log_requests : bool)
    (endpoint : string option) (rpc_addr : string option)
    (rpc_tls : string option) (sym_block_caching_time : Ptime.span option)
    (data_dir : string option) =
  let config_args =
    Proxy_server_config.make
      ~endpoint:(Option.map Uri.of_string endpoint)
      ~rpc_addr:(Option.map Uri.of_string rpc_addr)
      ~rpc_tls
      ~sym_block_caching_time
      ~data_dir
  in
  Lwt_main.run
    (let open Lwt_syntax in
    let* r =
      Lwt_exit.wrap_and_error
      @@ main_promise config_file config_args log_requests
    in
    match r with
    | Ok (Ok _) ->
        let+ _ = Lwt_exit.exit_and_wait 0 in
        `Ok ()
    | Ok (Error err) ->
        let+ _ = Lwt_exit.exit_and_wait 2 in
        `Error (false, Format.asprintf "%a" pp_print_trace err)
    | Error exit_status ->
        Lwt.return (`Error (false, Format.asprintf "Exited %d" exit_status)))

let term : unit Term.t =
  Term.(
    ret
      (const main $ config $ log_requests $ endpoint $ rpc_addr $ rpc_tls
     $ sym_block_caching_time $ data_dir))

let info =
  let doc = "Launches a server that is a readonly frontend to a Tezos node" in
  let man =
    [
      `S Manpage.s_bugs;
      `P "Report issues to https://gitlab.com/tezos/tezos/-/issues";
    ]
  in
  let version = Tezos_version_value.Bin_version.version_string in
  Cmd.info name ~version ~doc ~exits:Cmd.Exit.defaults ~man

let () = exit (Cmd.eval (Cmd.v info term))
