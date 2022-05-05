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

let group = {Clic.name = "protocols"; title = "Commands for managing protocols"}

let proto_param ~name ~desc t =
  Clic.param
    ~name
    ~desc
    (Clic.parameter (fun _ str -> Lwt.return (Protocol_hash.of_b58check str)))
    t

let commands () =
  let open Clic in
  let open Lwt_result_syntax in
  let check_dir _ dn =
    if Sys.is_directory dn then return dn
    else failwith "%s is not a directory" dn
  in
  let check_dir_parameter = parameter check_dir in
  [
    command
      ~group
      ~desc:"List protocols known by the node."
      no_options
      (prefixes ["list"; "protocols"] stop)
      (fun () (cctxt : #Client_context.full) ->
        let* protos = Shell_services.Protocol.list cctxt in
        let*! () =
          List.iter_s (fun ph -> cctxt#message "%a" Protocol_hash.pp ph) protos
        in
        return_unit);
    command
      ~group
      ~desc:"Inject a new protocol into the node."
      no_options
      (prefixes ["inject"; "protocol"]
      @@ param
           ~name:"dir"
           ~desc:"directory containing the sources of a protocol"
           check_dir_parameter
      @@ stop)
      (fun () dirname (cctxt : #Client_context.full) ->
        Lwt.catch
          (fun () ->
            let* (_hash, proto) =
              Tezos_base_unix.Protocol_files.read_dir dirname
            in
            let*! injection_result =
              Shell_services.Injection.protocol cctxt proto
            in
            match injection_result with
            | Ok hash ->
                let*! () =
                  cctxt#message
                    "Injected protocol %a successfully"
                    Protocol_hash.pp
                    hash
                in
                return_unit
            | Error err ->
                cctxt#error
                  "Error (error) while injecting protocol from %s: %a"
                  dirname
                  Error_monad.pp_print_trace
                  err)
          (fun exn ->
            cctxt#error
              "Error (exn) while injecting protocol from %s: %a"
              dirname
              Error_monad.pp_print_trace
              [Error_monad.Exn exn]));
    command
      ~group
      ~desc:"Dump a protocol from the node's record of protocol."
      no_options
      (prefixes ["dump"; "protocol"]
      @@ proto_param ~name:"protocol hash" ~desc:""
      @@ stop)
      (fun () ph (cctxt : #Client_context.full) ->
        let* proto = Shell_services.Protocol.contents cctxt ph in
        let* () =
          Tezos_base_unix.Protocol_files.write_dir
            (Protocol_hash.to_short_b58check ph)
            ~hash:ph
            proto
        in
        let*! () =
          cctxt#message "Extracted protocol %a" Protocol_hash.pp_short ph
        in
        return_unit);
    command
      ~group
      ~desc:"Show the environment version used by a protocol."
      no_options
      (prefixes ["protocol"; "environment"]
      @@ proto_param ~name:"protocol hash" ~desc:""
      @@ stop)
      (fun () protocol_hash (cctxt : #Client_context.full) ->
        let* env = Shell_services.Protocol.environment cctxt protocol_hash in
        let*! () =
          cctxt#message
            "Protocol %a uses environment %s"
            Protocol_hash.pp
            protocol_hash
            (Protocol.module_name_of_env_version env)
        in
        return_unit);
    command
      ~group
      ~desc:"Fetch a protocol from the network."
      no_options
      (prefixes ["fetch"; "protocol"]
      @@ proto_param ~name:"protocol hash" ~desc:""
      @@ stop)
      (fun () hash (cctxt : #Client_context.full) ->
        let*! fetch_result = Shell_services.Protocol.fetch cctxt hash in
        match fetch_result with
        | Ok () ->
            let*! () =
              cctxt#message
                "Protocol %a successfully fetched."
                Protocol_hash.pp_short
                hash
            in
            return_unit
        | Error err ->
            let*! () =
              cctxt#error
                "Error while fetching protocol: %a"
                Error_monad.pp_print_trace
                err
            in
            return_unit);
  ]
