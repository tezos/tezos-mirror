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

type t = {
  endpoint : Uri.t option;
  rpc_addr : Uri.t option;
  rpc_tls : string option;
  sym_block_caching_time : int option;
  data_dir : string option;
}

let pp ppf {endpoint; rpc_addr; rpc_tls; sym_block_caching_time; data_dir} =
  let pp_uri_opt = Format.pp_print_option Uri.pp in
  Format.fprintf
    ppf
    "@[<v>endpoint=%a@,\
     rpc_addr=%a@,\
     rpc_tls=%a@,\
     sym_block_caching_time=%a@,\
     data_dir=%a@]"
    pp_uri_opt
    endpoint
    pp_uri_opt
    rpc_addr
    (Format.pp_print_option Format.pp_print_string)
    rpc_tls
    (Format.pp_print_option Format.pp_print_int)
    sym_block_caching_time
    (Format.pp_print_option Format.pp_print_string)
    data_dir

let example_config =
  {|{"endpoint": "http://127.0.0.1:18731", "rpc_addr": "http://127.0.0.1:18732", "sym_block_caching_time": 60}|}

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun t ->
      ( Option.map Uri.to_string t.endpoint,
        Option.map Uri.to_string t.rpc_addr,
        t.rpc_tls,
        Option.map Int32.of_int t.sym_block_caching_time,
        t.data_dir ))
    (fun (endpoint, rpc_addr, rpc_tls, sym_block_caching_time, data_dir) ->
      {
        endpoint = Option.map Uri.of_string endpoint;
        rpc_addr = Option.map Uri.of_string rpc_addr;
        rpc_tls;
        sym_block_caching_time = Option.map Int32.to_int sym_block_caching_time;
        data_dir;
      })
    (obj5
       (opt "endpoint" string)
       (opt "rpc_addr" string)
       (opt "rpc_tls" string)
       (opt "sym_block_caching_time" int32)
       (opt "data_dir" string))

let make ~endpoint ~rpc_addr ~rpc_tls ~sym_block_caching_time ~data_dir : t =
  {endpoint; rpc_addr; rpc_tls; sym_block_caching_time; data_dir}

let sym_block_caching_time_error sym_block_caching_time =
  match sym_block_caching_time with
  | Some sym_block_caching_time when sym_block_caching_time <= 0 ->
      Some
        (Format.sprintf
           {|--sym-block-caching-time argument and sym_block_caching_time field must be strictly positive, but found %d|}
           sym_block_caching_time)
  | _ -> None

type 'a destructed = Valid of 'a | Invalid of string | CannotDeserialize

let destruct_config json =
  match Data_encoding.Json.destruct encoding json with
  | cfg -> (
      match sym_block_caching_time_error cfg.sym_block_caching_time with
      | Some err -> Invalid err
      | None -> Valid cfg)
  | exception _ -> CannotDeserialize

let union_right_bias (t1 : t) (t2 : t) =
  {
    endpoint = Option.either t2.endpoint t1.endpoint;
    rpc_addr = Option.either t2.rpc_addr t1.rpc_addr;
    rpc_tls = Option.either t2.rpc_tls t1.rpc_tls;
    sym_block_caching_time =
      Option.either t2.sym_block_caching_time t1.sym_block_caching_time;
    data_dir = Option.either t2.data_dir t1.data_dir;
  }

type runtime = {
  endpoint : Uri.t;
  rpc_server_address : P2p_addr.t;
  rpc_server_port : int;
  rpc_server_tls : (string * string) option;
  sym_block_caching_time : int option;
  data_dir : string option;
}

(** Given the value of the [--rpc-addr] argument (or the [rpc_addr] CONFIG field),
    return the address and the port of the server that should be spawned. *)
let address_and_port_for_runtime rpc_addr =
  let open Result in
  let wrong_rpc_addr looked_for =
    error
    @@ Format.asprintf
         {|Wrong "--rpc-addr" argument or "rpc_addr" field: %a. %s cannot be determined|}
         Uri.pp
         rpc_addr
         looked_for
  in
  match (Uri.host rpc_addr, Uri.port rpc_addr) with
  | (None, _) -> wrong_rpc_addr "Hostname"
  | (_, None) -> wrong_rpc_addr "Port"
  | (Some rpc_server_address, Some rpc_server_port) -> (
      match P2p_addr.of_string_opt rpc_server_address with
      | Some rpc_server_address -> Ok (rpc_server_address, rpc_server_port)
      | None ->
          error
          @@ Format.asprintf
               {|Cannot convert hostname of "--rpc-addr" argument or "rpc_addr" field to P2p_addr: %s|}
               rpc_server_address)

(** Given the value of the [--rpc-tls] argument (or the [rpc_tls] CONFIG field),
    return the paths to the certificate and the key for use by TLS *)
let tls_for_runtime =
  let open Result in
  let regexp_str = "(.*),(.*)" in
  let regexp = Re.compile (Re.Perl.re regexp_str) in
  fun rpc_tls ->
    match Re.exec_opt regexp rpc_tls with
    | None ->
        error
        @@ Format.asprintf
             {|Value of "--rpc-tls" argument or "rpc_tls" field cannot be parsed: %s doesn't match regexp %s|}
             rpc_tls
             regexp_str
    | Some group -> ok (Re.Group.get group 1, Re.Group.get group 2)

(** Helper to lift a validation function [f : 'a -> ('b * _) result] over
    an optional value. *)
let opt_res_to_res_opt = function
  | None ->
      (* No data to validate: no error, no data. *)
      Ok None
  | Some (Ok x) ->
      (* Data was successfully validated: no error, some data. *)
      Ok (Some x)
  | Some (Error x) ->
      (* Data could not be successfully validated: an error and no data *)
      Error x

let to_runtime
    ({endpoint; rpc_addr; rpc_tls; sym_block_caching_time; data_dir} : t) :
    (runtime, string) result =
  (* Validating sym_block_caching_time is required if it was specified
     on the command line. In this case it wasn't validated yet. *)
  let open Result_syntax in
  match
    (endpoint, rpc_addr, sym_block_caching_time_error sym_block_caching_time)
  with
  | (None, _, _) ->
      fail
        {|Endpoint not specified: pass argument --endpoint or specify "endpoint" field in CONFIG file|}
  | (_, None, _) ->
      fail
        {|RPC address not specified: pass argument --rpc-addr or specify "rpc_addr" field in CONFIG file|}
  | (_, _, Some err) -> fail err
  | (Some endpoint, Some rpc_addr, None) ->
      let* (rpc_server_address, rpc_server_port) =
        address_and_port_for_runtime rpc_addr
      in
      let* rpc_server_tls =
        Option.map tls_for_runtime rpc_tls |> opt_res_to_res_opt
      in
      Ok
        {
          endpoint;
          rpc_server_address;
          rpc_server_port;
          rpc_server_tls;
          sym_block_caching_time;
          data_dir;
        }
