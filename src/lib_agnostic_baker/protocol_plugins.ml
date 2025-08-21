(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type error += Unsupported_protocol of Protocol_hash.t

let () =
  register_error_kind
    ~id:"agnostic_baker.unsupported_protocol"
    ~title:"Protocol not supported by agnostic baker"
    ~description:"Protocol not supported by agnostic baker."
    ~pp:(fun ppf proto ->
      Format.fprintf
        ppf
        "Protocol %a is not supported by the agnostic baker."
        Protocol_hash.pp
        proto)
    `Permanent
    Data_encoding.(obj1 (req "protocol" Protocol_hash.encoding))
    (function Unsupported_protocol p -> Some p | _ -> None)
    (fun p -> Unsupported_protocol p)

type proto_plugin = (module Protocol_plugin_sig.S)

let proto_plugins : proto_plugin Protocol_hash.Table.t =
  Protocol_hash.Table.create 7

let last_registered = ref None

let register (plugin : proto_plugin) =
  let module Plugin = (val plugin) in
  if Protocol_hash.Table.mem proto_plugins Plugin.protocol_hash then
    Format.kasprintf
      invalid_arg
      "The agnostic baker protocol plugin for protocol %a is already \
       registered. Did you register it manually multiple times?"
      Protocol_hash.pp
      Plugin.protocol_hash ;
  last_registered := Some Plugin.protocol_hash ;
  Protocol_hash.Table.add proto_plugins Plugin.protocol_hash plugin

let registered_protocols () =
  Protocol_hash.Table.to_seq_keys proto_plugins |> List.of_seq

let last_registered () =
  match !last_registered with
  | None -> Stdlib.failwith "No protocol plugins registered"
  | Some p -> p

let proto_plugin_for_protocol protocol =
  Protocol_hash.Table.find proto_plugins protocol
  |> Option.to_result ~none:[Unsupported_protocol protocol]
