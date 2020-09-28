(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Local_services

type rpc_error =
  | Rpc_generic_error
  | Rpc_not_found of string option
  | Rpc_unauthorized
  | Rpc_unexpected_type_of_failure
  | Rpc_cannot_parse_path
  | Rpc_cannot_parse_query
  | Rpc_cannot_parse_body
  | Rpc_streams_not_handled

type error += Not_implemented_in_local_RPC_mode of Uri.t

type error += Local_RPC_error of rpc_error

let rpc_error_encoding =
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"Rpc_generic_error"
        (obj1 (req "kind" (constant "generic_error")))
        (function Rpc_generic_error -> Some () | _ -> None)
        (fun () -> Rpc_generic_error);
      case
        (Tag 1)
        ~title:"Rpc_not_found"
        (obj2
           (req "kind" (constant "rpc_not_found"))
           (opt "description" string))
        (function Rpc_not_found m -> Some ((), m) | _ -> None)
        (fun ((), m) -> Rpc_not_found m);
      case
        (Tag 2)
        ~title:"Rpc_unauthorized"
        (obj1 (req "kind" (constant "rpc_unauthorized")))
        (function Rpc_unauthorized -> Some () | _ -> None)
        (fun () -> Rpc_unauthorized);
      case
        (Tag 3)
        ~title:"Rpc_unexpected_type_of_failure"
        (obj1 (req "kind" (constant "rpc_unexpected_failure")))
        (function Rpc_unexpected_type_of_failure -> Some () | _ -> None)
        (fun () -> Rpc_unexpected_type_of_failure);
      case
        (Tag 4)
        ~title:"Rpc_cannot_parse_path"
        (obj1 (req "kind" (constant "rpc_cannot_parse")))
        (function Rpc_cannot_parse_path -> Some () | _ -> None)
        (fun () -> Rpc_cannot_parse_path);
      case
        (Tag 5)
        ~title:"Rpc_cannot_parse_query"
        (obj1 (req "kind" (constant "rpc_cannot_query")))
        (function Rpc_cannot_parse_query -> Some () | _ -> None)
        (fun () -> Rpc_cannot_parse_query);
      case
        (Tag 6)
        ~title:"Rpc_cannot_parse_body"
        (obj1 (req "kind" (constant "rpc_cannot_body")))
        (function Rpc_cannot_parse_body -> Some () | _ -> None)
        (fun () -> Rpc_cannot_parse_body);
      case
        (Tag 7)
        ~title:"Rpc_streams_not_handled"
        (obj1 (req "kind" (constant "rpc_streams_not_handled")))
        (function Rpc_streams_not_handled -> Some () | _ -> None)
        (fun () -> Rpc_streams_not_handled) ]

class local_ctxt (base_dir : string) (mem_only : bool)
  (mockup_env : Registration.mockup_environment) (chain_id : Chain_id.t)
  (rpc_context : Tezos_protocol_environment.rpc_context) : RPC_context.json =
  let write_context rpc_context =
    let (module Mockup_environment) = mockup_env in
    Persistence.overwrite_mockup
      ~chain_id
      ~protocol_hash:Mockup_environment.protocol_hash
      ~rpc_context
      ~base_dir
  in
  let directory =
    let (module Mockup_environment) = mockup_env in
    let proto_directory =
      (* register protocol-specific RPCs *)
      Directory.prefix
        Tezos_shell_services.Chain_services.path
        (Directory.prefix
           Tezos_shell_services.Block_services.path
           (Directory.map
              (fun (_chain, _block) -> Lwt.return rpc_context)
              Mockup_environment.directory))
    in
    let shell_directory =
      let (module Mockup_environment) = mockup_env in
      Local_services.build_shell_directory
        mockup_env
        chain_id
        rpc_context
        mem_only
        write_context
    in
    let base = Directory.merge shell_directory proto_directory in
    RPC_directory.register_describe_directory_service
      base
      RPC_service.description_service
  in
  let rpc_dir_ctxt = new Tezos_rpc.RPC_context.of_directory directory in
  let generic_json_call_stub :
      RPC_service.meth ->
      ?body:Data_encoding.json ->
      Uri.t ->
      (Data_encoding.json, Data_encoding.json option) RPC_context.rest_result
      Lwt.t =
   fun meth ?body uri ->
    (* This code is a ripoff of resto-cohttp-server/server.ml/callback *)
    (* let uri = Cohttp.Request.uri req in *)
    let path = Uri.pct_decode (Uri.path uri) in
    let path = Resto_cohttp.Utils.split_path path in
    Directory.lookup directory () meth path
    >>= fun result ->
    match result with
    | Error (`Cannot_parse_path _) ->
        Error_monad.fail (Local_RPC_error Rpc_cannot_parse_path)
    | Error (`Method_not_allowed _) ->
        Error_monad.fail (Local_RPC_error Rpc_unauthorized)
    | Error `Not_found ->
        Error_monad.fail (Local_RPC_error (Rpc_not_found None)) (*TODO*)
    | Ok (Directory.Service s) -> (
        ( match
            Resto.Query.parse
              s.types.query
              (List.map
                 (fun (k, l) -> (k, String.concat "," l))
                 (Uri.query uri))
          with
        | exception Resto.Query.Invalid _s ->
            Error_monad.fail (Local_RPC_error Rpc_cannot_parse_query)
        | query ->
            return query )
        >>=? fun query ->
        let output_encoding = s.types.output in
        let output = Data_encoding.Json.construct output_encoding in
        let error = Option.map (Data_encoding.Json.construct s.types.error) in
        ( match s.types.input with
        | Service.No_input ->
            s.handler query () >>= return
        | Service.Input input -> (
            let body =
              Option.map
                (fun b ->
                  Cohttp_lwt.Body.of_string (Data_encoding.Json.to_string b))
                body
            in
            let body = Option.value ~default:Cohttp_lwt.Body.empty body in
            Cohttp_lwt.Body.to_string body
            >>= fun body ->
            match Tezos_rpc_http.Media_type.json.destruct input body with
            | Error _s ->
                Error_monad.fail (Local_RPC_error Rpc_cannot_parse_body)
            | Ok body ->
                s.handler query body >>= return ) )
        >>=? function
        | `Ok o ->
            let body = output o in
            return (`Ok body)
        | `Error e ->
            let body = error e in
            return (`Error body)
        | `Not_found e ->
            let body = error e in
            return (`Not_found body)
        | `Unauthorized e ->
            let body = error e in
            return (`Unauthorized body)
        | `Forbidden e ->
            let body = error e in
            return (`Forbidden body)
        | `Conflict e ->
            let body = error e in
            return (`Conflict body)
        | `OkStream _ostream ->
            Error_monad.fail (Local_RPC_error Rpc_streams_not_handled)
        | `Created _ | `No_content | `Gone _ ->
            Error_monad.fail (Local_RPC_error Rpc_generic_error) )
  in
  object
    method base = Uri.empty

    method call_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) RPC_service.t -> 'p ->
          'q -> 'i -> 'o tzresult Lwt.t =
      fun (type p q i o)
          (service : (_, _, p, q, i, o, _) Service.t)
          (params : p)
          (query : q)
          (input : i) ->
        try rpc_dir_ctxt#call_service service params query input
        with Not_found ->
          let description = print_service service in
          fail (Local_RPC_error (Rpc_not_found (Some description)))

    method call_streamed_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
          on_chunk:('o -> unit) -> on_close:(unit -> unit) -> 'p -> 'q -> 'i ->
          (unit -> unit) tzresult Lwt.t =
      rpc_dir_ctxt#call_streamed_service

    method generic_json_call = generic_json_call_stub
  end

let () =
  register_error_kind
    `Permanent
    ~id:"local_rpc_client.request_failed"
    ~title:"Local mockup RPC request failure"
    ~description:"An RPC request failed in mockup mode"
    ~pp:(fun ppf error ->
      Format.fprintf ppf "A mockup RPC request failed" ;
      match error with
      | Rpc_generic_error ->
          ()
      | Rpc_not_found None ->
          Format.fprintf ppf ": RPC not found"
      | Rpc_not_found (Some desc) ->
          Format.fprintf ppf ": RPC not found (%s)" desc
      | Rpc_unauthorized ->
          Format.fprintf ppf ": RPC unauthorized"
      | Rpc_unexpected_type_of_failure ->
          Format.fprintf ppf ": unexpected type of failure"
      | Rpc_cannot_parse_path ->
          Format.fprintf ppf ": cannot parse path"
      | Rpc_cannot_parse_query ->
          Format.fprintf ppf ": cannot parse query"
      | Rpc_cannot_parse_body ->
          Format.fprintf ppf ": cannot parse body"
      | Rpc_streams_not_handled ->
          Format.fprintf ppf ": streamed RPCs not handled yet")
    (let open Data_encoding in
    obj1 (req "rpc_error" rpc_error_encoding))
    (function Local_RPC_error rpc_error -> Some rpc_error | _ -> None)
    (fun rpc_error -> Local_RPC_error rpc_error)

let () =
  register_error_kind
    `Permanent
    ~id:"local_rpc_client.not_implemented_in_local_mode"
    ~title:"Local mockup RPC request not implemented"
    ~description:"A specific RPC is not implemented in mockup mode"
    ~pp:(fun ppf (uri : Uri.t) ->
      Format.fprintf ppf "RPC not implemented in mockup mode: %a" Uri.pp uri)
    Data_encoding.(obj1 (req "uri" RPC_encoding.uri_encoding))
    (function Not_implemented_in_local_RPC_mode uri -> Some uri | _ -> None)
    (fun uri -> Not_implemented_in_local_RPC_mode uri)
