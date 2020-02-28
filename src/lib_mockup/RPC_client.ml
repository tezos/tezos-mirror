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

type error += Not_implemented_in_local_RPC_mode

type error += Local_RPC_error of rpc_error

class local_ctxt (base_dir : string) (mem_only : bool)
  (mockup_env : Registration.mockup_environment)
  (rpc_context : Tezos_protocol_environment.rpc_context) : RPC_context.json =
  let write_context rpc_context =
    let (module Mockup_environment) = mockup_env in
    Persistence.overwrite_mockup
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
        rpc_context
        mem_only
        write_context
    in
    Directory.merge shell_directory proto_directory
  in
  let call_service_local (type p q i o)
      (service : (_, _, p, q, i, o, _) Service.t) (params : p) (query : q)
      (input : i) : o tzresult Lwt.t =
    Directory.transparent_lookup directory service params query input
    >>= fun res ->
    match res with
    | `Ok output ->
        return output
    | `Error (Some err) ->
        Lwt.return (Error err)
    | `Not_found (Some err) ->
        Lwt.return (Error err)
    | `Unauthorized (Some err) ->
        Lwt.return (Error err)
    | `Error None ->
        fail (Local_RPC_error Rpc_generic_error)
    | `Not_found None ->
        let path = print_service service in
        fail (Local_RPC_error (Rpc_not_found (Some path)))
    | `Unauthorized None ->
        fail (Local_RPC_error Rpc_unauthorized)
    | _ ->
        fail (Local_RPC_error Rpc_unexpected_type_of_failure)
  in
  let call_streamed_service_local :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
        on_chunk:('o -> unit) -> on_close:(unit -> unit) -> 'p -> 'q -> 'i ->
        (unit -> unit) tzresult Lwt.t =
   fun service ~on_chunk ~on_close params query input ->
    call_service_local service params query input
    >>=? fun result ->
    on_chunk result ;
    on_close () ;
    return (fun () -> ())
  in
  let generic_json_call_stub :
      RPC_service.meth ->
      ?body:Data_encoding.json ->
      Uri.t ->
      (Data_encoding.json, Data_encoding.json option) RPC_context.rest_result
      Lwt.t =
   fun meth ?body uri ->
    ignore (meth, body, uri) ;
    Error_monad.fail Not_implemented_in_local_RPC_mode
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
        try call_service_local service params query input
        with Not_found ->
          let description = print_service service in
          fail (Local_RPC_error (Rpc_not_found (Some description)))

    method call_streamed_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) RPC_service.t ->
          on_chunk:('o -> unit) -> on_close:(unit -> unit) -> 'p -> 'q -> 'i ->
          (unit -> unit) tzresult Lwt.t =
      call_streamed_service_local

    method generic_json_call = generic_json_call_stub
  end

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
        (fun () -> Rpc_unexpected_type_of_failure) ]

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
          Format.fprintf ppf ": unexpected type of failure")
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
    ~pp:(fun ppf () -> Format.fprintf ppf "RPC not implemented in mockup mode")
    Data_encoding.empty
    (function Not_implemented_in_local_RPC_mode -> Some () | _ -> None)
    (fun () -> Not_implemented_in_local_RPC_mode)
