(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Clic
open Protocol.Alpha_context

let get_sc_rollup_addresses_command () =
  command
    ~desc:
      "Retrieve the smart-contract rollup address the node is interacting with."
    no_options
    (fixed ["get"; "sc"; "rollup"; "address"])
    (fun () (cctxt : #Configuration.sc_client_context) ->
      RPC.get_sc_rollup_addresses_command cctxt >>=? fun addr ->
      cctxt#message "@[%a@]" Sc_rollup.Address.pp addr >>= fun () -> return_unit)

(** [display_answer cctxt answer] will print an RPC answer to the screen. *)
let display_answer (cctxt : #Configuration.sc_client_context) :
    RPC_context.generic_call_result -> unit Lwt.t = function
  | `Json (`Ok json) -> cctxt#answer "%a" Json_repr.(pp (module Ezjsonm)) json
  | `Binary (`Ok binary) -> cctxt#answer "%a" Hex.pp (Hex.of_string binary)
  | `Json (`Error (Some error)) ->
      cctxt#error
        "@[<v 2>Command failed: @[%a@]@]@."
        (Format.pp_print_list Error_monad.pp)
        (Data_encoding.Json.destruct
           (Data_encoding.list Error_monad.error_encoding)
           error)
  | `Binary (`Error (Some error)) -> (
      match Data_encoding.Binary.of_string Error_monad.trace_encoding error with
      | Ok trace ->
          cctxt#error
            "@[<v 2>Command failed: @[%a@]@]@."
            Error_monad.pp_print_trace
            trace
      | Error msg ->
          cctxt#error
            "@[<v 2>Error whilst decoding the server response: @[%a@]@]@."
            Data_encoding.Binary.pp_read_error
            msg)
  | `Json (`Not_found _) | `Binary (`Not_found _) | `Other (_, `Not_found _) ->
      cctxt#error "No service found at this URL\n%!"
  | `Json (`Gone _) | `Binary (`Gone _) | `Other (_, `Gone _) ->
      cctxt#error
        "Requested data concerns a pruned block and target resource is no \
         longer available\n\
         %!"
  | `Json (`Unauthorized _)
  | `Binary (`Unauthorized _)
  | `Other (_, `Unauthorized _) ->
      cctxt#error "@[<v 2>[HTTP 403] Access denied to: %a@]@." Uri.pp cctxt#base
  | _ -> cctxt#error "Unexpected server answer\n%!"

(** [call_get cctxt raw_url] executes a GET RPC call against the [raw_url]. *)
let call_get (cctxt : #Configuration.sc_client_context) raw_url =
  let open Lwt_result_syntax in
  let meth = `GET in
  let uri = Uri.of_string raw_url in
  let* answer = cctxt#generic_media_type_call meth uri in
  let*! () = display_answer cctxt answer in
  return_unit

let rpc_get =
  command
    ~desc:"Call an RPC with the GET method."
    no_options
    (prefixes ["rpc"; "get"] @@ string ~name:"url" ~desc:"the RPC URL" @@ stop)
    (fun () url cctxt -> call_get cctxt url)

let all () = [get_sc_rollup_addresses_command (); rpc_get]
