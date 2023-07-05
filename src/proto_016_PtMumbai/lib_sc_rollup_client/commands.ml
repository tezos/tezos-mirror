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

open Protocol.Alpha_context

let possible_block_ids = ["head"; "finalized"; "cemented"; "<hash>"; "<level>"]

let block_arg =
  Tezos_clic.default_arg
    ~long:"block"
    ~short:'B'
    ~placeholder:"block"
    ~default:"head"
    ~doc:
      (Format.sprintf
         "The block identifier for which the command applies (possible values: \
          %s)."
         (String.concat ", " possible_block_ids))
    (Tezos_clic.parameter
       (fun _ s ->
         match Sc_rollup_services.Arg.destruct_block_id s with
         | Ok b -> return b
         | Error reason -> failwith "Invalid block id: %s" reason)
       ~autocomplete:(fun _ -> return possible_block_ids))

let get_sc_rollup_addresses_command () =
  Tezos_clic.command
    ~desc:"Retrieve the smart rollup address the node is interacting with."
    Tezos_clic.no_options
    (Tezos_clic.fixed ["get"; "smart"; "rollup"; "address"])
    (fun () (cctxt : #Configuration.sc_client_context) ->
      RPC.get_sc_rollup_addresses_command cctxt >>=? fun addr ->
      cctxt#message "@[%a@]" Sc_rollup.Address.pp addr >>= fun () -> return_unit)

let get_state_value_command () =
  Tezos_clic.command
    ~desc:"Observe a key in the PVM state."
    (Tezos_clic.args1 block_arg)
    (Tezos_clic.prefixes ["get"; "state"; "value"; "for"]
    @@ Tezos_clic.string ~name:"key" ~desc:"The key of the state value"
    @@ Tezos_clic.stop)
    (fun block key (cctxt : #Configuration.sc_client_context) ->
      RPC.get_state_value_command cctxt block key >>=? fun bytes ->
      cctxt#message "@[%S@]" (String.of_bytes bytes) >>= fun () -> return_unit)

(** [display_answer cctxt answer] prints an RPC answer. *)
let display_answer (cctxt : #Configuration.sc_client_context) :
    Tezos_rpc.Context.generic_call_result -> unit Lwt.t = function
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

let unparsed_transaction_encoding =
  let open Data_encoding in
  obj3
    (req "parameters" string)
    (req "destination" Contract.originated_encoding)
    (opt "entrypoint" Entrypoint.simple_encoding)

type unparsed_batch =
  (string * Protocol.Contract_hash.t * Entrypoint.t option) list

let unparsed_batch_encoding : unparsed_batch Data_encoding.t =
  Data_encoding.list unparsed_transaction_encoding

let expand_expr expr =
  let open Result_syntax in
  let* expr =
    Michelson_v1_parser.parse_expression expr
    |> Tezos_micheline.Micheline_parser.no_parsing_error
  in
  return expr.expanded

let parse_unparsed_batch json =
  let open Lwt_result_syntax in
  let transactions = Data_encoding.Json.destruct unparsed_batch_encoding json in
  let* transactions =
    List.map_es
      (fun (unparsed_parameters, destination, entrypoint) ->
        let*? unparsed_parameters = expand_expr unparsed_parameters in
        return
          Sc_rollup.Outbox.Message.
            {
              unparsed_parameters;
              destination;
              entrypoint = Option.value ~default:Entrypoint.default entrypoint;
            })
      transactions
  in
  return (Sc_rollup.Outbox.Message.Atomic_transaction_batch {transactions})

let outbox_message_parameter =
  Tezos_clic.parameter (fun (cctxt : #Configuration.sc_client_context) str ->
      match Data_encoding.Json.from_string str with
      | Ok json -> parse_unparsed_batch json
      | Error reason -> cctxt#error "Invalid transaction json: %s" reason)

let get_output_proof () =
  Tezos_clic.command
    ~desc:"Ask the rollup node for an output proof."
    Tezos_clic.no_options
    (Tezos_clic.prefixes ["get"; "proof"; "for"; "message"]
    @@ Tezos_clic.string
         ~name:"index"
         ~desc:"The index of the message in the outbox"
    @@ Tezos_clic.prefixes ["of"; "outbox"; "at"; "level"]
    @@ Tezos_clic.string
         ~name:"level"
         ~desc:"The level of the rollup outbox where the message is available"
    @@ Tezos_clic.prefixes ["transferring"]
    @@ Tezos_clic.param
         ~name:"transactions"
         ~desc:"A JSON description of the transactions"
         outbox_message_parameter
    @@ Tezos_clic.stop)
    (fun () index level message (cctxt : #Configuration.sc_client_context) ->
      let open Lwt_result_syntax in
      let output =
        Protocol.Alpha_context.Sc_rollup.
          {
            message_index = Z.of_string index;
            outbox_level = Raw_level.of_int32_exn (Int32.of_string level);
            message;
          }
      in
      RPC.get_outbox_proof cctxt output >>=? fun (commitment_hash, proof) ->
      cctxt#message
        {|@[{ "proof" : "0x%a", "commitment_hash" : "%a"@]}|}
        Hex.pp
        (Hex.of_string proof)
        Protocol.Alpha_context.Sc_rollup.Commitment.Hash.pp
        commitment_hash
      >>= fun () -> return_unit)

let get_output_message_encoding () =
  Tezos_clic.command
    ~desc:"Get output message encoding."
    Tezos_clic.no_options
    (Tezos_clic.prefixes ["encode"; "outbox"; "message"]
    @@ Tezos_clic.param
         ~name:"transactions"
         ~desc:"A JSON description of the transactions"
         outbox_message_parameter
    @@ Tezos_clic.stop)
    (fun () message (cctxt : #Configuration.sc_client_context) ->
      let open Lwt_result_syntax in
      let open Protocol.Alpha_context.Sc_rollup.Outbox.Message in
      let encoded_message = serialize message in
      (match encoded_message with
      | Ok encoded_message ->
          let encoded_message = unsafe_to_string encoded_message in
          cctxt#message "%a" Hex.pp (Hex.of_string encoded_message)
      | Error _ -> cctxt#message "Error while encoding outbox message.")
      >>= fun () -> return_unit)

let call ?body meth raw_url (cctxt : #Configuration.sc_client_context) =
  let open Lwt_result_syntax in
  let uri = Uri.of_string raw_url in
  let body =
    (* This code is similar to a piece of code in [fill_in]
       function. An RPC is declared as POST, PATCH or PUT, but the
       body is not given. In that case, the body should be an empty
       JSON object. *)
    match (meth, body) with
    | _, Some _ -> body
    | `DELETE, None | `GET, None -> None
    | `PATCH, None | `PUT, None | `POST, None -> Some (`O [])
  in
  let* answer = cctxt#generic_media_type_call ?body meth uri in
  let*! () = display_answer cctxt answer in
  return_unit

let call_with_json meth raw_url json (cctxt : #Configuration.sc_client_context)
    =
  match Ezjsonm.value_from_string_result json with
  | Error err ->
      cctxt#error
        "Failed to parse the provided json: %s\n%!"
        (Ezjsonm.read_error_description err)
  | Ok body -> call meth ~body raw_url cctxt

let call_with_file_or_json meth url maybe_file
    (cctxt : #Configuration.sc_client_context) =
  let open Lwt_result_syntax in
  let* json =
    match TzString.split ':' ~limit:1 maybe_file with
    | ["file"; filename] ->
        Lwt.catch
          (fun () ->
            Lwt_result.ok @@ Lwt_io.(with_file ~mode:Input filename read))
          (fun exn -> failwith "cannot read file (%s)" (Printexc.to_string exn))
    | _ -> return maybe_file
  in
  call_with_json meth url json cctxt

let rpc_commands () =
  let open Tezos_clic in
  let group = {name = "rpc"; title = "Commands for the low level RPC layer"} in
  [
    command
      ~group
      ~desc:"Call an RPC with the GET method."
      no_options
      (prefixes ["rpc"; "get"] @@ string ~name:"url" ~desc:"the RPC URL" @@ stop)
      (fun () -> call `GET);
    command
      ~group
      ~desc:"Call an RPC with the POST method."
      no_options
      (prefixes ["rpc"; "post"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ stop)
      (fun () -> call `POST);
    command
      ~group
      ~desc:
        "Call an RPC with the POST method, providing input data via the \
         command line."
      no_options
      (prefixes ["rpc"; "post"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ prefix "with"
      @@ string
           ~name:"input"
           ~desc:
             "the raw JSON input to the RPC\n\
              For instance, use `{}` to send the empty document.\n\
              Alternatively, use `file:path` to read the JSON data from a file."
      @@ stop)
      (fun () -> call_with_file_or_json `POST);
  ]

module Keys = struct
  open Tezos_client_base.Client_keys

  let generate_keys () =
    Tezos_clic.command
      ~desc:"Generate a pair of keys."
      (Tezos_clic.args1 (Secret_key.force_switch ()))
      (Tezos_clic.prefixes ["gen"; "unencrypted"; "keys"]
      @@ Aggregate_alias.Secret_key.fresh_alias_param @@ Tezos_clic.stop)
      (fun force name (cctxt : #Configuration.sc_client_context) ->
        Client_keys_commands.Bls_commands.generate_keys
          ~force
          ~encrypted:false
          name
          cctxt)

  let list_keys () =
    Tezos_clic.command
      ~desc:"List keys."
      Tezos_clic.no_options
      (Tezos_clic.prefixes ["list"; "keys"] @@ Tezos_clic.stop)
      (fun () (cctxt : #Configuration.sc_client_context) ->
        Client_keys_commands.Bls_commands.list_keys cctxt)

  let show_address () =
    Tezos_clic.command
      ~desc:"Show the keys associated with an account."
      Tezos_clic.no_options
      (Tezos_clic.prefixes ["show"; "address"]
      @@ Aggregate_alias.Public_key_hash.alias_param @@ Tezos_clic.stop)
      (fun () (name, _pkh) (cctxt : #Configuration.sc_client_context) ->
        Client_keys_commands.Bls_commands.show_address
          ~show_private:true
          name
          cctxt)

  let import_secret_key () =
    Tezos_clic.command
      ~desc:"Add a secret key to the wallet."
      (Tezos_clic.args1 (Aggregate_alias.Secret_key.force_switch ()))
      (Tezos_clic.prefixes ["import"; "secret"; "key"]
      @@ Aggregate_alias.Secret_key.fresh_alias_param @@ aggregate_sk_uri_param
      @@ Tezos_clic.stop)
      (fun force name sk_uri (cctxt : #Configuration.sc_client_context) ->
        Client_keys_commands.Bls_commands.import_secret_key
          ~force
          name
          sk_uri
          cctxt)
end

let all () =
  [
    get_sc_rollup_addresses_command ();
    get_state_value_command ();
    get_output_proof ();
    get_output_message_encoding ();
    Keys.generate_keys ();
    Keys.list_keys ();
    Keys.show_address ();
    Keys.import_secret_key ();
  ]
  @ rpc_commands ()
