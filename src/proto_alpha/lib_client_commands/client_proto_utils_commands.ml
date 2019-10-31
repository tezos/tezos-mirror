(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.ch>                      *)
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

open Protocol
open Alpha_context
open Client_proto_contracts
open Client_proto_utils

let group = {Clic.name = "utilities"; title = "Utility Commands"}

let commands () =
  let open Clic in
  let string_parameter ~name ~desc =
    param ~name ~desc Client_proto_args.string_parameter
  in
  let signature_parameter =
    parameter (fun _cctxt s ->
        match Signature.of_b58check_opt s with
        | Some s ->
            return s
        | None ->
            failwith "Not given a valid signature")
  in
  let block_arg =
    default_arg
      ~long:"block"
      ~short:'b'
      ~placeholder:"hash|tag"
      ~doc:
        "block on which to apply contextual commands (possible tags are \
         'head' and 'genesis')"
      ~default:(Block_services.to_string `Genesis)
      (Client_config.block_parameter ())
  in
  [ command
      ~group
      ~desc:
        "Sign a message and display it using the failing_noop operation. This \
         operation is not executable in the protocol. Please note that \
         signing/checking an arbitrary message in itself is not sufficient to \
         verify a key ownership"
      (args1 block_arg)
      ( prefixes ["sign"; "message"]
      @@ string_parameter ~name:"message" ~desc:"message to sign"
      @@ prefixes ["for"]
      @@ ContractAlias.alias_param
           ~name:"src"
           ~desc:"name of the signer contract"
      @@ stop )
      (fun block_head message (_, contract) cctxt ->
        match Contract.is_implicit contract with
        | None ->
            failwith "only implicit accounts can sign"
        | Some source ->
            Shell_services.Blocks.hash
              cctxt
              ~chain:cctxt#chain
              ~block:block_head
              ()
            >>=? fun block ->
            Client_keys.get_key cctxt source
            >>=? fun (_, _, src_sk) ->
            sign_message cctxt ~src_sk ~block ~message
            >>=? fun signature ->
            cctxt#message "Signature: %a" Signature.pp signature
            >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Check the signature of an arbitrary message using the failing_noop \
         operation. Please note that signing/checking an arbitrary message in \
         itself is not sufficient to verify a key ownership."
      (args2
         block_arg
         (switch ~doc:"Use only exit codes" ~short:'q' ~long:"quiet" ()))
      ( prefixes ["check"; "that"; "message"]
      @@ string_parameter ~name:"message" ~desc:"signed message"
      @@ prefixes ["was"; "signed"; "by"]
      @@ ContractAlias.destination_param
           ~name:"signer"
           ~desc:"name of the signer contract"
      @@ prefixes ["to"; "produce"]
      @@ param
           ~name:"signature"
           ~desc:"the signature to check"
           signature_parameter
      @@ stop )
      (fun (block_head, quiet)
           message
           (_, contract)
           signature
           (cctxt : #Protocol_client_context.full) ->
        match Contract.is_implicit contract with
        | None ->
            failwith "only implicit accounts can sign"
        | Some source -> (
            Shell_services.Blocks.hash
              cctxt
              ~chain:cctxt#chain
              ~block:block_head
              ()
            >>=? fun block ->
            Client_keys.get_public_key cctxt source
            >>=? fun (_, src_pk) ->
            check_message cctxt ~src_pk ~block ~quiet ~message ~signature
            >>=? function
            | false ->
                cctxt#error "invalid signature"
            | true ->
                if quiet then return_unit
                else
                  cctxt#message "Signature check successful"
                  >>= fun () -> return_unit )) ]
