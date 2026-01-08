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

open Client_proto_utils

let group = {Tezos_clic.name = "utilities"; title = "Utility Commands"}

let commands () =
  let open Tezos_clic in
  let string_param ~name ~desc =
    param ~name ~desc Client_proto_args.string_parameter
  in
  let block_arg =
    default_arg
      ~long:"branch"
      ~short:'b'
      ~placeholder:"hash|tag"
      ~doc:
        "Block hash used to create the no-op operation to sign (possible tags \
         are 'head' and 'genesis'). Defaults to 'genesis'. Note that the the \
         genesis block hash is network-dependent."
      ~default:(Block_services.to_string `Genesis)
      (Client_config.block_parameter ())
  in
  [
    command
      ~group
      ~desc:
        "Sign a message and display it using the failing_noop operation. This \
         operation is not executable in the protocol. Please note that \
         signing/checking an arbitrary message in itself is not sufficient to \
         verify a key ownership"
      (args1 block_arg)
      (prefixes ["sign"; "message"]
      @@ string_param ~name:"message" ~desc:"message to sign"
      @@ prefixes ["for"]
      @@ Client_keys_v0.Secret_key.source_param
           ~name:"src"
           ~desc:"name of the signer contract"
      @@ stop)
      (fun block_head message src_sk cctxt ->
        Shell_services.Blocks.hash cctxt ~chain:cctxt#chain ~block:block_head ()
        >>=? fun block ->
        sign_message cctxt ~src_sk ~block ~message >>=? fun signature ->
        cctxt#message "Signature: %a" Tezos_crypto.Signature.V0.pp signature
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
      (prefixes ["check"; "that"; "message"]
      @@ string_param ~name:"message" ~desc:"signed message"
      @@ prefixes ["was"; "signed"; "by"]
      @@ Client_keys_v0.Public_key.alias_param
           ~name:"signer"
           ~desc:"name of the signer contract"
      @@ prefixes ["to"; "produce"]
      @@ param
           ~name:"signature"
           ~desc:"the signature to check"
           Client_proto_args.signature_parameter
      @@ stop)
      (fun (block_head, quiet)
           message
           (_, (key_locator, _))
           signature
           (cctxt : #Protocol_client_context.full)
         ->
        Shell_services.Blocks.hash cctxt ~chain:cctxt#chain ~block:block_head ()
        >>=? fun block ->
        check_message cctxt ~key_locator ~block ~quiet ~message ~signature
        >>=? function
        | false -> cctxt#error "invalid signature"
        | true ->
            if quiet then return_unit
            else
              cctxt#message "Signature check successful" >>= fun () ->
              return_unit);
  ]
