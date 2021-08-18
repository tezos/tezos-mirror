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

module Events = Delegate_events.Revelation

let inject_seed_nonce_revelation (cctxt : #Protocol_client_context.full) ~chain
    ~block ?async nonces =
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  match nonces with
  | [] -> Events.(emit no_nonce_reveal) hash >>= fun () -> return_unit
  | _ ->
      List.iter_es
        (fun (level, nonce) ->
          Plugin.RPC.Forge.seed_nonce_revelation
            cctxt
            (chain, block)
            ~branch:hash
            ~level
            ~nonce
            ()
          >>=? fun bytes ->
          let bytes = Signature.concat bytes Signature.zero in
          Shell_services.Injection.operation cctxt ?async ~chain bytes
          >>=? fun oph ->
          Events.(emit reveal_nonce)
            ( nonce,
              level,
              Block_services.chain_to_string chain,
              Block_services.to_string block,
              oph )
          >>= fun () -> return_unit)
        nonces
