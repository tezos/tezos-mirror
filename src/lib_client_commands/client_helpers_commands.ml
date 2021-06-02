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

let unique_switch =
  Clic.switch
    ~long:"unique"
    ~short:'u'
    ~doc:"Fail when there is more than one possible completion."
    ()

let commands () =
  Clic.
    [
      command
        ~desc:
          "Autocomplete a prefix of Base58Check-encoded hash.\n\
           This actually works only for blocks, operations, public key and \
           contract identifiers."
        (args1 unique_switch)
        (prefixes ["complete"]
        @@ string ~name:"prefix" ~desc:"the prefix of the hash to complete"
        @@ stop)
        (fun unique prefix (cctxt : #Client_context.full) ->
          Shell_services.Blocks.Helpers.complete
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            prefix
          >>=? fun completions ->
          match completions with
          | [] -> Stdlib.exit 3
          | _ :: _ :: _ when unique -> Stdlib.exit 3
          | completions ->
              List.iter print_endline completions ;
              return_unit);
      command
        ~desc:"Wait for the node to be bootstrapped."
        no_options
        (prefixes ["bootstrapped"] @@ stop)
        (fun () (cctxt : #Client_context.full) ->
          Client_confirmations.wait_for_bootstrapped cctxt);
      command
        ~desc:"Computes the chain id corresponding to a block hash."
        no_options
        (prefixes ["compute"; "chain"; "id"; "from"; "block"; "hash"]
        @@ string
             ~name:"hash"
             ~desc:"the block hash from which to compute the chain id"
        @@ stop)
        (fun () block_hash_str (cctxt : #Client_context.full) ->
          Lwt.return (Tezos_crypto.Block_hash.of_b58check block_hash_str)
          >>=? fun block_hash ->
          let chain_id = Tezos_crypto.Chain_id.of_block_hash block_hash in
          cctxt#message "%a" Tezos_crypto.Chain_id.pp chain_id >>= fun () ->
          return_unit);
      command
        ~desc:"Computes a chain id from a seed"
        no_options
        (prefixes ["compute"; "chain"; "id"; "from"; "seed"]
        @@ string
             ~name:"string"
             ~desc:"the seed from which to compute the chain id"
        @@ stop)
        (fun () seed_str (cctxt : #Client_context.full) ->
          let chain_id =
            Tezos_crypto.Chain_id.hash_bytes [Bytes.of_string seed_str]
          in
          cctxt#message "%a" Tezos_crypto.Chain_id.pp chain_id >>= fun () ->
          return_unit);
    ]
