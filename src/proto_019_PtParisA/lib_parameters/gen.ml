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

(* Prints the json encoding of the parametric constants of protocol alpha.
   $ dune utop src/proto_alpha/lib_protocol/test/helpers/ constants.ml
*)

let () =
  let print_usage_and_fail s =
    Printf.eprintf
      "Usage: %s [ --sandbox | --test | --mainnet | --mainnet-with-chain-id ]"
      Sys.argv.(0) ;
    raise (Invalid_argument s)
  in
  let dump ?chain_id parameters file =
    let str =
      Data_encoding.Json.to_string
        (Default_parameters.json_of_parameters ?chain_id parameters)
    in
    let fd = open_out file in
    output_string fd str ;
    close_out fd
  in
  if Array.length Sys.argv < 2 then print_usage_and_fail ""
  else
    match Sys.argv.(1) with
    | "--sandbox" ->
        dump
          Default_parameters.(parameters_of_constants constants_sandbox)
          "sandbox-parameters.json"
    | "--test" ->
        dump
          Default_parameters.(
            parameters_of_constants
              ~commitments:(Lazy.force test_commitments)
              constants_sandbox)
          "test-parameters.json"
    | "--mainnet" ->
        dump
          Default_parameters.(parameters_of_constants constants_mainnet)
          "mainnet-parameters.json"
    | "--mainnet-with-chain-id" ->
        dump
          ~chain_id:Protocol.Alpha_context.Constants.mainnet_id
          Default_parameters.(parameters_of_constants constants_mainnet)
          "mainnet-with-chain-id-parameters.json"
    | s -> print_usage_and_fail s
