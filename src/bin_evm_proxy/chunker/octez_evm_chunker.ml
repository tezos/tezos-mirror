(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** Chunk transactions
    -------
    Component:    EVM kernel
    Invocation:   dune exec octez_evm_chunker.exe rollup_address raw_tx
                    - rollup_address : string in sr1... format. If the address
                    is omitted then the zero address will be used.
                    - raw_tx : string in hex format (with or without 0x prefix)
    Subject:      Split a raw transactions into chunks accepted by the evm_kernel

  example:
    dune exec src/bin_evm_proxy/chunker/octez_evm_chunker.exe \
    "sr1SKEeoKZizGDczpS8JgYiAdkjyN8Am6TcK" \
    "f901f2808506fc23ac00831000008080b9019f608060405234801561001057600080fd5b5061017f806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c6343000812003325a0e0d5dd3ddec07eb85eb066c7782ab64a4f9f47f22e444c147708d3277dba6d68a03e358c75e5c2436ada1dcebe4aa1391797658950a9a5abae90ccff61ce5e7f34"

    dune exec src/bin_evm_proxy/chunker/octez_evm_chunker.exe \
    "f901f2808506fc23ac00831000008080b9019f608060405234801561001057600080fd5b5061017f806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c6343000812003325a0e0d5dd3ddec07eb85eb066c7782ab64a4f9f47f22e444c147708d3277dba6d68a03e358c75e5c2436ada1dcebe4aa1391797658950a9a5abae90ccff61ce5e7f34"
*)

open Evm_proxy_lib

let encode_address address =
  let open Lwt_result_syntax in
  let open Tezos_crypto.Hashed.Smart_rollup_address in
  let*? s = of_b58check address in
  let s = to_string s in
  return s

let main args =
  let open Lwt_result_syntax in
  let print_chunks smart_rollup_address s =
    let*? _, messages =
      Rollup_node.make_encoded_messages
        ~smart_rollup_address
        (Hash (Ethereum_types.strip_0x s))
    in
    Format.printf "Chunked transactions :\n%!" ;
    List.iter (Format.printf "%s\n%!") messages ;
    return_unit
  in
  match args with
  | Some [smart_rollup_address; s] ->
      let* smart_rollup_address = encode_address smart_rollup_address in
      let* () = print_chunks smart_rollup_address s in
      return ()
  | _ ->
      Format.printf
        "Usage: octez_evm_chunker <address> <hash>\n\
         - <address>: Smart rollup address (Optional)\n\
         - <hash>: Ethereum transaction hash\n\
         %!" ;
      return_unit

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match Lwt_main.run (main args) with
  | Ok _ -> ()
  | Error errs ->
      List.iter (fun e -> Format.eprintf "%a\n%!" Error_monad.pp e) errs
