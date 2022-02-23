(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** The protocol we are using. *)
let protocol = Protocol.Alpha

(** The constants we are using. *)
let constants = Protocol.Constants_mainnet

let estimate_gas_tps ~average_block_path () =
  Average_block.load average_block_path >>= fun average_block ->
  let transaction_cost = Gas.average_transaction_cost average_block in
  Format.printf "Average transaction cost: %d@." transaction_cost ;
  let gas_tps = Gas.deduce_tps ~protocol ~constants ~transaction_cost () in
  Format.printf "Gas TPS: %d@." gas_tps ;
  Lwt.return ()

module Term = struct
  let average_block_path_arg =
    let open Cmdliner in
    let doc = "Path to the file with description of the average block" in
    let docv = "AVERAGE_BLOCK_PATH" in
    Arg.(value & opt (some string) None & info ["average-block"] ~docv ~doc)

  let process average_block_path =
    Lwt_main.run (estimate_gas_tps ~average_block_path ()) ;
    `Ok ()

  let term =
    let open Cmdliner.Term in
    ret (const process $ average_block_path_arg)
end

module Manpage = struct
  let command_description = "Estimate TPS based on gas"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Term.info ~doc:command_description ~man "gas-tps"
end

let cmd = (Term.term, Manpage.info)
