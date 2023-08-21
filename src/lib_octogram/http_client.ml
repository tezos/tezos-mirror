(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Uri
module SMap = Map.Make (String)

type t = {mutable pending_download : string Lwt.t SMap.t}

let create () = {pending_download = SMap.empty}

let local_path_from_agent_uri ?(keep_name = false) ?(exec = true) client =
  function
  | Owned {name = res} -> return res
  | Remote {endpoint} -> (
      let handler =
        if keep_name then Filename.basename endpoint
        else
          let (`Hex handler) =
            Tezos_crypto.Blake2B.(hash_string [endpoint] |> to_hex)
          in
          handler
      in
      match SMap.find_opt handler client.pending_download with
      | Some promise -> promise
      | None ->
          let promise, u = Lwt.task () in
          client.pending_download <-
            SMap.add handler promise client.pending_download ;
          Background.register
            (let* path = Helpers.download endpoint handler in
             let* () =
               if exec then
                 let* () = Lwt_unix.chmod path 0o777 in
                 unit
               else unit
             in
             Lwt.wakeup u path ;
             unit) ;
          promise)
