(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Error_monad

type error_code = int

type t =
  | No_motive
  | Too_many_connections
  | Already_connected
  | Unknown_chain_name
  | Deprecated_distributed_db_version
  | Deprecated_p2p_version
  | Unknown_motive of error_code

let pp ppf motive =
  match motive with
  | No_motive -> Format.fprintf ppf "No motive"
  | Too_many_connections -> Format.fprintf ppf "Too many connections"
  | Already_connected -> Format.fprintf ppf "Already connected/connecting"
  | Unknown_chain_name -> Format.fprintf ppf "Unknown chain name"
  | Deprecated_distributed_db_version ->
      Format.fprintf ppf "Running a deprecated distributed db version"
  | Deprecated_p2p_version ->
      Format.fprintf ppf "Running a deprecated p2p layer version"
  | Unknown_motive error_code ->
      Format.fprintf ppf "Rejected for unknown reason, code (%i)" error_code

let pp_short ppf motive =
  match motive with
  | No_motive -> Format.fprintf ppf "No motive"
  | Too_many_connections -> Format.fprintf ppf "Too many connections"
  | Already_connected -> Format.fprintf ppf "Already connected"
  | Unknown_chain_name -> Format.fprintf ppf "Unknown chain name"
  | Deprecated_distributed_db_version ->
      Format.fprintf ppf "Deprecated ddb version"
  | Deprecated_p2p_version -> Format.fprintf ppf "Deprecated p2p version"
  | Unknown_motive error_code ->
      Format.fprintf ppf "unknown code (%i)" error_code

let encoding =
  let open Data_encoding in
  conv
    (function
      | No_motive -> 0
      | Too_many_connections -> 1
      | Unknown_chain_name -> 2
      | Deprecated_p2p_version -> 3
      | Deprecated_distributed_db_version -> 4
      | Already_connected -> 5
      | Unknown_motive error_code -> error_code)
    (function
      | 0 -> No_motive
      | 1 -> Too_many_connections
      | 2 -> Unknown_chain_name
      | 3 -> Deprecated_p2p_version
      | 4 -> Deprecated_distributed_db_version
      | 5 -> Already_connected
      | error_code -> Unknown_motive error_code)
    Data_encoding.int16

type error += Rejecting of {motive : t}

let () =
  (* Rejecting socket connection with motive *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.rejecting_incoming"
    ~title:"Rejecting socket connection"
    ~description:"Rejecting peer connection with motive."
    ~pp:(fun ppf motive ->
      Format.fprintf ppf "Rejecting peer connection. Cause : %a." pp motive)
    Data_encoding.(obj1 (req "motive" encoding))
    (function Rejecting {motive} -> Some motive | _ -> None)
    (fun motive -> Rejecting {motive})

let rejecting motive = Result_syntax.tzfail (Rejecting {motive})
