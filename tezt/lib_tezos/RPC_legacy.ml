(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

type ctxt_type = Bytes | Json

module Seed = struct
  let get_seed ?endpoint ?hooks ?(chain = "main") ?(block = "head") client =
    let path = ["chains"; chain; "blocks"; block; "context"; "seed"] in
    let* json = Client.rpc ?endpoint ?hooks POST path client in
    return (JSON.as_string json)

  let get_seed_status ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "seed_computation"]
    in
    Client.rpc ?endpoint ?hooks GET path client
end

module Script_cache = struct
  let get_cached_contracts ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "cache"; "contracts"; "all"]
    in
    Client.rpc ?endpoint ?hooks GET path client
end

let raw_bytes ?endpoint ?hooks ?(chain = "main") ?(block = "head") ?(path = [])
    client =
  let path =
    ["chains"; chain; "blocks"; block; "context"; "raw"; "bytes"] @ path
  in
  Client.rpc ?endpoint ?hooks GET path client

module Curl = struct
  let parse url process =
    let* output = Process.check_and_read_stdout process in
    return (JSON.parse ~origin:url output)

  let get ?runner ?(args = []) url =
    let process = Process.spawn ?runner "curl" (args @ ["-s"; url]) in
    Runnable.{value = process; run = parse url}

  let get_raw ?runner ?(args = []) url =
    let process = Process.spawn ?runner "curl" (args @ ["-s"; url]) in
    Runnable.{value = process; run = Process.check_and_read_stdout}

  let post ?runner ?(args = []) url data =
    let process =
      Process.spawn
        ?runner
        "curl"
        (args
        @ [
            "-X";
            "POST";
            "-H";
            "Content-Type: application/json";
            "-s";
            url;
            "-d";
            JSON.encode data;
          ])
    in
    Runnable.{value = process; run = parse url}
end
