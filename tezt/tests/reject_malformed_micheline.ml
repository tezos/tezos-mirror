(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Node
   Invocation:   dune exec tezt/tests/main.exe -- --test "Reject malformed micheline"
   Subject:      Test that malformed Micheline values are rejected
*)

let make_data s =
  {|
  {
    "operation": {
        "branch": "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2",
        "contents": [
            {
                "kind": "register_global_constant",
                "value": {
                    "prim": "unit",
                    "annots": [
                      |}
  ^ s
  ^ {|
                    ]
                },
                "source": "tz1LZhjR5Tyn63zZ1JSgT4WQZYyPXKTQiveu",
                "fee": "10000",
                "counter": "3819159",
                "gas_limit": "10000",
                "storage_limit": "1000"
            }
        ],
        "signature": "sigQFenAPMsrMxVvgH1K33sJgj5VqD3gajK1sBJyEugCzZ9EgTvHGiXii9opAgkei7tY1qpJKyB37YGdGGMAWdgodPyDcQvg"
    },
    "chain_id": "NetXdQprcVkpaWU"
}
  |}

let empty_implicit_contract_error_rex =
  rex "^proto\\.[^.]+\\.implicit\\.empty_implicit_contract$"

(** Test sending malformed Micheline as JSON
    to an RPC endpoint.
 *)
let reject_malformed_micheline =
  Protocol.register_test
    ~__FILE__
    ~title:"Reject malformed micheline"
    ~tags:
      [
        "micheline";
        "empty_implicit_contract";
        "malformed_annotation";
        "run_operation";
      ]
  @@ fun protocol ->
  let* node, _client = Client.init_with_protocol `Client ~protocol () in
  let send_operation data =
    (* The [run_operation] RPC is used because it doesn't require
       valid signatures. *)
    let json : RPC_core.data = Data (Ezjsonm.from_string data) in
    let* response =
      Node.RPC.(
        call_raw node (post_chain_block_helpers_scripts_run_operation json))
    in
    return response.body
  in
  (* We send a valid annotation. *)
  let* output = send_operation @@ make_data "\"%test\"" in
  let id =
    JSON.parse ~origin:"run_operation response" output
    |> JSON.geti 0 |> JSON.get "id" |> JSON.as_string
  in
  (* We're using an implicit contract that doesn't exist by default,
     so, we expect this to fail with the following message. *)
  if id =~! empty_implicit_contract_error_rex then
    Test.fail
      "Expected empty_implicit_contract failure during global constant \
       registration, but got some other failure.\n\
      \ %s"
      output
  else
    (* In this test, we send a malformed annotation, and expect
       the operation to fail accordingly. *)
    let* output = send_operation @@ make_data "\"\\\"%test\"" in
    let test = Str.regexp ".*Malformed annotation.*" in
    let has_correct_error_message =
      output |> String.split_on_char '\n'
      |> List.exists (fun s -> Str.string_match test s 0)
    in
    if has_correct_error_message then return ()
    else Test.fail "Unexpected success on malformed Micheline value"

let register ~protocols = reject_malformed_micheline protocols
