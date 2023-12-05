(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* Replace variables that may change between different runs by constants.

   Order them by length.
*)
let replace_variables string =
  let replacements =
    [
      ("sh\\w{72}\\b", "[DAL_SLOT_HEADER]");
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/3752
         Remove this regexp as soon as the WASM PVM stabilizes. *)
      ("srs\\w{51}\\b", "[SC_ROLLUP_PVM_STATE_HASH]");
      ("\\bB\\w{50}\\b", "[BLOCK_HASH]");
      ("SRCo\\w{50}\\b", "[SC_ROLLUP_CONTEXT_HASH]");
      ("Co\\w{50}\\b", "[CONTEXT_HASH]");
      ("src1\\w{50}\\b", "[SC_ROLLUP_COMMITMENT_HASH]");
      ("srib1\\w{50}\\b", "[SC_ROLLUP_INBOX_HASH]");
      ("srib2\\w{50}\\b", "[SC_ROLLUP_INBOX_MERKELIZED_PAYLOAD_HASHES_HASH]");
      ("srib3\\w{50}\\b", "[SC_ROLLUP_INBOX_MESSAGE_HASH]");
      ("edpk\\w{50}\\b", "[PUBLIC_KEY]");
      ("\\bo\\w{50}\\b", "[OPERATION_HASH]");
      ("tz[1234]\\w{33}\\b", "[PUBLIC_KEY_HASH]");
      ("sr1\\w{33}\\b", "[SMART_ROLLUP_HASH]");
      ("KT1\\w{33}\\b", "[CONTRACT_HASH]");
      ("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z", "[TIMESTAMP]");
      (* Ports are non-deterministic when using -j. *)
      ("/localhost:\\d{4,5}/", "/localhost:[PORT]/");
      ("/127.0.0.1:\\d{4,5}/", "/127.0.0.1:[PORT]/");
    ]
  in
  List.fold_left
    (fun string (replace, by) ->
      replace_string ~all:true (rex replace) ~by string)
    string
    replacements

let scrubbed_global_options =
  ["--base-dir"; "-d"; "--endpoint"; "-E"; "--sources"]

let hooks_custom ?(scrubbed_global_options = scrubbed_global_options)
    ?(replace_variables = replace_variables) () =
  let on_spawn command arguments =
    (* Remove arguments that shouldn't be captured in regression output. *)
    let arguments, _ =
      List.fold_left
        (fun (acc, scrub_next) arg ->
          if scrub_next then (acc, false)
          else
            match arg with
            (* scrub client global options *)
            | option when List.mem option scrubbed_global_options -> (acc, true)
            | _ -> (acc @ [replace_variables arg], false))
        ([], (* scrub_next *) false)
        arguments
    in
    let message = Log.quote_shell_command command arguments in
    Regression.capture ("\n" ^ message)
  in
  let on_log output = replace_variables output |> Regression.capture in
  {Process.on_spawn; on_log}

let hooks = hooks_custom ~scrubbed_global_options ~replace_variables ()

let rpc_hooks : RPC_core.rpc_hooks =
  let on_request input = replace_variables input |> Regression.capture in
  let on_response status body =
    Regression.capture (Cohttp.Code.string_of_status status) ;
    Regression.capture (replace_variables body)
  in
  {on_request; on_response}
