(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(*

  This library acts as a dependency to the protocol environment. Everything that
  must be exposed to the protocol via the environment shall be added here.

*)

exception Set_input_step_expected_input

module Make (T : Tree.S) : Wasm_pvm_sig.S with type tree = T.tree = struct
  include
    Gather_floppies.Make
      (T)
      (struct
        type tree = T.tree

        module Decodings = Wasm_decodings.Make (T)
        module Thunk = Thunk.Make (T)
        module Decoding = Tree_decoding.Make (T)

        let compute_step s =
          let open Lwt.Syntax in
          (* register the PVM host funcs wrappers in a module ["rollup_safe_core"]
             into the WASM linker *)
          let* () =
            Tezos_webassembly_interpreter.(
              Import.register ~module_name:(Utf8.decode "rollup_safe_core"))
              Host_funcs.lookup
          in
          (* build the registry of host functions (to be passed to the interpreter via its config *)
          let host_funcs_registry =
            Tezos_webassembly_interpreter.Host_funcs.empty ()
          in
          Host_funcs.register_host_funcs host_funcs_registry ;
          Lwt.return s

        let get_output _ _ = Lwt.return ""

        let get_info _ =
          Lwt.return
            Wasm_pvm_sig.
              {
                current_tick = Z.of_int 0;
                last_input_read = None;
                input_request = Input_required;
              }
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/3226
           Implement handling of input logic.
        *)

        let set_input_step input_info message tree =
          let open Lwt_syntax in
          let open Wasm_pvm_sig in
          let* {input_request; _} = get_info tree in
          let {inbox_level; message_counter} = input_info in
          let level =
            Int32.to_string @@ Bounded.Int32.NonNegative.to_int32 inbox_level
          in
          let id = Z.to_string message_counter in
          match input_request with
          | No_input_required -> raise Set_input_step_expected_input
          | _ -> T.add tree ["input"; level; id] @@ Bytes.of_string message

        let _module_instance_of_tree modules =
          Decodings.run (Decodings.module_instance_decoding modules)

        let _module_instances_of_tree =
          Decodings.run Decodings.module_instances_decoding
      end)
end
