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
        module Decoding = Tree_decoding.Make (T)
        module Encoding = Tree_encoding.Make (T)

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

        let current_tick_decoding =
          Decoding.value ["wasm"; "current_tick"] Data_encoding.z

        let current_tick_encoding =
          Encoding.value ["wasm"; "current_tick"] Data_encoding.z

        let level_decoding =
          Decoding.value ["input"; "level"] Bounded.Int32.NonNegative.encoding

        let id_decoding = Decoding.value ["input"; "id"] Data_encoding.z

        let status_encoding =
          Encoding.value ["input"; "consuming"] Data_encoding.bool

        let status_decoding =
          Decoding.value ["input"; "consuming"] Data_encoding.bool

        let get_info tree =
          let open Lwt_syntax in
          let* waiting =
            try Decoding.run status_decoding tree with _ -> Lwt.return false
          in
          let input_request =
            if waiting then Wasm_pvm_sig.Input_required
            else Wasm_pvm_sig.No_input_required
          in
          let* inbox_level =
            try Decoding.run level_decoding tree
            with _ ->
              Lwt.return
                (match Bounded.Int32.NonNegative.of_int32 0l with
                | Some x -> x
                | _ -> assert false)
          in
          let* message_counter =
            try Decoding.run id_decoding tree
            with _ -> Lwt.return (Z.of_int (-1))
          in
          let last_input_read =
            if message_counter = Z.of_int (-1) then None
            else Some Wasm_pvm_sig.{inbox_level; message_counter}
          in
          let* current_tick =
            try Decoding.run current_tick_decoding tree
            with _ -> Lwt.return Z.zero
          in
          Lwt.return Wasm_pvm_sig.{current_tick; last_input_read; input_request}

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
          let inp_encoding =
            Encoding.value ["input"; level; id] Data_encoding.string
          in
          match input_request with
          | No_input_required -> raise Set_input_step_expected_input
          | _ ->
              let* current_tick = Decoding.run current_tick_decoding tree in
              let* tree =
                Encoding.run current_tick_encoding (Z.succ current_tick) tree
              in
              let* tree = Encoding.run status_encoding false tree in
              Encoding.run inp_encoding message tree

        let _module_instance_of_tree modules =
          Decodings.run (Decodings.module_instance_decoding modules)

        let _module_instances_of_tree =
          Decodings.run Decodings.module_instances_decoding
      end)
end
