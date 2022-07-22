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

open Tezos_webassembly_interpreter

module Make (T : Tree.S) : Wasm_pvm_sig.S with type tree = T.tree = struct
  include
    Gather_floppies.Make
      (T)
      (struct
        type tree = T.tree

        module Decodings = Wasm_decodings.Make (T)
        module EncDec =
          Tree_encoding_decoding.Make
            (Lazy_map.LwtInt32Map)
            (Lazy_vector.LwtInt32Vector)
            (Chunked_byte_vector.Lwt)
            (T)

        let compute_step = Lwt.return

        let get_output _ _ = Lwt.return ""

        (* TODO: #3444
           Create a may-fail tree-encoding-decoding combinator.
           https://gitlab.com/tezos/tezos/-/issues/3444
        *)

        (* TODO: #3448
           Remove the mention of exceptions from lib_scoru_wasm Make signature.
           Add try_with or similar to catch exceptions and put the machine in a
           stuck state instead. https://gitlab.com/tezos/tezos/-/issues/3448
        *)

        let current_tick_encoding =
          EncDec.value ["wasm"; "current_tick"] Data_encoding.z

        let level_encoding =
          EncDec.value ["input"; "level"] Bounded.Int32.NonNegative.encoding

        let id_encoding = EncDec.value ["input"; "id"] Data_encoding.z

        let last_input_read_encoder =
          EncDec.tup2 ~flatten:true level_encoding id_encoding

        let status_encoding =
          EncDec.value ["input"; "consuming"] Data_encoding.bool

        let inp_encoding level id =
          EncDec.value ["input"; level; id] Data_encoding.string

        let get_info tree =
          let open Lwt_syntax in
          let* waiting =
            try EncDec.decode status_encoding tree with _ -> Lwt.return false
          in
          let input_request =
            if waiting then Wasm_pvm_sig.Input_required
            else Wasm_pvm_sig.No_input_required
          in
          let* input =
            try
              let* t = EncDec.decode last_input_read_encoder tree in
              Lwt.return @@ Some t
            with _ -> Lwt.return_none
          in
          let last_input_read =
            Option.map
              (fun (inbox_level, message_counter) ->
                Wasm_pvm_sig.{inbox_level; message_counter})
              input
          in

          let* current_tick =
            try EncDec.decode current_tick_encoding tree
            with _ -> Lwt.return Z.zero
          in
          Lwt.return Wasm_pvm_sig.{current_tick; last_input_read; input_request}

        let set_input_step input_info message tree =
          let open Lwt_syntax in
          let open Wasm_pvm_sig in
          let {inbox_level; message_counter} = input_info in
          let level =
            Int32.to_string @@ Bounded.Int32.NonNegative.to_int32 inbox_level
          in
          let id = Z.to_string message_counter in
          let* current_tick = EncDec.decode current_tick_encoding tree in
          let* tree =
            EncDec.encode current_tick_encoding (Z.succ current_tick) tree
          in
          let* tree = EncDec.encode status_encoding false tree in
          EncDec.encode (inp_encoding level id) message tree

        let _module_instance_of_tree modules =
          Decodings.run (Decodings.module_instance_decoding modules)

        let _module_instances_of_tree =
          Decodings.run Decodings.module_instances_decoding
      end)
end
