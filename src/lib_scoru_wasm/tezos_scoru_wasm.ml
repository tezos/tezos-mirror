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
open Instance

type input = {
  inbox_level : Tezos_base.Bounded.Int32.NonNegative.t;
  message_counter : Z.t;
}

type output = {
  outbox_level : Tezos_base.Bounded.Int32.NonNegative.t;
  message_index : Z.t;
}

type input_request = No_input_required | Input_required

type info = {
  current_tick : Z.t;
  last_input_read : input option;
  input_request : input_request;
}

module type S = sig
  type tree

  val compute_step : tree -> tree Lwt.t

  val set_input_step : input -> string -> tree -> tree Lwt.t

  val get_output : output -> tree -> string Lwt.t

  val get_info : tree -> info Lwt.t
end

module Make (T : Tree.S) : S with type tree = T.tree = struct
  type tree = T.tree

  module Decodings = Wasm_decodings.Make (T)

  let compute_step = Lwt.return

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3092
     Implement handling of input logic.
  *)
  let set_input_step _ _ = Lwt.return

  let get_output _ _ = Lwt.return ""

  let get_info _ =
    Lwt.return
      {
        current_tick = Z.of_int 0;
        last_input_read = None;
        input_request = No_input_required;
      }

  let _module_instance_of_tree modules =
    Decodings.run (Decodings.module_instance_decoding modules)

  let _module_instances_of_tree =
    Decodings.run Decodings.module_instances_decoding
end

exception Bad_input

let aux_write_input_in_memory ~input_buffer ~module_inst ~rtype_offset
    ~level_offset ~id_offset ~dst ~max_bytes =
  let open Lwt.Syntax in
  let memories = !module_inst.memories in
  let* {rtype; raw_level; message_counter; payload} =
    Input_buffer.dequeue input_buffer
  in
  let input_size = Bytes.length payload in
  if Int64.of_int input_size > 4096L then
    raise (Eval.Crash (Source.no_region, "input too large"))
  else
    let payload =
      Bytes.sub payload 0 @@ min input_size (Int64.to_int max_bytes)
    in
    let* memory =
      match Vector.num_elements memories with
      | 1l -> Vector.get 0l memories
      | _ ->
          raise
            (Eval.Crash
               (Source.no_region, "the memories is supposed to be a singleton"))
    in

    let _ = Memory.store_bytes memory dst (Bytes.to_string payload) in
    let _ = Memory.store_num memory rtype_offset 0l (I32 rtype) in
    let _ = Memory.store_num memory level_offset 0l (I32 raw_level) in
    let _ =
      Memory.store_num memory id_offset 0l (I64 (Z.to_int64 message_counter))
    in
    Lwt.return input_size

let read_input =
  let open Lwt.Syntax in
  let input_types =
    Types.
      [
        NumType I64Type;
        NumType I64Type;
        NumType I64Type;
        NumType I64Type;
        NumType I64Type;
      ]
  in
  let output_types = Types.[NumType I32Type] in
  let fun_type = Types.FuncType (input_types, output_types) in
  let f input_buffer module_inst inputs =
    match inputs with
    | [
     Values.(Num (I64 rtype_offset));
     Values.(Num (I64 level_offset));
     Values.(Num (I64 id_offset));
     Values.(Num (I64 dst));
     Values.(Num (I64 max_bytes));
    ] ->
        let* x =
          aux_write_input_in_memory
            ~input_buffer
            ~module_inst
            ~rtype_offset
            ~level_offset
            ~id_offset
            ~dst
            ~max_bytes
        in
        Lwt.return [Values.(Num (I32 (I32.of_int_s x)))]
    | _ -> raise Bad_input
  in
  Func.HostFunc (fun_type, f)
