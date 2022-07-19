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

open Tezos_webassembly_interpreter
open Instance

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
      Bytes.sub payload 0 @@ min input_size (Int32.to_int max_bytes)
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

let read_input_type =
  let input_types =
    Types.
      [
        NumType I32Type;
        NumType I32Type;
        NumType I32Type;
        NumType I32Type;
        NumType I32Type;
      ]
    |> Vector.of_list
  in
  let output_types = Types.[NumType I32Type] |> Vector.of_list in
  let fun_type = Types.FuncType (input_types, output_types) in
  fun_type

let read_input_name = "tezos_read_input"

let read_input_desc = (read_input_type, read_input_name)

let read_input input_buffer module_inst inputs =
  let open Lwt.Syntax in
  match inputs with
  | [
   Values.(Num (I32 rtype_offset));
   Values.(Num (I32 level_offset));
   Values.(Num (I32 id_offset));
   Values.(Num (I32 dst));
   Values.(Num (I32 max_bytes));
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

let lookup name =
  let open Lwt.Syntax in
  let+ name = Utf8.encode name in
  match name with
  | "read_input" -> ExternFunc (HostFunc (read_input_type, read_input_name))
  | _ -> raise Not_found

let register_host_funcs registry =
  Host_funcs.register ~global_name:read_input_name read_input registry

module Internal_for_tests = struct
  let aux_write_input_in_memory = aux_write_input_in_memory
end
