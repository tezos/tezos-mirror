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

(** [lookup name] retrieves or instantiates a host function by the given [name].
    Currently dispatches [read_input] to {!read_input} using host function global
    names as registered by {!register_host_funcs}.
    Used to plug host function wrappers in the WASN interpreter linker. *)
val lookup :
  Tezos_webassembly_interpreter.Ast.name ->
  Tezos_webassembly_interpreter.Instance.extern

(** [lookup_opt name] is exactly [lookup name] but returns an option instead of
    raising `Not_found`. *)
val lookup_opt :
  Tezos_webassembly_interpreter.Ast.name ->
  Tezos_webassembly_interpreter.Instance.extern option

(** [register_host_funcs] registers all the PVMs host functions into a WASM
    interpreter's registry, using the names expected by {!lookup}.

    Currently, the registered functions are:
    - [read_input]:
      It has to be invoked with a list
      of 5 values representing rtype_offset, level_offset, id_offset,
      dst and max_bytes, otherwise it raises the [Bad_input] exception.

      When invoked, it write the content of an input message into the
      memory of a [module_inst]. It also checks that the input payload
      is no larger than the input is not too large. Finally, it returns
      returns a singleton value list containing the size of the
      input_buffer payload. *)
val register_host_funcs :
  Tezos_webassembly_interpreter.Host_funcs.registry -> unit

exception Bad_input

(** A durable key was given by the kernel with a longer-than-allowed length. *)
exception Key_too_large of int

module Internal_for_tests : sig
  (** [aux_write_output ~input_buffer ~output_buffer ~module_inst ~src
       ~num_bytes] reads num_bytes from the memory of module_inst starting at
       src and writes this to the output_buffer. It also checks that
       the input payload is no larger than `max_output`. It returns 0 for Ok and
      1 for `output too large`.*)
  val aux_write_output :
    input_buffer:Tezos_webassembly_interpreter.Input_buffer.t ->
    output_buffer:Tezos_webassembly_interpreter.Output_buffer.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    src:int32 ->
    num_bytes:int32 ->
    int32 Lwt.t

  val write_output : Tezos_webassembly_interpreter.Instance.func_inst

  (** [aux_write_memory ~input_buffer ~module_inst ~rtype_offset
       ~level_offset ~id_offset ~dst ~max_bytes] reads `input_buffer`
       and writes its components to the memory of `module_inst` based
       on the memory addreses offsets described. It also checks that
       the input payload is no larger than `max_input` and crashes
       with `input too large` otherwise. It returns the size of the
       payload. Note also that, if the level increases this function also
      updates the level of the output buffer and resets its id to zero.*)
  val aux_write_input_in_memory :
    input_buffer:Tezos_webassembly_interpreter.Input_buffer.t ->
    output_buffer:Tezos_webassembly_interpreter.Output_buffer.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    rtype_offset:int32 ->
    level_offset:int32 ->
    id_offset:int32 ->
    dst:int32 ->
    max_bytes:int32 ->
    int Lwt.t

  val read_input : Tezos_webassembly_interpreter.Instance.func_inst

  (** [store_has] returns whether a key corresponds to a value and/or subtrees.
      Namely, it returns the following enum:
      - [0]: There is no value at [key], nor subtrees under [key].
      - [1]: There is a value at [key], but no subtrees under [key].
      - [2]: There is no value at [key], but there are subtrees under [key].
      - [3]: There is a value at [key], and subtrees under [key].
  *)
  val store_has : Tezos_webassembly_interpreter.Instance.func_inst

  val store_delete : Tezos_webassembly_interpreter.Instance.func_inst

  val store_copy : Tezos_webassembly_interpreter.Instance.func_inst

  val store_move : Tezos_webassembly_interpreter.Instance.func_inst

  val store_read : Tezos_webassembly_interpreter.Instance.func_inst

  val store_write : Tezos_webassembly_interpreter.Instance.func_inst

  val store_list_size : Tezos_webassembly_interpreter.Instance.func_inst
end
