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

(** [all] represents all registered host functions that are important for the
    SCORU WASM PVM. *)
val all : Tezos_webassembly_interpreter.Host_funcs.registry

exception Bad_input

(** A durable key was given by the kernel with a longer-than-allowed length. *)
exception Key_too_large of int

module Error : sig
  type t =
    | Store_key_too_large
        (** The store key submitted as an argument of a host function exceeds
            the authorized limit. Has code `-1`. *)
    | Store_invalid_key
        (** The store key submitted as an argument of a host function cannot be
            parsed. Has code `-2`. *)
    | Store_not_a_value
        (** The contents (if any) of the store under the key submitted as an
            argument of a host function is not a value. Has code `-3`. *)
    | Store_invalid_access
        (** An access in a value of the durable storage has failed, supposedly
            out of bounds of a value. Has code `-4`. *)
    | Store_value_size_exceeded
        (** Writing a value has exceeded 2^31 bytes. Has code `-5`. *)
    | Memory_invalid_access
        (** An address is out of bound of the memory. Has code `-6`. *)
    | Input_output_too_large
        (** The input or output submitted as an argument of a host function
            exceeds the authorized limit. Has code `-7`. *)
    | Generic_invalid_access
        (** Generic error code for unexpected errors. Has code `-8`. *)

  (** [code error] returns the error code associated to the error. *)
  val code : t -> int32
end

module Aux : sig
  (** [aux_write_output ~input_buffer ~output_buffer ~module_inst ~src
       ~num_bytes] reads num_bytes from the memory of module_inst starting at
       src and writes this to the output_buffer. It also checks that
       the input payload is no larger than `max_output`. It returns 0 for Ok and
      1 for `output too large`.*)
  val write_output :
    output_buffer:Tezos_webassembly_interpreter.Output_buffer.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    src:int32 ->
    num_bytes:int32 ->
    int32 Lwt.t

  (** [aux_write_memory ~input_buffer ~module_inst ~level_offset
       ~id_offset ~dst ~max_bytes] reads `input_buffer` and writes its
       components to the memory of `module_inst` based on the memory
       addreses offsets described. It also checks that the input
       payload is no larger than `max_input` and crashes with `input
       too large` otherwise. It returns the size of the payload. Note
       also that, if the level increases this function also updates
       the level of the output buffer and resets its id to zero. *)
  val read_input :
    input_buffer:Tezos_webassembly_interpreter.Input_buffer.t ->
    output_buffer:Tezos_webassembly_interpreter.Output_buffer.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    level_offset:int32 ->
    id_offset:int32 ->
    dst:int32 ->
    max_bytes:int32 ->
    int32 Lwt.t

  val store_has :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    key_offset:int32 ->
    key_length:int32 ->
    int32 Lwt.t

  val store_delete :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    key_offset:int32 ->
    key_length:int32 ->
    (Durable.t * int32) Lwt.t

  val store_copy :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    from_key_offset:int32 ->
    from_key_length:int32 ->
    to_key_offset:int32 ->
    to_key_length:int32 ->
    (Durable.t * int32) Lwt.t

  val store_move :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    from_key_offset:int32 ->
    from_key_length:int32 ->
    to_key_offset:int32 ->
    to_key_length:int32 ->
    (Durable.t * int32) Lwt.t

  val store_value_size :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    key_offset:int32 ->
    key_length:int32 ->
    int32 Lwt.t

  val store_read :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    key_offset:int32 ->
    key_length:int32 ->
    value_offset:int32 ->
    dest:int32 ->
    max_bytes:int32 ->
    int32 Lwt.t

  val store_write :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    key_offset:int32 ->
    key_length:int32 ->
    value_offset:int32 ->
    src:int32 ->
    num_bytes:int32 ->
    (Durable.t * int32) Lwt.t

  val store_list_size :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    key_offset:int32 ->
    key_length:int32 ->
    (Durable.t * int64) Lwt.t

  val store_get_nth_key :
    durable:Durable.t ->
    memory:Tezos_webassembly_interpreter.Instance.memory_inst ->
    key_offset:int32 ->
    key_length:int32 ->
    index:int64 ->
    dst:int32 ->
    max_size:int32 ->
    int32 Lwt.t
end

module Internal_for_tests : sig
  (** The number of bytes used to store the metadata of a rollup in memory *)
  val metadata_size : int

  val write_output : Tezos_webassembly_interpreter.Instance.func_inst

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

  val store_value_size : Tezos_webassembly_interpreter.Instance.func_inst

  val store_read : Tezos_webassembly_interpreter.Instance.func_inst

  val store_write : Tezos_webassembly_interpreter.Instance.func_inst

  val store_list_size : Tezos_webassembly_interpreter.Instance.func_inst

  val store_get_nth_key : Tezos_webassembly_interpreter.Instance.func_inst
end
