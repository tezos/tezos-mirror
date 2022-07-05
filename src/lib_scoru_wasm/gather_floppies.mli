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

(** Raised when the origination message contains neither a complete nor an
    incomplete kernel message. *)
exception Malformed_origination_message of Data_encoding.Binary.read_error

(** Raised when a message containing a kernel image chunk was expected, but 
    the message in the inbox contained something else. *)
exception Malformed_inbox_message of Data_encoding.Binary.read_error

(** Raised when [compute_step] was called when the floppy gathering module 
    expected input. *)
exception Compute_step_expected_input

(** Raised when the floppy gathering module wasn't expecting input, but input
    was given using [set_input_step]. A [compute_step] is needed right after
    origination. *)
exception Set_input_step_expected_compute_step

(** Generic internal error. Some data in storage had errornous encoding. *)
exception Encoding_error of Data_encoding.Binary.write_error

(** Internal error. Raised if the [input_info] record that is stored somehow
    gets overwritten with something malformed. *)
exception Malformed_input_info_record

(** [Make] encapsulates a WASM PVM to give it the ability to load a kernel
    image as either a complete kernel in the origination message or a kernel
    image divided into chunks and provided via both origination- and inbox-
    messages. *)
module Make (T : Tree.S) (Wasm : Wasm_pvm.S with type tree = T.tree) :
  Wasm_pvm.S with type tree = T.tree
