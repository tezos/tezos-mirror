(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** [Gas_input_size] includes the definitions for the different sizes used
    in the gas models of the protocol. They do not always represent memory
    sizes, but rather they can be seen as an information size. They are
    tailored to the models that use them, and should not be used for anything
    other than gas computation.
 *)

include module type of Gas_comparable_input_size

(* ------------------------------------------------------------------------- *)

val list : 'a Script_list.t -> t

val set : 'a Script_typed_ir.set -> t

val map : ('a, 'b) Script_typed_ir.map -> t

(* ------------------------------------------------------------------------- *)
(* Micheline/Michelson-related *)

val of_micheline : ('a, 'b) Micheline.node -> micheline_size

(* ------------------------------------------------------------------------- *)
(* Sapling-related *)

val sapling_transaction_inputs : Alpha_context.Sapling.transaction -> t

val sapling_transaction_outputs : Alpha_context.Sapling.transaction -> t

val sapling_transaction_bound_data : Alpha_context.Sapling.transaction -> t
