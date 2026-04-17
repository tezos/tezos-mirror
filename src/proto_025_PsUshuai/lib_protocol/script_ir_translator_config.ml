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

open Alpha_context

(** [type_logger] is a function, whose task is to log how a stack's type
   is altered by some operation being logged. *)
type type_logger =
  Script.location ->
  stack_ty_before:Script.expr list ->
  stack_ty_after:Script.expr list ->
  unit

(** LEGACY MODE is the feature of the Translator and Interpreter which
    allows us to distinguish between scripts already originated on chain
    and new ones.

    The reason to treat those types of scripts differently is the evolving
    nature of Michelson, which sometimes requires disabling features
    available in previous versions. These features must be supported at all
    times for already originated contracts, but we still want to disable
    them at least for new contracts.

    This distinction gives us a handy deprecation mechanism, which
    allows us to make sure that from a certain point on no more
    contract will be originated using these deprecated features. When
    that point time is reached, it becomes possible to patch existing
    contracts so that they no longer use the feature and remove it
    entirely.

    As a side effect, legacy mode can also be used to skip checks that
    have already been performed and hence are guaranteed to pass.*)

(** [elab_config] is a record grouping together some flags and options
    shared by many of the functions in [Script_ir_translator]. It's
    convenient to group them together, because they're of similar
    types ([bool] or ['a option]), so they're easier not to mix together.
    It also makes for shorter and more readable function calls. *)
type elab_config = {
  type_logger : type_logger option;
      (** A function responsible for logging stack types during typechecking.
        Used especially in plugins for editors and IDEs. *)
  keep_extra_types_for_interpreter_logging : bool;
      (** If set to [true], it instructs the elaborator to retain some
        additional type information necessary for logging. This should
        never be enabled during validation to save memory occupied by
        cached contracts.

        NOTE: if this option wasn't passed to the elaborator and the 
        interpreter was still called with logging enabled, it might
        result in a crash. This cannot be helped at the moment, but since 
        logging is never enabled during validation, we should be safe. *)
  legacy : bool;  (** If set to true, it enables the legacy mode (see above). *)
}

(** [make ?type_logger ?logging_enabled ~legacy ()] creates an [elab_config]
    record to be passed to parsing functions in [Script_ir_translator].

    Note: [?logging_enabled] defaults to [false], because it only ever should
    be set to [true] if the Translator is called from outside the protocol
    (i.e. from the Plugin). *)
let make :
    ?type_logger:type_logger ->
    ?keep_extra_types_for_interpreter_logging:bool ->
    legacy:bool ->
    unit ->
    elab_config =
 fun ?type_logger
     ?(keep_extra_types_for_interpreter_logging = false)
     ~legacy
     () ->
  {type_logger; keep_extra_types_for_interpreter_logging; legacy}
