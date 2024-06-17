(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Extends the {!Result_syntax} with additional utilities for wrapping
    results produced by the protocol, i.e. [Environment.Error_monad.tzresult],
    to [tzresult] values used in the tests.

    The added utilities are binding operators. They use the same symbols as
    the ones from {!Result_syntax} with an added [@] character. This
    character symbolizes the {!e wrapping} of the internal error monad type in a
    shell error.  *)

include module type of Tezos_base.TzPervasives.Result_syntax

(** [wrap res] is equivalent to [Environment.wrap_tzresult res] *)
val wrap : 'a Environment.Error_monad.tzresult -> 'a tzresult

(** [let@ x = m in f x] is equivalent to [let x = Environment.wrap_tzresult m in f x] *)
val ( let@ ) : 'a Environment.Error_monad.tzresult -> ('a tzresult -> 'b) -> 'b

(** [let*@ x = m in f x] is equivalent to [let* x = Environment.wrap_tzresult m in f x].
        Mnemonic: [@] "wraps" a protocol error in a shell error. *)
val ( let*@ ) :
  'a Environment.Error_monad.tzresult -> ('a -> 'b tzresult) -> 'b tzresult

(** [let+@ x = m in f x] is equivalent to [let+ x = Environment.wrap_tzresult m in f x].
          Mnemonic: [@] "wraps" a protocol error in a shell error. *)
val ( let+@ ) : 'a Environment.Error_monad.tzresult -> ('a -> 'b) -> 'b tzresult
