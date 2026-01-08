(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [add_ansi_marking ppf] will enable [ppf] to handle semantic tags.

    See {{:https://ocaml.org/manual/5.2/api/Format.html#tags}}
    for more precisions.

    Possible values are:
    - Foreground colors as 'fg_<color>'
    - Background colors as 'bg_<color>'
    - 'italic', 'bold', 'underline' ('i', 'b' and 'u' can also be used)
    - '/italic', '/bold', '/underline' (to revert these precise emphasis)

    Possible colors are: 'black', 'blue', 'cyan', 'green', 'magenta',
    'red', 'white', 'yellow'.

    Styles are declared with "@\{<list of styles separated by ';'>...@\}"

    It's a good habit to enable semantic tags at point and reset to the
    previous behavior to avoid conflicting with other printers that would
    implement their own semantic tags handlers. This is why this function
    returns a function that allows to reset [ppf] to its old behavior.

    Example of use:

    \[
    let pp ppf t =
      let reset = add_ansi_marking ppf in
      Format.fprintf ppf "@\{<bold; fg_cyan>...@\}" t;
      reset ()
    \]
*)
val add_ansi_marking : Format.formatter -> unit -> unit

val pp_centered : ?char:char -> int -> Format.formatter -> string -> unit

val pp_right_aligned : ?char:char -> int -> Format.formatter -> string -> unit

val pp_left_aligned : ?char:char -> int -> Format.formatter -> string -> unit

module Handled_tags : Set.S with type elt = string

(** [handles tag] returns [true] if [tag] is handled by this module.

    This is useful when there's a possibility that a different handler for
    semantic tags is defined or used with a function that already uses this
    module to ignore these tags (see {!module:Tezos_base.Tezos_clic}) *)
val handles : string -> bool
