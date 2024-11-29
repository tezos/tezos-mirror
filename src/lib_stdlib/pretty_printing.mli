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
