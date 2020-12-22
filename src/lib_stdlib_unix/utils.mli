(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Print over the current [stdout] line. Takes a message formatting function of
    the form [(fun m -> m <format_string>)]. Message formatting occurs only when
    the line is actually printed (i.e. when [(fst refresh_rate) mod (snd
    refresh_rate) = 0]). [refresh_rate] defaults to always printing the supplied
    message.

    {2 Examples:}

    - [display_progress (fun m -> m "Loading... %d/100" percent)].

    - [display_progress ~refresh_rate:(index, 1000) (fun m -> m "Written %d
      bytes" total)]. Display progress message when [index] is divisible by
      1000.
 *)
val display_progress :
  ?refresh_rate:int * int ->
  ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) ->
  unit

(** Finalizes progress display *)
val display_progress_end : unit -> unit
