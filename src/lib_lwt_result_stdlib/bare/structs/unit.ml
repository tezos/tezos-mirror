(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

include Stdlib.Unit

let unit = ()

let unit_s = Monad.Lwt_syntax.return_unit

let unit_e = Monad.Result_syntax.return_unit

let unit_es = Monad.Lwt_result_syntax.return_unit

let catch ?(catch_only = fun _ -> true) f =
  match f () with
  | () -> ()
  | exception ((Stack_overflow | Out_of_memory) as e) -> Lwt.reraise e
  | exception e -> if catch_only e then () else Lwt.reraise e

let catch_f ?(catch_only = fun _ -> true) f h =
  match f () with
  | () -> ()
  | exception ((Stack_overflow | Out_of_memory) as e) -> Lwt.reraise e
  | exception e -> if catch_only e then h e else Lwt.reraise e

let catch_s ?(catch_only = fun _ -> true) f =
  Lwt.catch f (function
    | (Stack_overflow | Out_of_memory) as e -> Lwt.reraise e
    | e -> if catch_only e then Lwt.return_unit else Lwt.reraise e)
