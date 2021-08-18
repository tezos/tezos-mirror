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

open Base

type state =
  | Not_started
  | Started of {
      exception_handler : exn -> unit;
      mutable pending : unit Lwt.t list;
    }

let state = ref Not_started

let register p =
  match !state with
  | Not_started -> invalid_arg "Background.register: not started"
  | Started started_state ->
      let p =
        Lwt.catch
          (fun () -> p)
          (function
            | Lwt.Canceled -> unit
            | exn ->
                started_state.exception_handler exn ;
                unit)
      in
      started_state.pending <- p :: started_state.pending

let start exception_handler =
  match !state with
  | Not_started -> state := Started {exception_handler; pending = []}
  | Started _ -> invalid_arg "Background.start: already started"

let stop () =
  match !state with
  | Not_started -> unit
  | Started started_state ->
      let rec loop () =
        match started_state.pending with
        | [] -> unit
        | list ->
            started_state.pending <- [] ;
            (* Note that when registered promises are rejected, their wrapper
               cause them to resolve with unit instead. So this [join]
               cannot be rejected. (The user may still cancel it though, by
               canceling the promise returned by [stop].) *)
            let* () = Lwt.join list in
            (* Maybe some new promises were registered. *)
            loop ()
      in
      let* () = loop () in
      state := Not_started ;
      unit
