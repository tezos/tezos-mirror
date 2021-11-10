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

let ( >>== ) p v = Lwt_main.run (Lwt.map (( = ) v) p)

let () =
  let open Support.Lib.Seq in
  assert (first empty = None) ;
  assert (first (return 0) = Some 0) ;
  assert (first (cons 0 empty) = Some 0) ;
  assert (first (append empty empty) = None) ;
  assert (List.of_seq empty = []) ;
  assert (List.of_seq (return 0) = [0]) ;
  assert (List.of_seq (cons 0 empty) = [0]) ;
  assert (List.of_seq (append empty empty) = [])

let () =
  let open Support.Lib.Seq_e in
  assert (first empty = None) ;
  assert (first (return 0) = Some (Ok 0)) ;
  assert (first (return_e (Ok 0)) = Some (Ok 0)) ;
  assert (first (return_e (Error ())) = Some (Error ())) ;
  assert (first (interrupted ()) = Some (Error ())) ;
  assert (first (cons 0 empty) = Some (Ok 0)) ;
  assert (first (cons_e (Ok 0) empty) = Some (Ok 0)) ;
  assert (first (cons_e (Error ()) empty) = Some (Error ())) ;
  assert (first (cons_e (Ok 0) (interrupted ())) = Some (Ok 0)) ;
  assert (first (append empty empty) = None)

let () =
  let open Support.Lib.Seq_s in
  assert (first empty >>== None) ;
  assert (first (return 0) >>== Some 0) ;
  assert (first (cons 0 empty) >>== Some 0) ;
  assert (first (append empty empty) >>== None)

let () =
  let open Support.Lib.Seq_es in
  assert (first empty >>== None) ;
  assert (first (return 0) >>== Some (Ok 0)) ;
  assert (first (return_e (Ok 0)) >>== Some (Ok 0)) ;
  assert (first (return_e (Error ())) >>== Some (Error ())) ;
  assert (first (interrupted ()) >>== Some (Error ())) ;
  assert (first (cons 0 empty) >>== Some (Ok 0)) ;
  assert (first (cons_e (Ok 0) empty) >>== Some (Ok 0)) ;
  assert (first (cons_e (Error ()) empty) >>== Some (Error ())) ;
  assert (first (cons_e (Ok 0) (interrupted ())) >>== Some (Ok 0)) ;
  assert (first (append empty empty) >>== None)
