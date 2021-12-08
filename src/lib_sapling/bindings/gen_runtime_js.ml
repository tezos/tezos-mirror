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

let gen_fn ~cname ~stub_name fmt _fn =
  Format.fprintf
    fmt
    "//Provides: %s@.//Requires: caml_failwith@.function %s (_) { \
     caml_failwith('%s unimplemetned'); }@."
    stub_name
    stub_name
    cname

let gen_value ~cname ~stub_name fmt _typ =
  Format.fprintf
    fmt
    "//Provides: %s@.//Requires: caml_failwith@.function %s (_) { \
     caml_failwith('%s unimplemetned'); }@."
    stub_name
    stub_name
    cname

module type FOREIGN = Ctypes.FOREIGN

module type FOREIGN' = FOREIGN with type 'a result = unit

module type BINDINGS = functor (F : FOREIGN') -> sig end

let write_js fmt (module B : BINDINGS) =
  let module M = B (struct
    let prefix = ""

    let counter = ref 0

    let var prefix name =
      incr counter ;
      Printf.sprintf "%s_%d_%s" prefix !counter name

    type 'a fn = 'a Ctypes.fn

    type 'a return = 'a

    type 'a result = unit

    let foreign cname fn = gen_fn ~cname ~stub_name:(var prefix cname) fmt fn

    let foreign_value cname typ =
      gen_value ~cname ~stub_name:(var prefix cname) fmt typ

    let returning = Ctypes.returning

    let ( @-> ) = Ctypes.( @-> )
  end) in
  ()

let write m = write_js Format.std_formatter m

let () = write (module Rustzcash_ctypes_bindings.Bindings)
