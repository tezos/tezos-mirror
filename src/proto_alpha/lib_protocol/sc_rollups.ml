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

open Alpha_context.Sc_rollup

module PVM = struct
  type boot_sector = Alpha_context.Sc_rollup.PVM.boot_sector

  module type S = sig
    val name : string

    val parse_boot_sector : string -> boot_sector option

    val pp_boot_sector : Format.formatter -> boot_sector -> unit
  end

  type t = (module S)
end

let all = [Kind.Example_arith]

let kind_of_string = function "arith" -> Some Kind.Example_arith | _ -> None

let example_arith_pvm = (module Sc_rollup_arith : PVM.S)

let of_kind = function Kind.Example_arith -> example_arith_pvm

let kind_of (module M : PVM.S) =
  match kind_of_string M.name with
  | Some k -> k
  | None ->
      failwith
        (Format.sprintf "The module named %s is not in Sc_rollups.all." M.name)

let from ~name = Option.map of_kind (kind_of_string name)

let all_names =
  List.map
    (fun k ->
      let (module M : PVM.S) = of_kind k in
      M.name)
    all
