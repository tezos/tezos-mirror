(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* ------------------------------------------------------------------------- *)
(* OCaml codegen *)

module Codegen_helpers = struct
  open Ast_helper

  let loc txt = {Asttypes.txt; loc = Location.none}

  let loc_ident x = {Asttypes.txt = Longident.Lident x; loc = Location.none}

  let loc_str (x : string) = {Asttypes.txt = x; loc = Location.none}

  let ident x = Exp.ident (loc_ident x)

  let pvar x = Pat.var (loc_str x)

  let call f args =
    let f = WithExceptions.Option.get ~loc:__LOC__ @@ Longident.unflatten f in
    let args = List.map (fun x -> (Asttypes.Nolabel, x)) args in
    Exp.(apply (ident (loc f)) args)

  let string_of_fv fv = Format.asprintf "%a" Free_variable.pp fv
end

module Codegen : Costlang.S with type 'a repr = Parsetree.expression = struct
  type 'a repr = Parsetree.expression

  type size = int

  open Codegen_helpers
  open Ast_helper

  let true_ = Exp.construct (loc_ident "true") None

  let false_ = Exp.construct (loc_ident "false") None

  let int i = Exp.constant (Const.int i)

  let float f =
    call ["int_of_float"] [Exp.constant @@ Const.float (string_of_float f)]

  let ( + ) x y = call ["+"] [x; y]

  let ( - ) x y = call ["-"] [x; y]

  let ( * ) x y = call ["*"] [x; y]

  let ( / ) x y = call ["/"] [x; y]

  let max x y = call ["max"] [x; y]

  let min x y = call ["min"] [x; y]

  let log2 x = call ["log2"] [x]

  let free ~name = Exp.ident (loc_ident (string_of_fv name))

  let lt x y = call ["<"] [x; y]

  let eq x y = call ["="] [x; y]

  let shift_left i bits = call ["lsl"] [i; int bits]

  let shift_right i bits = call ["lsr"] [i; int bits]

  let lam ~name f =
    let patt = pvar name in
    let var = ident name in
    Exp.fun_ Nolabel None patt (f var)

  let app x y = Exp.apply x [(Nolabel, y)]

  let let_ ~name m f =
    let id = ident name in
    let var = pvar name in
    Exp.let_ Nonrecursive [Vb.mk var m] (f id)

  let if_ cond ift iff = Exp.ifthenelse cond ift (Some iff)
end

let make_module bindings =
  let open Ast_helper in
  let structure_items =
    List.map
      (fun (name, expr) ->
        let name = Printf.sprintf "cost_%s" name in
        Str.value
          Asttypes.Nonrecursive
          [Vb.mk (Codegen_helpers.pvar name) expr])
      bindings
  in
  Str.module_
    (Mb.mk (Codegen_helpers.loc (Some "Generated")) @@ Mod.structure structure_items)

let pp_expr fmtr expr =
  Pprintast.expression fmtr expr

let pp_structure_item fmtr generated =
  Pprintast.structure fmtr [generated]

(* ------------------------------------------------------------------------- *)

(* Precompose pretty-printing by let-lifting *)
module Lift_then_print = Costlang.Let_lift (Codegen)

(* ------------------------------------------------------------------------- *)
(* The data required to perform code generation is a map from variables to
   (floating point) coefficients. *)
type solution = float Free_variable.Map.t

let load_solution (fn : string) : solution =
  let infile = open_in fn in
  try
    let res = Marshal.from_channel infile in
    close_in infile ; res
  with exn ->
    close_in infile ;
    Format.eprintf "Codegen.load_solution: could not load %s@." fn ;
    raise exn

let save_solution (s : solution) (fn : string) =
  let outfile = open_out fn in
  Marshal.to_channel outfile s [] ;
  close_out outfile

(* ------------------------------------------------------------------------- *)

let codegen (Model.For_codegen model) (sol : solution)
    (transform : Costlang.transform) =
  let subst fv =
    match Free_variable.Map.find fv sol with
    | None ->
        raise (Fixed_point_transform.Codegen_error (Variable_not_found fv))
    | Some f ->
        f
  in
  let module T = (val transform) in
  let module Impl = T (Lift_then_print) in
  let module Subst_impl =
    Costlang.Subst
      (struct
        let subst = subst
      end)
      (Impl)
  in
  match model with
  | Model.Preapplied _ ->
      None
  | Model.Packaged {conv = _; model} ->
      let module M = (val model) in
      let module M = M.Def (Subst_impl) in
      let expr = Lift_then_print.prj @@ Impl.prj @@ Subst_impl.prj M.model in
      Some expr

let codegen_module models sol transform =
  let items =
    List.filter_map
      (fun (name, model) ->
        codegen model sol transform |> Option.map (fun expr -> (name, expr)))
      models
  in
  make_module items
