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

  let saturated name = ["S"; name]

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

  let int i = call (saturated "safe_int") [Exp.constant (Const.int i)]

  let float f =
    call
      (saturated "safe_int")
      [call ["int_of_float"] [Exp.constant @@ Const.float (string_of_float f)]]

  let ( + ) x y = call ["+"] [x; y]

  let ( - ) x y = call ["-"] [x; y]

  let ( * ) x y = call ["*"] [x; y]

  let ( / ) x y = call ["/"] [x; y]

  let max x y = call (saturated "max") [x; y]

  let min x y = call (saturated "min") [x; y]

  let log2 x = call ["log2"] [x]

  let sqrt x = call ["sqrt"] [x]

  let free ~name = Exp.ident (loc_ident (string_of_fv name))

  let lt x y = call ["<"] [x; y]

  let eq x y = call ["="] [x; y]

  let shift_left i bits = call ["lsl"] [i; Exp.constant (Const.int bits)]

  let shift_right i bits = call ["lsr"] [i; Exp.constant (Const.int bits)]

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

let detach_funcs =
  let open Parsetree in
  let rec aux acc expr =
    match expr with
    | {
     pexp_desc = Pexp_fun (_, _, {ppat_desc = Ppat_var {txt = arg; _}; _}, expr');
     _;
    } ->
        aux (arg :: acc) expr'
    | _ -> (acc, expr)
  in
  aux []

let rec restore_funcs (acc, expr) =
  let open Ast_helper in
  match acc with
  | arg :: acc ->
      let expr = Exp.fun_ Nolabel None (Codegen_helpers.pvar arg) expr in
      restore_funcs (acc, expr)
  | [] -> expr

let generate_let_binding =
  let open Ast_helper in
  let open Codegen_helpers in
  let let_open_in x expr = Exp.open_ (Opn.mk (Mod.ident (loc_ident x))) expr in
  fun name expr ->
    let args, expr = detach_funcs expr in
    let expr =
      List.fold_left
        (fun e arg ->
          let var = ident arg in
          let patt = pvar arg in
          Exp.let_
            Nonrecursive
            [Vb.mk patt (call (saturated "safe_int") [var])]
            e)
        expr
        args
    in
    let expr = let_open_in "S.Syntax" expr in
    let expr = restore_funcs (args, expr) in
    Str.value Asttypes.Nonrecursive [Vb.mk (pvar name) expr]

let make_module structure_items =
  let open Ast_helper in
  let open Codegen_helpers in
  let suppress_unused_open_warning =
    Str.attribute
      (Attr.mk
         (loc_str "warning")
         (PStr [Str.eval (Exp.constant (Const.string "-33"))]))
  in
  let rename_saturation_repr =
    Str.module_
      (Mb.mk (loc (Some "S")) (Mod.ident (loc_ident "Saturation_repr")))
  in
  Str.module_
    (Mb.mk (Codegen_helpers.loc (Some "Generated"))
    @@ Mod.structure
         (suppress_unused_open_warning :: rename_saturation_repr
        :: structure_items))

let pp_structure_item fmtr generated = Pprintast.structure fmtr [generated]

(* ------------------------------------------------------------------------- *)

(* Precompose pretty-printing by let-lifting *)
module Lift_then_print = Costlang.Let_lift (Codegen)

(* ------------------------------------------------------------------------- *)
(* The data required to perform code generation is a map from variables to
   (floating point) coefficients. *)
type solution = float Free_variable.Map.t

let load_solution (fn : string) : solution =
  In_channel.with_open_bin fn Marshal.from_channel

let save_solution (s : solution) (fn : string) =
  Out_channel.with_open_bin fn @@ fun outfile -> Marshal.to_channel outfile s []

(* ------------------------------------------------------------------------- *)

let codegen (Model.For_codegen model) (sol : solution)
    (transform : Costlang.transform) (name : string) =
  let subst fv =
    match Free_variable.Map.find fv sol with
    | None ->
        raise (Fixed_point_transform.Codegen_error (Variable_not_found fv))
    | Some f -> f
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
  | Model.Preapplied _ -> None
  | Model.Packaged {conv = _; model} ->
      let module M = (val model) in
      let module M = M.Def (Subst_impl) in
      let expr = Lift_then_print.prj @@ Impl.prj @@ Subst_impl.prj M.model in
      Some (generate_let_binding name expr)

let codegen_module models sol transform =
  let items =
    List.filter_map
      (fun (name, model) ->
        let name = Printf.sprintf "cost_%s" name in
        codegen model sol transform name)
      models
  in
  make_module items
