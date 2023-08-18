(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Jingoo
open Jg_types

let run ~agent ~(vars : Global_variables.t) ~(res : tvalue) ~re ~item template =
  Jg_template.from_string
    ~models:
      [
        ( "var",
          Tfun
            (fun ?kwargs:_ tvalue ->
              match tvalue with
              | Tstr key -> Global_variables.(get vars key |> tvalue_of_var)
              | _ -> raise (Invalid_argument "Template.run: invalid input")) );
        ( "basename",
          Tfun
            (fun ?kwargs:_ tvalue ->
              match tvalue with
              | Tstr str -> Tstr (Filename.basename str)
              | _ -> raise (Invalid_argument "Template.run: invalid input")) );
        ( "hex",
          Tfun
            (fun ?kwargs:_ tvalue ->
              match tvalue with
              | Tstr str ->
                  let (`Hex hex) = Hex.of_string str in
                  Tstr hex
              | _ -> raise (Invalid_argument "Template.run: invalid input")) );
        ("vars", Global_variables.tvalue_of_vars vars);
        ("res", res);
        ("re", re);
        ("item", item);
        ("agent", agent);
      ]
    template

let expand_update_var ~vars ~agent ~re ~item ~res u =
  let open Global_variables in
  let run = run ~vars ~agent ~res ~re ~item in
  let key = run u.key in
  let value = Option.map run u.value in
  {u with key; value}

let expand_agent ~vars = run ~vars ~agent:Tnull ~re:Tnull ~res:Tnull ~item:Tnull
