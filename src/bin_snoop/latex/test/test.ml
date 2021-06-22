(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@tezos.com>                       *)
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

open Latex
open Syntax

let rec document = {title = "test"; sections = [section]}

and section = Section ("section 1", contents)

and contents =
  [
    Text
      [
        Text_blob (Normal, "Ceci est un morceau de maths:");
        Inline_math_blob "3 \\times x + 2";
      ];
    Figure ([], {filename = "lol.png"; size = Some (Width_cm 17)});
    table;
  ]

and table =
  let spec = [Vbar; L; Vbar; L; Vbar] in
  let rows =
    [
      Hline;
      Row
        [
          [Text_blob (Normal, "Model name:")];
          [Text_blob (Normal, "timer\\_ir\\_model")];
        ];
      Row [[Text_blob (Normal, "Benchmark name:")]; [Text_blob (Normal, "ABS")]];
      Row
        [
          [Text_blob (Normal, "Model:")];
          [
            Inline_math_blob
              "N\\_Abs\\_int1 \\times size + N\\_Abs\\_int\\_const";
          ];
        ];
      Hline;
    ]
  in
  Table (spec, rows)

let () = Format.printf "%a" Latex_pp.pp document
