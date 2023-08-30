(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Kaitai.Types

let default_doc_spec = DocSpec.{summary = None; refs = []}

let cond_no_cond =
  AttrSpec.ConditionalSpec.{ifExpr = None; repeat = RepeatSpec.NoRepeat}

module Enum = struct
  type map = string * Kaitai.Types.EnumSpec.t

  let bool =
    ( "bool",
      EnumSpec.
        {
          path = [];
          map =
            [
              (0, EnumValueSpec.{name = "false"; doc = default_doc_spec});
              (255, EnumValueSpec.{name = "true"; doc = default_doc_spec});
            ];
        } )

  let add enums enum = if List.memq enum enums then enums else enum :: enums
end

module Attr = struct
  let bool =
    AttrSpec.
      {
        path = [];
        id = "bool";
        dataType = DataType.(NumericType (Int_type (Int1Type {signed = false})));
        cond = cond_no_cond;
        valid = Some (ValidationAnyOf [IntNum 0; IntNum 255]);
        doc = default_doc_spec;
        enum = Some (fst Enum.bool);
      }

  let u1 =
    AttrSpec.
      {
        path = [];
        id = "uint8";
        dataType = DataType.(NumericType (Int_type (Int1Type {signed = false})));
        cond = cond_no_cond;
        valid = None;
        doc = default_doc_spec;
        enum = None;
      }
end
