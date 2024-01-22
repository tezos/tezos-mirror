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

open Jingoo.Jg_types

type item = Global_variables.var

type items = item list

type with_items = Seq of items | Prod of items list

let items_encoding =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    union
      [
        c.mk_case
          "singleton"
          Global_variables.var_encoding
          (function [x] -> Some x | _ -> None)
          (fun x -> [x]);
        c.mk_case "list" (list Global_variables.var_encoding) Option.some Fun.id;
      ])

let with_items_encoding =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    union
      [
        c.mk_case
          "seq"
          items_encoding
          (function Seq l -> Some l | _ -> None)
          (fun l -> Seq l);
        c.mk_case
          "prod"
          (obj1 (req "product" (list items_encoding)))
          (function Prod p -> Some p | _ -> None)
          (fun p -> Prod p);
      ])

type header = {
  name : string;
  with_items : with_items option;
  mode : Execution_params.mode;
  vars_updates : Global_variables.update list;
}

type 'uri body =
  | Remote_procedure of {procedure : 'uri Remote_procedure.packed}
  | Copy of {source : string; destination : string}

type 'uri t = {header : header; body : 'uri body}

let header_encoding =
  Data_encoding.(
    conv
      (fun {name; with_items; mode; vars_updates} ->
        (name, with_items, mode, vars_updates))
      (fun (name, with_items, mode, vars_updates) ->
        {name; with_items; mode; vars_updates})
      (obj4
         (req "name" string)
         (opt "with_items" with_items_encoding)
         (dft "run_items" Execution_params.mode_encoding Sequential)
         (dft "vars_updates" Global_variables.updates_encoding [])))

let encoding uri_encoding =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    delayed @@ fun () ->
    union
      [
        c.mk_case
          "remote_procedure"
          (Remote_procedure.merged_encoding header_encoding uri_encoding)
          (function
            | {header; body = Remote_procedure {procedure}} ->
                Some (header, procedure)
            | _ -> None)
          (fun (header, procedure) ->
            {header; body = Remote_procedure {procedure}});
        c.mk_case
          "copy"
          (merge_objs
             header_encoding
             (obj1
                (req
                   "copy"
                   (obj2 (req "local_path" string) (req "remote_path" string)))))
          (function
            | {header; body = Copy {source; destination}} ->
                Some (header, (source, destination))
            | _ -> None)
          (fun (header, (source, destination)) ->
            {header; body = Copy {source; destination}});
      ])

let expand_body ~self ~vars ~agent ~re ~item =
  let run = Template.run ~vars ~agent ~re ~item ~res:Tnull in
  function
  | Remote_procedure {procedure} ->
      Remote_procedure
        {
          procedure =
            Remote_procedure.expand ~self ~vars ~agent ~re ~item procedure;
        }
  | Copy {source; destination} ->
      let source = run source in
      let destination = run destination in
      Copy {source; destination}

let expand_item ~vars ~agent ~re def =
  match Global_variables.tvalue_of_var def with
  | Tstr def -> (
      let def = Template.run ~vars ~agent ~re ~item:Tnull ~res:Tnull def in
      match def =~** rex {|(\d+)\.\.(\d+)|} with
      | Some (range_from, range_to) ->
          let range_from = int_of_string range_from in
          let range_to = int_of_string range_to in
          Seq.init (range_to - range_from + 1) (fun x -> Tint (range_from + x))
      | None -> (
          match int_of_string_opt def with
          | Some i -> Seq.return (Tint i)
          | None -> (
              match bool_of_string_opt def with
              | Some b -> Seq.return (Tbool b)
              | None -> Seq.return (Tstr def))))
  | x -> Seq.return x
