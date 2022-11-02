(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Size = Protocol.Gas_input_size

type kind = Parsing | Unparsing

type code_or_data = Code | Data

type t =
  | Typechecker_workload of {
      t_kind : kind;
      code_or_data : code_or_data;
      micheline_size : Size.micheline_size;
      consumed : Size.t;
    }

let kind_encoding : kind Data_encoding.t =
  let open Data_encoding in
  def "kind_encoding"
  @@ string_enum [("parsing", Parsing); ("unparsing", Unparsing)]

let code_or_data_encoding : code_or_data Data_encoding.t =
  let open Data_encoding in
  def "code_or_data_encoding" @@ string_enum [("code", Code); ("data", Data)]

let encoding : t Data_encoding.t =
  let open Data_encoding in
  def "translator_trace_encoding"
  @@ conv
       (function
         | Typechecker_workload {t_kind; code_or_data; micheline_size; consumed}
           ->
             (t_kind, code_or_data, micheline_size, consumed))
       (fun (t_kind, code_or_data, micheline_size, consumed) ->
         Typechecker_workload {t_kind; code_or_data; micheline_size; consumed})
       (tup4
          kind_encoding
          code_or_data_encoding
          Size.micheline_size_encoding
          Size.encoding)

let pp_kind fmtr (kind : kind) =
  match kind with
  | Parsing -> Format.pp_print_string fmtr "Parsing"
  | Unparsing -> Format.pp_print_string fmtr "Unparsing"

let pp_code_or_data fmtr (x : code_or_data) =
  match x with
  | Code -> Format.pp_print_string fmtr "Code"
  | Data -> Format.pp_print_string fmtr "Data"

let pp fmtr (trace : t) =
  match trace with
  | Typechecker_workload {t_kind; code_or_data; micheline_size; consumed} ->
      Format.fprintf
        fmtr
        "typechecker_trace { %a; %a; %a; %a }"
        pp_kind
        t_kind
        pp_code_or_data
        code_or_data
        Size.pp_micheline_size
        micheline_size
        Size.pp
        consumed

let workload_to_sparse_vec (trace : t) =
  let name, {Size.traversal; int_bytes; string_bytes}, consumed =
    match trace with
    | Typechecker_workload {t_kind; code_or_data; micheline_size; consumed} ->
        let name =
          Format.asprintf "%a_%a" pp_kind t_kind pp_code_or_data code_or_data
        in
        (name, micheline_size, consumed)
  in
  let n s = name ^ "_" ^ s in
  let vars =
    [
      (n "traversal", float_of_int (Size.to_int traversal));
      (n "int_bytes", float_of_int (Size.to_int int_bytes));
      (n "string_bytes", float_of_int (Size.to_int string_bytes));
      (n "gas", float_of_int (Size.to_int consumed));
    ]
  in
  Sparse_vec.String.of_list vars

let data_typechecker_workload ctxt t_kind micheline_node ex_ty =
  let open Protocol in
  match ex_ty with
  | Script_typed_ir.Ex_ty ty ->
      let ctxt = Gas_helpers.set_limit ctxt in
      Lwt_main.run
        ( Script_ir_translator.parse_data
            ctxt
            ~elab_conf:(Script_ir_translator_config.make ~legacy:false ())
            ~allow_forged:false
            ty
            micheline_node
        |> Lwt.map Environment.wrap_tzresult
        >>= fun res ->
          match res with
          | Ok (_res, ctxt_after) ->
              let micheline_size = Size.of_micheline micheline_node in
              let consumed =
                Alpha_context.Gas.consumed ~since:ctxt ~until:ctxt_after
              in
              let trace =
                Typechecker_workload
                  {
                    t_kind;
                    code_or_data = Data;
                    micheline_size;
                    consumed =
                      Size.of_int (Z.to_int (Gas_helpers.fp_to_z consumed));
                  }
              in
              Lwt.return (Some trace)
          | Error errors ->
              Michelson_v1_error_reporter.report_errors
                ~details:true
                ~show_source:true
                Format.err_formatter
                errors ;
              Format.eprintf "@." ;
              Lwt.return None )

let code_typechecker_workload (ctxt : Protocol.Alpha_context.context)
    (t_kind : kind) (code : Protocol.Alpha_context.Script.node)
    (bef : Protocol.Script_ir_translator.ex_stack_ty) =
  let open Protocol in
  let ctxt = Gas_helpers.set_limit ctxt in
  let (Script_ir_translator.Ex_stack_ty stack_ty) = bef in
  Lwt_main.run
    ( Script_ir_translator.parse_instr
        Script_tc_context.data
        ctxt
        ~elab_conf:(Script_ir_translator_config.make ~legacy:false ())
        code
        stack_ty
    |> Lwt.map Environment.wrap_tzresult
    >>= fun res ->
      match res with
      | Ok (_res, ctxt_after) ->
          let micheline_size = Size.of_micheline code in
          let consumed =
            Alpha_context.Gas.consumed ~since:ctxt ~until:ctxt_after
          in
          let trace =
            Typechecker_workload
              {
                t_kind;
                code_or_data = Code;
                micheline_size;
                consumed = Size.of_int (Z.to_int (Gas_helpers.fp_to_z consumed));
              }
          in
          Lwt.return (Some trace)
      | Error errs ->
          Michelson_v1_error_reporter.report_errors
            ~details:true
            ~show_source:true
            Format.err_formatter
            errs ;
          Format.eprintf "@." ;
          Lwt.return None )
