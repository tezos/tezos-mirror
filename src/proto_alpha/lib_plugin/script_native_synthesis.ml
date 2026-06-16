(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Environment
open Environment.Error_monad
open Protocol
open Protocol.Alpha_context

type error += Native_script_synthesis_failed of {kind : Script_native_repr.t}

let () =
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"plugin.nativeScriptSynthesisFailed"
    ~title:"Native script synthesis failed"
    ~description:
      "The Michelson script of a native contract could not be synthesized from \
       its typed interface."
    ~pp:(fun ppf _kind ->
      Format.fprintf
        ppf
        "The Michelson script of a native contract could not be synthesized \
         from its typed interface.")
    Data_encoding.(obj1 (req "kind" Script_native_repr.encoding))
    (function Native_script_synthesis_failed {kind} -> Some kind | _ -> None)
    (fun kind -> Native_script_synthesis_failed {kind})

(* Marker string embedded in the stub bodies so that the synthesized code is
   recognizable as a non-functional placeholder rather than the contract's real
   logic. *)
let stub_marker = "native contract: no Michelson code"

(* A stub instruction body that type-checks against any input/output stack: it
   discards the input, pushes a marker string and fails. Used both for the
   contract code and for every view body. *)
let stub_body loc =
  Micheline.Seq
    ( loc,
      [
        Micheline.Prim (loc, Script.I_DROP, [], []);
        Micheline.Prim
          ( loc,
            Script.I_PUSH,
            [
              Micheline.Prim (loc, Script.T_string, [], []);
              Micheline.String (loc, stub_marker);
            ],
            [] );
        Micheline.Prim (loc, Script.I_FAILWITH, [], []);
      ] )

let of_native ctxt kind ~storage =
  let open Result_syntax in
  record_trace (Native_script_synthesis_failed {kind})
  @@
  (* Unparsing consumes gas; this is an RPC-only synthesis, so run it
     unaccounted like the existing normalized-script handler. *)
  let ctxt = Gas.set_unlimited ctxt in
  let loc = Micheline.dummy_location in
  let* (Script_native_types.Ex_kind_and_types
          (typed_kind, Script_typed_ir.{arg_type; storage_type; entrypoints; _}))
      =
    Script_native_types.get_typed_kind_and_types kind
  in
  (* Real parameter type carrying the real entrypoint annotations. *)
  let* parameter_node, ctxt =
    Script_ir_unparser.unparse_parameter_ty ~loc ctxt arg_type ~entrypoints
  in
  (* Real declared storage type (coherent with the real storage returned). *)
  let* storage_node, _ctxt =
    Script_ir_unparser.unparse_ty ~loc ctxt storage_type
  in
  (* Real views: real input/output types, stubbed bodies. *)
  let* views = Script_native.get_views typed_kind in
  let* view_nodes, _ctxt =
    Script_map.fold
      (fun name
           (Script_native_types.Ex_view
              {
                ty =
                  Script_native_types.
                    {
                      input_ty = Script_typed_ir.Ty_ex_c input_ty;
                      output_ty = Script_typed_ir.Ty_ex_c output_ty;
                    };
                _;
              })
           acc
         ->
        let* view_nodes, ctxt = acc in
        let* input_node, ctxt =
          Script_ir_unparser.unparse_ty ~loc ctxt input_ty
        in
        let* output_node, ctxt =
          Script_ir_unparser.unparse_ty ~loc ctxt output_ty
        in
        let view_node =
          Micheline.Prim
            ( loc,
              Script.K_view,
              [
                Micheline.String (loc, Script_string.to_string name);
                input_node;
                output_node;
                stub_body loc;
              ],
              [] )
        in
        return (view_node :: view_nodes, ctxt))
      views
      (return ([], ctxt))
  in
  let code_expr =
    Micheline.strip_locations
    @@ Micheline.Seq
         ( loc,
           [
             Micheline.Prim (loc, Script.K_parameter, [parameter_node], []);
             Micheline.Prim (loc, Script.K_storage, [storage_node], []);
             Micheline.Prim (loc, Script.K_code, [stub_body loc], []);
           ]
           @ List.rev view_nodes )
  in
  return {Script.code = Script.lazy_expr code_expr; storage}
