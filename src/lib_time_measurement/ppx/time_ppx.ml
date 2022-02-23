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

(** {1 Ppx for embedded time measurement tooling generation} *)

(** This PPX is used to handle the rewriting of Ocaml expressions
    annotated with the following attributes: [[@time.duration]],
    [[@time.duration_lwt]], [[@time.timestamp_pre]] and [[@time.flush]].

    If one of these annotations is detected on an expression,
    this last one will be wrapped in a thunk that will be given
    respectively to [duration], [duration_lwt], [timestamp_pre]
    or [flush] functions from
    [Tezos_time_measurement_runtime.Default.Time_measurement]
    module along with the label extracted from the attribute
    payload if needed.

    Example:

    {[
      let x = 40 + 2 [@time.duration label42] in x

      (* ==> *)

      let x =
        Tezos_time_measurement_runtime.Default.Time_measurement.duration
          "label42"
          (fun () -> 40 + 2)
    ]}

*)

(** {2 Expressions recursivity} *)

(** The PPX handles cases where sub-expressions are annotated.

    For example, the following program will be valid and both
    [@time.duration] attributes will be effective:

    {[
      (let x =
         "Hi there!" [@time.duration sub_expr]
       in x) [@time.duration expr]
    ]}

    It however needs to be handled with care: When tagging nested
    expressions, the outer time measurement will include the time
    spent to measure the sub-expression. *)

(** {2 Validity} *)

(** All dedicated attributes must follow the following rules in
    order to be semantically correct:
    - A same expression can not be annotated twice with the same
      annotation.
    - [@time.duration], [@time.duration_lwt] and [@time.timestamp_pre]
      must contain a payload.
    - [@time.duration], [@time.duration_lwt] and [@time.timestamp_pre]'s
      payload should be a valid Ocaml identifier (as described here:
      https://ocaml.org/manual/lex.html) optionally followed by an
      expression that evaluates to a string. *)

open Ppxlib

type measurement_key = {label : string; metadata : expression Option.t}

type rewriter =
  | Duration of measurement_key * location
  | Duration_lwt of measurement_key * location
  | Timestamp_pre of measurement_key * location
  | Flush of location

type rewriter_constants = {fn_name : string; attribute_name : string}

let duration key loc = Duration (key, loc)

let duration_lwt key loc = Duration_lwt (key, loc)

let timestamp_pre key loc = Timestamp_pre (key, loc)

let flush loc = Flush loc

let create_rewriter_constants fn_name =
  let namespaced name = "time" ^ "." ^ name in
  {fn_name; attribute_name = namespaced fn_name}

let duration_constants = create_rewriter_constants "duration"

let duration_lwt_constants = create_rewriter_constants "duration_lwt"

let timestamp_pre_constants = create_rewriter_constants "timestamp_pre"

let flush_constants = create_rewriter_constants "flush"

let attribute_names =
  [
    duration_constants.attribute_name;
    duration_lwt_constants.attribute_name;
    timestamp_pre_constants.attribute_name;
    flush_constants.attribute_name;
  ]

let constants_of_rewriter = function
  | Duration _ -> duration_constants
  | Duration_lwt _ -> duration_lwt_constants
  | Timestamp_pre _ -> timestamp_pre_constants
  | Flush _ -> flush_constants

let labeled_rewriter_of_attr_name name =
  if name = duration_constants.attribute_name then Some duration
  else if name = duration_lwt_constants.attribute_name then Some duration_lwt
  else if name = timestamp_pre_constants.attribute_name then Some timestamp_pre
  else None

let locaction_of_rewriter = function
  | Duration (_, loc) -> loc
  | Duration_lwt (_, loc) -> loc
  | Timestamp_pre (_, loc) -> loc
  | Flush loc -> loc

let error loc err =
  let open Format in
  let (msg, hint) =
    match err with
    | `Too_many_Detection attribute_name ->
        ( sprintf
            "Attribute [@%s] detected several times on the same expression."
            attribute_name,
          sprintf
            "A same expression can only be annotated once with [@%s] attribute."
            attribute_name )
    | `Invalid_payload ->
        ( "Invalid or empty attribute payload.",
          "This attribute payload must be of the form: <id> [<expr>]\n\
          \  where <id> should be a valid identifier\n\
          \  and <expr> an Ocaml expression evaluating to a string list." )
    | `Non_empty_payload ->
        ("Payload is not empty.", "This attribute should not contain a payload.")
  in
  Location.raise_errorf ~loc "time_measurement_ppx: %s\nHint: %s" msg hint

let get_attribute_name attribute = attribute.attr_name.txt

(** [has_attribute_name attribute name] evaluates if the name of the given
    [attribute] is equal to the given [name]. *)
let has_attribute_name attribute name = get_attribute_name attribute = name

(** [filter_attribute_with_name name attributes] evaluates in the
    list of [attributes] whose elements having the given [name]
    have been filtered out. *)
let filter_attribute_with_name name =
  List.filter (fun attr -> not @@ has_attribute_name attr name)

(** [filter_attribute_with_names names attributes] evaluates in
    the list of [attributes]
    whose elements having one of the given [names] have been filtered out. *)
let filter_attribute_with_names names attributes =
  List.fold_left
    (fun filtered name -> filter_attribute_with_name name filtered)
    attributes
    names

(** [fresh_identifier prefix] evaluates in a new identifier prefixed
    by [prefix] at each call. The function guarantees that it never
    returns the same identifier twice. *)
let fresh_identifier =
  let counter = ref 0 in
  fun prefix ->
    let fresh = prefix ^ string_of_int !counter in
    counter := !counter + 1 ;
    fresh

(** [key_of_payload location payload] extracts a [measurement_key] from the given
    [payload] if possible. The payload can be:
    - An ocaml identifier as described here: https://ocaml.org/manual/lex.html.
      In this case, this identifier will be used as key label.
    - An apply of expression where the first element should be an ocaml identifier
      an the second one an expression that should evaluate in a list of string.
      In this case, the first element will be used as key label and the second one
      as key metadata.
      Note that this preprocessing step will not enforce this expression to
      evaluate to a list of string. It will rather accept every expression and let
      the compilation check the typing during typing analysis afterward. *)
let key_of_payload loc = function
  | PStr [[%stri [%e? {pexp_desc = Pexp_ident {txt = Lident label; _}; _}]]] ->
      {label; metadata = None}
  (* Example:
     {[
       x [@time.duration fun_app]

       (* ==> *)

       Tezos_time_measurement_runtime.Default.Time_measurement.duration
         ("fun_app", [])
         (fun () -> x)
     ]}
  *)
  | PStr
      [
        [%stri
          [%e?
            {
              pexp_desc =
                Pexp_apply
                  ( {pexp_desc = Pexp_ident {txt = Lident label; _}; _},
                    [(Nolabel, expr)] );
              _;
            }]];
      ] ->
      {label; metadata = Some expr}
  (* Example:
     {[
       x [@time.duration fun_app ["123"]]

       (* ==> *)

       Tezos_time_measurement_runtime.Default.Time_measurement.duration
         ("fun_app", ["123"])
         (fun () -> x)
     ]}
  *)
  | _ -> error loc `Invalid_payload

let check_empty_payload loc = function
  | PStr [] -> ()
  | _ -> error loc `Non_empty_payload

(** [rewriter_of_attribute attribute] inspects the given [attribute] name
    and tries to recognise if it is related to this PPX. In this case, it
    will evaluate in the corresponding rewriter. *)
let rewriter_of_attribute ({attr_payload; attr_loc; _} as attribute) =
  match get_attribute_name attribute |> labeled_rewriter_of_attr_name with
  | Some rwt ->
      let label = key_of_payload attr_loc attr_payload in
      Some (rwt label attr_loc)
  | None ->
      if has_attribute_name attribute flush_constants.attribute_name then (
        check_empty_payload attr_loc attr_payload ;
        Some (Flush attr_loc))
      else None

(** [extract_rewriters attributes] inspects the given list of [attributes]
    and evaluates in the list of rewriters that have been identified with it. *)
let extract_rewriters = List.filter_map rewriter_of_attribute

(** [validate_rewriters rewriters] inspects the given list of [rewriters]
    and ensures that it is semantically correct. *)
let validate_rewriters rewriters =
  let duration_flag = ref false in
  let duration_lwt_flag = ref false in
  let timestamp_flag = ref false in
  let flush_flag = ref false in
  let check_visited flag name loc =
    if !flag then error loc (`Too_many_Detection name) else flag := true
  in
  let flag_of_rewriter = function
    | Duration _ -> duration_flag
    | Duration_lwt _ -> duration_lwt_flag
    | Timestamp_pre _ -> timestamp_flag
    | Flush _ -> flush_flag
  in
  List.iter
    (fun rewriter ->
      let {attribute_name; _} = constants_of_rewriter rewriter in
      let flag = flag_of_rewriter rewriter in
      let loc = locaction_of_rewriter rewriter in
      check_visited flag attribute_name loc)
    rewriters

(** [argument_of_key key loc] transforms the given measurement [key] into
    a labeled argument that intends be used in an expression application
    at the given [loc]ation. *)
let argument_of_key key loc =
  let label = Ast_helper.(Exp.constant @@ Const.string key.label) in
  let metadata =
    match key.metadata with Some expr -> expr | None -> [%expr []]
  in
  [%expr [%e label], [%e metadata]]

let ldot_of_non_empty_list (x, xs) =
  List.fold_left (fun acc ident -> Ldot (acc, ident)) (Lident x) xs

let time_measurement_longident name =
  ldot_of_non_empty_list
    ("Tezos_time_measurement_runtime", ["Default"; "Time_measurement"; name])

(** [wrap_with_labelled_call expr loc key fn] wraps the given
    [expr]ession into a thunk and gives it as an argument to a
    call of the function [fn] of module [Time_measurement].
    It will use the given measurement [key] to compute subsidiary
    arguments. *)
let wrap_with_labelled_call expr loc key fn =
  let open Ast_helper in
  let fn_longident = time_measurement_longident fn in
  let fexpr = Exp.ident @@ {txt = fn_longident; loc} in
  let last_arg = [%expr fun () -> [%e expr]] in
  let args = [(Nolabel, argument_of_key key loc); (Nolabel, last_arg)] in
  Exp.apply fexpr args

(** [bind_with_flush_call expr loc] binds the given [expr]ession
    with a [Lwt] promise that memoizes the results of [expr] and
    then call [Time_measurement.flush]. It then binds to a new
    [Lwt] promise that returns the memoized result of [expr]. *)
let bind_with_flush_call expr loc =
  let open Ast_helper in
  let identifier = fresh_identifier "__flush__id__" in
  let id_pattern = Pat.var {txt = identifier; loc} in
  let id_expr = Exp.ident {txt = Lident identifier; loc} in
  [%expr
    [%e expr] >>= fun [%p id_pattern] ->
    Tezos_time_measurement_runtime.Default.Time_measurement.flush ()
    >|= fun () -> [%e id_expr]]

(** [rewrite rewriters initial_expr] sequentially interpretes
    the given rewriters in order to rewrite the given expression
    [initial_expr]. *)
let rewrite rewriters initial_expr =
  let loc = initial_expr.pexp_loc in
  validate_rewriters rewriters ;
  List.fold_left
    (fun expr rewriter ->
      let {fn_name; _} = constants_of_rewriter rewriter in
      match rewriter with
      | Duration (key, _) | Duration_lwt (key, _) | Timestamp_pre (key, _) ->
          wrap_with_labelled_call expr loc key fn_name
      | Flush _ -> bind_with_flush_call expr loc)
    initial_expr
    rewriters

(** [remove_attributes expr] removes the attributes of the given
    [expr]ession if they match attributes handled by this PPX.*)
let remove_attributes expr =
  {
    expr with
    pexp_attributes =
      filter_attribute_with_names attribute_names expr.pexp_attributes;
  }

let mapper =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      (* Inspecting the attributes of the given expression [e] in order
         to get a description of what rewriting should be applied on [e]. *)
      let detected_rewriters = extract_rewriters e.pexp_attributes in
      (* Removing dedicated attributes since they don't need to
         stay in the AST as they will be processed by the rewriter. *)
      remove_attributes e
      (* Interpreting the detected rewriters to rewrite the cleaned
         expression. *)
      |> rewrite detected_rewriters
      (* Using [super#expression] to propagate to sub-expressions.
         Since [self#expression] is overriden, each sub-expression
         present in the AST will then be transformed as well inside
         the resulting expression by recursivity. *)
      |> super#expression
  end

let () =
  Driver.register_transformation "time_measurement" ~impl:mapper#structure
