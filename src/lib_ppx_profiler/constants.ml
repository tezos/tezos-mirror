(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* This rewriter handles ppxes starting with profiler. *)
let namespace = "profiler"

type t = {attribute_name : string; action : string}

let create_constant action =
  let attribute_name = namespace ^ "." ^ action in
  {action; attribute_name}

(** Constant representing [@profiler.aggregate] *)
let aggregate_constant = create_constant "aggregate"

(** Constant representing [@profiler.aggregate_f] *)
let aggregate_f_constant = create_constant "aggregate_f"

(** Constant representing [@profiler.aggregate_s] *)
let aggregate_s_constant = create_constant "aggregate_s"

(** Constant representing [@profiler.mark] *)
let mark_constant = create_constant "mark"

(** Constant representing [@profiler.overwrite] *)
let overwrite_constant = create_constant "overwrite"

(** Constant representing [@profiler.record] *)
let record_constant = create_constant "record"

(** Constant representing [@profiler.record_f] *)
let record_f_constant = create_constant "record_f"

(** Constant representing [@profiler.record_s] *)
let record_s_constant = create_constant "record_s"

(** Constant representing [@profiler.reset_block_section] *)
let reset_block_section_constant = create_constant "reset_block_section"

(** Constant representing [@profiler.span] *)
let span_constant = create_constant "span"

(** Constant representing [@profiler.span_s] *)
let span_f_constant = create_constant "span_f"

(** Constant representing [@profiler.span_s] *)
let span_s_constant = create_constant "span_s"

(** Constant representing [@profiler.stamp] *)
let stamp_constant = create_constant "stamp"

(** Constant representing [@profiler.stop] *)
let stop_constant = create_constant "stop"

(** Constant representing [@profiler.wrap_f] *)
let wrap_f_constant = create_constant "wrap_f"

(** Constant representing [@profiler.wrap_s] *)
let wrap_s_constant = create_constant "wrap_s"

let get_attribute t = t.attribute_name

let get_action t = t.action

(* This list should always be up to date by containing all the constants
   defined above *)
let constants =
  [
    aggregate_constant;
    aggregate_f_constant;
    aggregate_s_constant;
    mark_constant;
    overwrite_constant;
    record_constant;
    record_f_constant;
    record_s_constant;
    reset_block_section_constant;
    span_constant;
    span_f_constant;
    span_s_constant;
    stamp_constant;
    stop_constant;
    wrap_f_constant;
    wrap_s_constant;
  ]

(** Attributes that are handled by this ppx *)
let attribute_names = List.map get_attribute constants

let has_attribute_name attribute name =
  String.equal (Ppxlib_helper.get_attribute_name attribute) name

let filter_out_attributes attributes name =
  List.filter
    (fun attribute -> not (has_attribute_name attribute name))
    attributes

let filter_out_all_handled_attributes attributes =
  List.fold_left
    (fun filtered_attributes handled_attribute_name ->
      filter_out_attributes filtered_attributes handled_attribute_name)
    attributes
    attribute_names
