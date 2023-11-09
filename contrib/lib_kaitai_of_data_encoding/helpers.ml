(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Kaitai.Types

(* Generator for ids in a tuple. *)
type tid_gen = unit -> string

let mk_tid_gen prefix : tid_gen =
  let i = ref 0 in
  fun () ->
    let id = prefix ^ "_field" ^ string_of_int !i in
    incr i ;
    id

let default_doc_spec = DocSpec.{summary = None; refs = []}

let cond_no_cond =
  AttrSpec.ConditionalSpec.{ifExpr = None; repeat = RepeatSpec.NoRepeat}

let default_attr_spec ~id =
  AttrSpec.
    {
      id;
      dataType = DataType.AnyType;
      cond = cond_no_cond;
      valid = None;
      doc = default_doc_spec;
      enum = None;
      size = None;
    }

let default_meta_spec ?(imports = []) () =
  MetaSpec.
    {
      isOpaque = false;
      id = None;
      endian = Some `BE;
      bitEndian = None;
      encoding = None;
      forceDebug = false;
      opaqueTypes = None;
      zeroCopySubstream = None;
      imports;
    }

let default_class_spec ?description ?imports () =
  ClassSpec.
    {
      fileName = None;
      meta = default_meta_spec ?imports ();
      doc = {default_doc_spec with summary = description};
      toStringExpr = None;
      params = [];
      seq = [];
      types = [];
      instances = [];
      enums = [];
    }

let add_uniq_assoc mappings ((k, v) as mapping) =
  match List.assoc_opt k mappings with
  | None -> mapping :: mappings
  | Some vv ->
      if v = vv then mappings
      else raise (Invalid_argument ("Mappings.add: duplicate keys (" ^ k ^ ")"))

let class_spec_of_attrs ?description ?(enums = []) ?(types = [])
    ?(instances = []) ?imports attrs =
  {
    (default_class_spec ?description ?imports ()) with
    seq = attrs;
    enums;
    types;
    instances;
  }

let default_instance_spec ~id value =
  InstanceSpec.
    {
      doc = default_doc_spec;
      descr = InstanceSpec.ValueInstanceSpec {id; value; ifExpr = None};
    }

let merge_summaries attr summary =
  match (attr.AttrSpec.doc.summary, summary) with
  | _, None -> attr
  | None, Some _ -> {attr with doc = {default_doc_spec with summary}}
  | Some sumsum, Some s ->
      let summary = Some (sumsum ^ "\n\n" ^ s) in
      {attr with doc = {default_doc_spec with summary}}
