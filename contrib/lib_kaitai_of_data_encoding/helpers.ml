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

let default_attr_spec =
  AttrSpec.
    {
      path = [];
      id = "";
      dataType = DataType.AnyType;
      cond = cond_no_cond;
      valid = None;
      doc = default_doc_spec;
      enum = None;
      size = None;
    }

let default_meta_spec ~encoding_name =
  MetaSpec.
    {
      path = [];
      isOpaque = false;
      id = Some encoding_name;
      endian = Some `BE;
      bitEndian = None;
      encoding = None;
      forceDebug = false;
      opaqueTypes = None;
      zeroCopySubstream = None;
      imports = [];
    }

let default_class_spec ~encoding_name ?description () =
  ClassSpec.
    {
      fileName = None;
      path = [];
      meta = default_meta_spec ~encoding_name;
      isTopLevel = true;
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
      else raise (Invalid_argument "Mappings.add: duplicate keys")

let types_field_from_attr_seq base attributes =
  let types =
    List.filter_map
      (fun {AttrSpec.dataType; _} ->
        match dataType with
        | DataType.ComplexDataType (UserType class_spec) -> (
            match class_spec.meta.id with
            | Some id -> Some (id, class_spec)
            | None -> failwith "User defined type has no name")
        | _ -> None)
      attributes
  in
  List.fold_left add_uniq_assoc base types

let class_spec_of_attrs ~encoding_name ?description ?(enums = []) ?(types = [])
    ?(instances = []) attrs =
  let types = types_field_from_attr_seq types attrs in
  {
    (default_class_spec ~encoding_name ?description ()) with
    seq = attrs;
    enums;
    types;
    instances;
  }

let default_instance_spec ~id value =
  InstanceSpec.
    {
      doc = default_doc_spec;
      descr =
        InstanceSpec.ValueInstanceSpec
          {id; path = []; value; ifExpr = None; dataTypeOpt = None};
    }
