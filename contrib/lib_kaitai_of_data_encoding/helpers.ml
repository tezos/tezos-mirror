(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Kaitai.Types

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

let default_class_spec ~encoding_name =
  ClassSpec.
    {
      fileName = None;
      path = [];
      meta = default_meta_spec ~encoding_name;
      doc = default_doc_spec;
      toStringExpr = None;
      params = [];
      seq = [];
      types = [];
      instances = [];
      enums = [];
    }

let class_spec_of_attr ~encoding_name ?(enums = []) attr =
  {(default_class_spec ~encoding_name) with seq = [attr]; enums}
