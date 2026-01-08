(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Script_typed_ir

type native = Ex_kind : ('arg, 'storage) Script_native_types.kind -> native

type ('arg, 'storage) implementation =
  | Lambda : {
      code :
        (('arg, 'storage) pair, (operation Script_list.t, 'storage) pair) lambda;
    }
      -> ('arg, 'storage) implementation
  | Native : {
      kind : ('arg, 'storage) Script_native_types.kind;
    }
      -> ('arg, 'storage) implementation

type ('arg, 'storage) script =
  | Script : {
      implementation : ('arg, 'storage) implementation;
      arg_type : ('arg, _) ty;
      storage : 'storage;
      storage_type : ('storage, _) ty;
      views : view_map;
      entrypoints : 'arg entrypoints;
      code_size : Cache_memory_helpers.sint;
          (* This is an over-approximation of the value size in memory, in
             bytes, of the contract's static part, that is its source
             code. This includes the code of the contract as well as the code
             of the views. The storage size is not taken into account by this
             field as it has a dynamic size. *)
    }
      -> ('arg, 'storage) script
