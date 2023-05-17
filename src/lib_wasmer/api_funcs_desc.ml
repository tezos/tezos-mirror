(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Ctypes
module Types = Api_types

(** This functor is used by Ctypes to generate function bindings for the
    Wasmer C API. *)
module Functions (S : FOREIGN) = struct
  open S

  (** This functor corresponds to the [WASM_DECLARE_VEC] macro in [wasm.h]. *)
  module Declare_vec (Vector : sig
    include Types.Named_type

    type item

    val item : item typ
  end) =
  struct
    let new_empty =
      foreign
        ("wasm_" ^ Vector.name ^ "_new_empty")
        (ptr Vector.t @-> returning void)

    let new_ =
      foreign
        ("wasm_" ^ Vector.name ^ "_new")
        (ptr Vector.t @-> size_t @-> ptr Vector.item @-> returning void)

    let new_uninitialized =
      foreign
        ("wasm_" ^ Vector.name ^ "_new_uninitialized")
        (ptr Vector.t @-> size_t @-> returning void)

    let delete =
      foreign
        ("wasm_" ^ Vector.name ^ "_delete")
        (ptr Vector.t @-> returning void)
  end

  (** Generate a pointer type from another type. *)
  module Ptr (Item : sig
    val name : string

    type t

    val t : t typ
  end) =
  struct
    include Item

    type t = Item.t Ctypes.ptr

    let t = ptr Item.t
  end

  (** Functions with the [wasmer_] prefix *)
  module Wasmer = struct
    module Compiler = struct
      let is_available =
        foreign
          "wasmer_is_compiler_available"
          (Types.Wasmer.Compiler.t @-> returning bool)
    end

    module Features = struct
      let new_ =
        foreign
          "wasmer_features_new"
          (void @-> returning (ptr Types.Wasmer.Features.t))

      let bulk_memory =
        foreign
          "wasmer_features_bulk_memory"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)

      let memory64 =
        foreign
          "wasmer_features_memory64"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)

      let module_linking =
        foreign
          "wasmer_features_module_linking"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)

      let multi_memory =
        foreign
          "wasmer_features_multi_memory"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)

      let multi_value =
        foreign
          "wasmer_features_multi_value"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)

      let reference_types =
        foreign
          "wasmer_features_reference_types"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)

      let simd =
        foreign
          "wasmer_features_simd"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)

      let tail_call =
        foreign
          "wasmer_features_tail_call"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)

      let threads =
        foreign
          "wasmer_features_threads"
          (ptr Types.Wasmer.Features.t @-> bool @-> returning bool)
    end

    let last_error_length =
      foreign "wasmer_last_error_length" (void @-> returning int)

    let last_error =
      foreign "wasmer_last_error_message" (ptr char @-> int @-> returning int)
  end

  (** Functions with the [wasm_config_] prefix *)
  module Config = struct
    let new_ =
      foreign "wasm_config_new" (void @-> returning (ptr Types.Config.t))

    let set_compiler =
      foreign
        "wasm_config_set_compiler"
        (ptr Types.Config.t @-> Types.Wasmer.Compiler.t @-> returning void)

    let set_features =
      foreign
        "wasm_config_set_features"
        (ptr Types.Config.t @-> ptr Types.Wasmer.Features.t @-> returning void)

    let delete =
      foreign "wasm_config_delete" (ptr Types.Config.t @-> returning void)
  end

  (** Functions with the [wasm_engine_] prefix *)
  module Engine = struct
    let new_with_config =
      foreign
        "wasm_engine_new_with_config"
        (ptr Types.Config.t @-> returning (ptr Types.Engine.t))

    let delete =
      foreign "wasm_engine_delete" (ptr Types.Engine.t @-> returning void)
  end

  (** Functions with the [wasm_store_] prefix *)
  module Store = struct
    let new_ =
      foreign
        "wasm_store_new"
        (ptr Types.Engine.t @-> returning (ptr Types.Store.t))

    let delete =
      foreign "wasm_store_delete" (ptr Types.Store.t @-> returning void)
  end

  (** Functions with the [wasm_module_] prefix *)
  module Module = struct
    let new_ =
      foreign
        "wasm_module_new"
        (ptr Types.Store.t @-> ptr Types.Byte_vec.t
        @-> returning (ptr Types.Module.t))

    let delete =
      foreign "wasm_module_delete" (ptr Types.Module.t @-> returning void)

    let imports =
      foreign
        "wasm_module_imports"
        (ptr Types.Module.t @-> ptr Types.Importtype.Vec.t @-> returning void)

    let exports =
      foreign
        "wasm_module_exports"
        (ptr Types.Module.t @-> ptr Types.Exporttype.Vec.t @-> returning void)
  end

  (** Functions with the [wasm_byte_vec_] prefix *)
  module Byte_vec = struct
    (* NOTE: This module does not use [Declare_vec] to allow us to use the
       better [Ctypes.string] type which takes care of string marshalling. *)

    let new_ =
      foreign
        "wasm_byte_vec_new"
        (ptr Types.Byte_vec.t @-> Ctypes.size_t @-> Ctypes.string
       @-> returning void)

    let new_empty =
      foreign "wasm_byte_vec_new_empty" (ptr Types.Byte_vec.t @-> returning void)

    let delete =
      foreign "wasm_byte_vec_delete" (ptr Types.Byte_vec.t @-> returning void)
  end

  (** Functions with the [wasm_val_vec_] prefix *)
  module Val_vec = Declare_vec (Types.Val_vec)

  (** Functions with the [wasm_valtype_] prefix *)
  module Valtype = struct
    let new_ =
      foreign
        "wasm_valtype_new"
        (Types.Valkind.t @-> returning (ptr Types.Valtype.t))

    let kind =
      foreign
        "wasm_valtype_kind"
        (ptr Types.Valtype.t @-> returning Types.Valkind.t)
  end

  (** Functions with the [wasm_valtype_vec_] prefix *)
  module Valtype_vec = Declare_vec (Types.Valtype.Vec)

  (** Functions with the [wasm_extern_] prefix *)
  module Extern = struct
    let as_func =
      foreign
        "wasm_extern_as_func"
        (ptr Types.Extern.t @-> returning (ptr Types.Func.t))

    let as_memory =
      foreign
        "wasm_extern_as_memory"
        (ptr Types.Extern.t @-> returning (ptr Types.Memory.t))

    let delete =
      foreign "wasm_extern_delete" (ptr Types.Extern.t @-> returning void)
  end

  (** Functions with the [wasm_extern_vec_] prefix *)
  module Extern_vec = Declare_vec (Types.Extern.Vec)

  (** Functions with the [wasm_functype_] prefix *)
  module Functype = struct
    let new_ =
      foreign
        "wasm_functype_new"
        (ptr Types.Valtype.Vec.t @-> ptr Types.Valtype.Vec.t
        @-> returning (ptr Types.Functype.t))

    let params =
      foreign
        "wasm_functype_params"
        (ptr Types.Functype.t @-> returning (ptr Types.Valtype.Vec.t))

    let results =
      foreign
        "wasm_functype_results"
        (ptr Types.Functype.t @-> returning (ptr Types.Valtype.Vec.t))
  end

  (** Functions with the [wasm_func_] prefix *)
  module Func = struct
    let new_ =
      foreign
        "wasm_func_new"
        (ptr Types.Store.t @-> ptr Types.Functype.t @-> Types.Func_callback.t
        @-> returning (ptr Types.Func.t))

    let as_extern =
      foreign
        "wasm_func_as_extern"
        (ptr Types.Func.t @-> returning (ptr Types.Extern.t))

    let call =
      foreign
        "wasm_func_call"
        (ptr Types.Func.t @-> ptr Types.Val_vec.t @-> ptr Types.Val_vec.t
        @-> returning (ptr Types.Trap.t))

    let param_arity =
      foreign "wasm_func_param_arity" (ptr Types.Func.t @-> returning size_t)

    let result_arity =
      foreign "wasm_func_result_arity" (ptr Types.Func.t @-> returning size_t)

    let type_ =
      foreign
        "wasm_func_type"
        (ptr Types.Func.t @-> returning (ptr Types.Functype.t))
  end

  (** Functions with the [wasm_memory_] prefix *)
  module Memory = struct
    let data =
      foreign "wasm_memory_data" (ptr Types.Memory.t @-> returning (ptr uint8_t))

    let data_size =
      foreign "wasm_memory_data_size" (ptr Types.Memory.t @-> returning size_t)

    let type_ =
      foreign
        "wasm_memory_type"
        (ptr Types.Memory.t @-> returning (ptr Types.Memorytype.t))
  end

  (** Functions with the [wasm_memory_type_] prefix *)
  module Memory_type = struct
    let limits =
      foreign
        "wasm_memorytype_limits"
        (ptr Types.Memorytype.t @-> returning (ptr Types.Limits.t))

    let delete =
      foreign
        "wasm_memorytype_delete"
        (ptr Types.Memorytype.t @-> returning void)
  end

  (** Functions with the [wasm_instance_] prefix *)
  module Instance = struct
    let new_ =
      foreign
        "wasm_instance_new"
        (ptr Types.Store.t @-> ptr Types.Module.t @-> ptr Types.Extern.Vec.t
        @-> ptr (ptr Types.Trap.t)
        @-> returning (ptr Types.Instance.t))

    let delete =
      foreign "wasm_instance_delete" (ptr Types.Instance.t @-> returning void)

    let exports =
      foreign
        "wasm_instance_exports"
        (ptr Types.Instance.t @-> ptr Types.Extern.Vec.t @-> returning void)
  end

  (** Functions with the [wasm_name_] prefix *)
  module Name = Byte_vec

  (** Functions with the [wasm_message_] prefix *)
  module Message = Name

  (** Functions with the [wasm_trap_] prefix *)
  module Trap = struct
    let new_ =
      foreign
        "wasm_trap_new"
        (ptr Types.Store.t @-> ptr Types.Message.t
        @-> returning (ptr Types.Trap.t))

    let message =
      foreign
        "wasm_trap_message"
        (ptr Types.Trap.t @-> ptr Types.Message.t @-> returning void)
  end

  (** Functions with the [wasm_externtype_] prefix *)
  module Externtype = struct
    let kind =
      foreign
        "wasm_externtype_kind"
        (ptr Types.Externtype.t @-> returning Types.Externkind.t)
  end

  (** Functions with the [wasm_importtype_] prefix *)
  module Importtype = struct
    let module_ =
      foreign
        "wasm_importtype_module"
        (ptr Types.Importtype.t @-> returning (ptr Types.Name.t))

    let name =
      foreign
        "wasm_importtype_name"
        (ptr Types.Importtype.t @-> returning (ptr Types.Name.t))

    let type_ =
      foreign
        "wasm_importtype_type"
        (ptr Types.Importtype.t @-> returning (ptr Types.Externtype.t))
  end

  (** Functions with the [wasm_importtype_vec_] prefix *)
  module Importtype_vec = Declare_vec (Types.Importtype.Vec)

  (** Functions with the [wasm_exporttype_] prefix *)
  module Exporttype = struct
    let name =
      foreign
        "wasm_exporttype_name"
        (ptr Types.Exporttype.t @-> returning (ptr Types.Name.t))

    let type_ =
      foreign
        "wasm_exporttype_type"
        (ptr Types.Exporttype.t @-> returning (ptr Types.Externtype.t))
  end

  (** Functions with the [wasm_exporttype_vec_] prefix *)
  module Exporttype_vec = Declare_vec (Types.Exporttype.Vec)

  let wat2wasm =
    foreign
      "wat2wasm"
      (ptr Types.Byte_vec.t @-> ptr Types.Byte_vec.t @-> returning void)
end
