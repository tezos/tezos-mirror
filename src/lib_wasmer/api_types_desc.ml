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

(** This functor is used by Ctypes to generate Wasmer C API type bindings. *)
module Types (S : Ctypes.TYPE) = struct
  open S

  module type Named = sig
    val name : string
  end

  module type Named_struct = sig
    include Named

    type s

    type t = s Ctypes.structure

    val t : t typ
  end

  (** This functor corresponds to the [WASM_DECLARE_OWN] macro in [wasm.h]. *)
  module Declare_own (Desc : Named) : Named_struct = struct
    include Desc

    type s

    type t = s Ctypes.structure

    let t : t typ =
      let name = "wasm_" ^ Desc.name ^ "_t" in
      typedef (structure name) name
  end

  module type Named_type = sig
    include Named

    type t

    val t : t typ
  end

  module type Vec = sig
    type s

    include Named_type with type t = s Ctypes.structure

    type item

    val item : item typ

    val size : (Unsigned.size_t, t) field

    val data : (item Ctypes.ptr, t) field
  end

  (** This functor corresponds to the [WASM_DECLARE_VEC] macro in [wasm.h]. *)
  module Declare_vec (Item : Named_type) : Vec with type item = Item.t = struct
    let name = Item.name ^ "_vec"

    type s

    type t = s Ctypes.structure

    let t : t typ =
      let name = "wasm_" ^ name ^ "_t" in
      typedef (structure name) name

    type item = Item.t

    let item = Item.t

    let size = field t "size" size_t

    let data = field t "data" (ptr Item.t)

    let () = seal t
  end

  (** Generate a pointer type from another type. *)
  module Ptr (Item : Named_type) : Named_type with type t = Item.t Ctypes.ptr =
  struct
    include Item

    type t = Item.t Ctypes.ptr

    let t = ptr Item.t
  end

  module Declare_type (Desc : Named) : sig
    include Named_struct

    module Vec : Vec with type item = t Ctypes.ptr
  end = struct
    module Self = Declare_own (Desc)
    module Vec = Declare_vec (Ptr (Self))
    include Self
  end

  module Wasmer = struct
    (** [wasmer_compiler_t] *)
    module Compiler = struct
      type s

      type t = CRANELIFT | LLVM | SINGLEPASS

      let t : t typ =
        enum
          "wasmer_compiler_t"
          [
            (CRANELIFT, constant "CRANELIFT" int64_t);
            (LLVM, constant "LLVM" int64_t);
            (SINGLEPASS, constant "SINGLEPASS" int64_t);
          ]
    end

    (** [wasmer_features_t] *)
    module Features = struct
      type s

      type t = s Ctypes.structure

      let t : t typ =
        let name = "wasmer_features_t" in
        typedef (structure name) name
    end
  end

  (** [wasm_config_t] *)
  module Config = Declare_own (struct
    let name = "config"
  end)

  (** [wasm_engine_t] *)
  module Engine = Declare_own (struct
    let name = "engine"
  end)

  (** [wasm_store_t] *)
  module Store = Declare_own (struct
    let name = "store"
  end)

  (** [wasm_module_t] *)
  module Module = Declare_own (struct
    let name = "module"
  end)

  (** [wasm_byte_t] *)
  module Byte = struct
    type t = Unsigned.uint8

    let t = uint8_t

    let name = "byte"
  end

  (** [wasm_byte_vec_t] *)
  module Byte_vec = Declare_vec (Byte)

  (** [wasm_name_t] *)
  module Name = Byte_vec

  (** [wasm_message_t] *)
  module Message = Name

  module Ref_repr = struct
    type s

    type t = s Ctypes.structure

    let t : t Ctypes.typ = Ctypes.structure "wasm_ref_t"
  end

  (** [wasm_ref_t] *)
  module Ref = struct
    let name = "ref"

    type t = Ref_repr.s Ctypes.structure

    let t : t typ = S.lift_typ Ref_repr.t
  end

  (** [wasm_valkind_t] *)
  module Valkind = struct
    type t = Unsigned.uint8

    let i32 = constant "WASM_I32" uint8_t

    let i64 = constant "WASM_I64" uint8_t

    let f32 = constant "WASM_F32" uint8_t

    let f64 = constant "WASM_F64" uint8_t

    let anyref = constant "WASM_ANYREF" uint8_t

    let funcref = constant "WASM_FUNCREF" uint8_t

    let t : t typ = uint8_t
  end

  (* The actual [Val.t] is an abstract representation of values.
     Unfortunately, it can't be properly represented using Ctypes' stubs
     functionality because it contains an anonymous union field.

     However, the default Ctypes functionality works fine here. The down side is
     that this is not sufficiently type checked, hence we must be careful with
     declarations below.

     Ultimately the [Val.t] is still the type to be used. The types described
     by this module are lifted within the [Val] module.
  *)
  module Val_repr = struct
    open Ctypes

    module Of = struct
      type s

      type t = s union

      let t : t typ = union ""

      let i32 = field t "i32" int32_t

      let i64 = field t "i64" int64_t

      let f32 = field t "f32" float

      let f64 = field t "f64" double

      let ref = field t "ref" (ptr Ref_repr.t)

      let () = seal t
    end

    type s

    type t = s structure

    let t : t typ = structure "wasm_val_t"

    let kind = field t "kind" uint8_t

    let of_ = field t "of" Of.t

    let () = seal t
  end

  (** [wasm_val_t] *)
  module Val = struct
    let name = "val"

    type t = Val_repr.s Ctypes.structure

    let t : t typ = S.lift_typ Val_repr.t
  end

  (** [wasm_val_vec_t] *)
  module Val_vec = Declare_vec (Val)

  (** [wasm_trap_t] *)
  module Trap = Declare_own (struct
    let name = "trap"
  end)

  (** [wasm_valtype_t] *)
  module Valtype = Declare_type (struct
    let name = "valtype"
  end)

  module Func_callback = struct
    let fn = ptr Val_vec.t @-> ptr Val_vec.t @-> returning (ptr Trap.t)

    let m =
      Foreign.dynamic_funptr ~runtime_lock:true ~thread_registration:true fn

    let t = static_funptr fn
  end

  (** [wasm_func_t] *)
  module Func = Declare_own (struct
    let name = "func"
  end)

  (** [wasm_memory_t] *)
  module Memory = Declare_own (struct
    let name = "memory"
  end)

  (** [wasm_extern_t] *)
  module Extern = Declare_type (struct
    let name = "extern"
  end)

  (** [wasm_instance_t] *)
  module Instance = Declare_own (struct
    let name = "instance"
  end)

  (** [wasm_functype_t] *)
  module Functype = Declare_own (struct
    let name = "functype"
  end)

  (** [wasm_globaltype_t] *)
  module Globaltype = Declare_own (struct
    let name = "globaltype"
  end)

  (** [wasm_tabletype_t] *)
  module Tabletype = Declare_own (struct
    let name = "tabletype"
  end)

  (** [wasm_memorytype_t] *)
  module Memorytype = Declare_own (struct
    let name = "memorytype"
  end)

  (** [wasm_limits_t] *)
  module Limits = struct
    open Ctypes

    type s

    type t = s structure

    let t : t typ = structure "wasm_limits_t"

    let min = field t "min" uint32_t

    let max = field t "max" uint32_t

    let () = seal t

    let max_default = constant "wasm_limits_max_default" S.uint32_t
  end

  (** [wasm_externkind_t] *)
  module Externkind = struct
    type t = Unsigned.uint8

    let func = constant "WASM_EXTERN_FUNC" uint8_t

    let global = constant "WASM_EXTERN_GLOBAL" uint8_t

    let table = constant "WASM_EXTERN_TABLE" uint8_t

    let memory = constant "WASM_EXTERN_MEMORY" uint8_t

    let t : t typ = uint8_t
  end

  (** [wasm_externtype_t] *)
  module Externtype = Declare_own (struct
    let name = "externtype"
  end)

  (** [wasm_exporttype_t] *)
  module Exporttype = Declare_type (struct
    let name = "exporttype"
  end)

  (** [wasm_importtype_t] *)
  module Importtype = Declare_type (struct
    let name = "importtype"
  end)
end
