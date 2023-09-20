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

module Config : sig
  (** Compiler backend *)
  type compiler = CRANELIFT | LLVM | SINGLEPASS

  (** [is_compiler_available compiler] checks if the given [compiler] is
      available in the linked Wasmer environment. *)
  val is_compiler_available : compiler -> bool

  (** Wasmer engine configuration *)
  type t = {compiler : compiler}

  (** Sensible default configuration for Wasmer *)
  val default : t
end

module Engine : sig
  (** WebAssembly engine *)
  type t

  (** [create config] instantiate a WebAssembly engine. *)
  val create : Config.t -> t
end

module Store : sig
  (** WebAssembly runtime store *)
  type t

  (** [create engine] instantiate a WebAssembly runtime store. *)
  val create : Engine.t -> t

  (** [delete store] destroys the handle to the WebAssembly store. *)
  val delete : t -> unit
end

module Ref : sig
  (** Generic reference *)
  type t
end

(** WebAssembly type *)
type _ typ

(** 32-bit integer *)
val i32 : int32 typ

(** 64-bit integer *)
val i64 : int64 typ

(** 32-bit floating point number *)
val f32 : float typ

(** 64-bit floating point number *)
val f64 : float typ

(** Generic reference *)
val anyref : Ref.t typ

(** Function reference *)
val funcref : Ref.t typ

(** Function type *)
type _ fn

(** [x @-> f] composes a function type such that [x] is in the contravariant
    position and [f] in the covariant position. *)
val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn

(** [returning1 ret] describes a function that receives no arguments and returns
    a single value of type [ret]. *)
val returning1 : 'a typ -> 'a Lwt.t fn

(** Return type *)
type 'a ret

(** [nothing] returns nothing. *)
val nothing : unit ret

(** [ret1 typ] constructs a return type for one value of type [typ]. *)
val ret1 : 'a typ -> 'a ret

(** [a @** b] composes two types as a tuple return type such that two values of
    respectively type [a] and [b] are returned. *)
val ( @** ) : 'a typ -> 'b typ -> ('a * 'b) ret

(** [x @* xs] composes things similarly to [@**] with the addition that the
    second parameter may already be a composite type. *)
val ( @* ) : 'a typ -> 'b ret -> ('a * 'b) ret

(** [returning ret] constructs a function type which receives no parameters from
    the WebAssembly side, but returns values as described by [ret]. *)
val returning : 'a ret -> 'a Lwt.t fn

(** [producer ret] works similar to [returning] but adds an extra unit argument
    so that the effects of the implementing function trigger at call time. *)
val producer : 'a ret -> (unit -> 'a Lwt.t) fn

(** Something that can be given to a WebAssembly module via an import *)
type extern

(** Construct an extern function. *)
val fn : 'a fn -> 'a -> extern

module Module : sig
  (** WebAssembly module *)
  type t

  (** Textual or binary representation of WebAssembly *)
  type format = Text | Binary

  (** [create store format code] parses a module in the given [format]. *)
  val create : Store.t -> format -> string -> t

  (** [delete module_] destroys a WebAssembly module. Make sure that [module_]
      is not used after [delete] is called on it. *)
  val delete : t -> unit
end

module Memory : sig
  exception Out_of_bounds

  module Array : module type of Ctypes.CArray

  (** WebAssembly memory *)
  type t = {
    raw : Unsigned.uint8 Array.t;  (** C array backing the memory *)
    min : Unsigned.uint32;  (** Minimum memory size in pages (64 KiB) *)
    max : Unsigned.uint32 option;  (** Maximum size in pages *)
  }

  (** [get mem addr] reads a byte at address [addr]. *)
  val get : t -> int -> Unsigned.uint8

  (** [get_string mem addr len] reads [len] bytes from address [addr] in
      mmemory [mem]. *)
  val get_string : t -> address:int -> length:int -> string

  (** [set mem addr value] sets a byte at address [addr] to [value]. *)
  val set : t -> int -> Unsigned.uint8 -> unit

  (** [set_string mem ~address ~data] writes a series of bytes represented by [data]
      at address [address] in the provided memory [mem]. *)
  val set_string : t -> address:int -> data:string -> unit

  (** [length mem] gives you the memory size in bytes. *)
  val length : t -> int

  module Internal_for_tests : sig
    (** [of_list content] creates a memory instance containing the bytes from [content] 
      The content is expected to be a multiple of pages size (64KB)
    *)
    val of_list : Unsigned.uint8 list -> t

    (** [to_list mem] returns a list containing each byte of [mem] *)
    val to_list : t -> Unsigned.uint8 list
  end
end

module Instance : sig
  (** WebAssembly module instance *)
  type t

  (** [create store module_ imports] instantiates a module and links the given
      imports against what the module needs. *)
  val create : Store.t -> Module.t -> (string * string * extern) list -> t Lwt.t

  (** [delete instance] destroys the module instance. Make sure that [instance]
      is not used after [delete] is called on it. *)
  val delete : t -> unit
end

module Exports : sig
  (** WebAssembly module instance exports *)
  type t

  (** [from_intance instance] extracts the exports from the given instance. *)
  val from_instance : Instance.t -> t

  (** [delete exports] cleans up the exports collection to free its associated
      objects. You must not use previously extracted export objects after
      calling call. *)
  val delete : t -> unit

  (** [fn exports name typ] looks for a function called [name] and type checks
      it against [typ]. *)
  val fn : t -> string -> 'a fn -> 'a

  (** [mem exports name] looks for a memory called [name]. *)
  val mem : t -> string -> Memory.t

  (** [mem0 exports] gives you the first memory instance it finds. The order of
      memory instances is not specified and may change in the future. This
      function should be avoided unless you know the module in question only
      exports one memory instance. *)
  val mem0 : t -> Memory.t
end
