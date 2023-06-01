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

(* For documentation please refer to the [Tezos_wasmer] module. *)

open Api
open Vectors

module Resolver = Map.Make (struct
  type t = string * Types.Externkind.t

  let compare (l1, l2) (r1, r2) =
    match (String.compare l1 r1, Unsigned.UInt8.compare l2 r2) with
    | 0, r -> r
    | r, _ -> r
end)

type t = {resolved : Types.Extern.t Ctypes.ptr Resolver.t; clean : unit -> unit}

let from_instance inst =
  let exports_vec = Module.exports inst.Instance.module_ in
  let exports = Export_type_vector.to_list exports_vec in
  let externs_vec = Extern_vector.empty () in
  Functions.Instance.exports inst.instance (Ctypes.addr externs_vec) ;
  let externs = Extern_vector.to_list externs_vec in
  let resolved =
    List.fold_right2
      (fun export extern tail ->
        let name = Export_type.name export in
        let kind = Export_type.type_ export |> Functions.Externtype.kind in
        Resolver.add (name, kind) extern tail)
      exports
      externs
      Resolver.empty
  in
  (* The exports vector is consumed in read-only or copying fashion, therefore
     there are no references to it at this point. We can clean it up. *)
  Functions.Exporttype_vec.delete (Ctypes.addr exports_vec) ;
  let clean () =
    (* This function shall be called when the user no longer needs to interact
       with the instance exports. Only then may we free the extern objects. *)
    Functions.Extern_vec.delete (Ctypes.addr externs_vec)
  in
  {resolved; clean}

let delete exports = exports.clean ()

exception Export_not_found of {name : string; kind : Unsigned.uint8}

let () =
  Printexc.register_printer (function
      | Export_not_found {name; kind} ->
          Some
            (Format.asprintf
               "Export %S (%i) not found"
               name
               (Unsigned.UInt8.to_int kind))
      | _ -> None)

let fn exports name typ =
  let kind = Types.Externkind.func in
  let extern = Resolver.find_opt (name, kind) exports.resolved in
  let extern =
    match extern with
    | None -> raise (Export_not_found {name; kind})
    | Some extern -> extern
  in
  let func = Functions.Extern.as_func extern in
  let f = Function.call func typ in
  () ;
  (* ^ This causes the current function to cap its arity. E.g. in case it gets
     aggressively inlined we make sure that the resulting extern function is
     entirely separate. *)
  f

let mem_of_extern extern =
  let mem = Functions.Extern.as_memory extern in
  let mem_type = Functions.Memory.type_ mem in
  let limits = Functions.Memory_type.limits mem_type in
  let min, max =
    let open Ctypes in
    (!@(limits |-> Types.Limits.min), !@(limits |-> Types.Limits.max))
  in
  let max =
    if Unsigned.UInt32.equal max Types.Limits.max_default then None
    else Some max
  in
  let raw =
    Ctypes.CArray.from_ptr
      (Functions.Memory.data mem)
      (Functions.Memory.data_size mem |> Unsigned.Size_t.to_int)
  in
  (* The memory type is no longer used after this, so we must delete it. *)
  Functions.Memory_type.delete mem_type ;
  Memory.{raw; min; max}

let mem exports name =
  let kind = Types.Externkind.memory in
  let extern = Resolver.find (name, kind) exports.resolved in
  mem_of_extern extern

let mem0 exports =
  let _, extern =
    Resolver.bindings exports.resolved
    |> List.find (fun ((_, kind), extern) -> kind = Types.Externkind.memory)
  in
  mem_of_extern extern
