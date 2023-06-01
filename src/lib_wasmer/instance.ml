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
open Utils

module Resolver = Map.Make (struct
  type t = string * string * Types.Externkind.t

  let compare (l1, l2, l3) (r1, r2, r3) =
    match
      (String.compare l1 r1, String.compare l2 r2, Unsigned.UInt8.compare l3 r3)
    with
    | 0, 0, r -> r
    | 0, r, _ -> r
    | r, _, _ -> r
end)

type t = {
  module_ : Module.t;
  instance : Types.Instance.t Ctypes.ptr;
  clean : unit -> unit;
}

exception
  Unsatisfied_import of {
    module_ : string;
    name : string;
    kind : Types.Externkind.t;
  }

let resolve_imports store modul resolver =
  let lookup import =
    let module_ = Import_type.module_ import in
    let name = Import_type.name import in
    let kind = Import_type.type_ import |> Functions.Externtype.kind in
    let match_ = Resolver.find_opt (module_, name, kind) resolver in
    match match_ with
    | None -> raise (Unsatisfied_import {module_; name; kind})
    | Some m -> Extern.to_extern store m
  in
  let imports_vec = Module.imports modul in
  let imports = Import_type_vector.to_array imports_vec |> Array.map lookup in
  let externs = Extern_vector.from_array (Array.map fst imports) in
  let clean () =
    Array.fold_left
      (fun clean_all (_, clean) ->
        () ;
        fun x ->
          clean_all x ;
          clean x)
      Fun.id
      imports
      ()
  in
  (* There are no references to the imports vector after this, so we can
     delete it. *)
  Functions.Importtype_vec.delete (Ctypes.addr imports_vec) ;
  let clean_after_instantiation () =
    (* Once the [externs] have been used during instantiation, we can get rid
       of the handles on our side because the underlying extern objects are
       kept alive on the Wasmer side. *)
    Functions.Extern_vec.delete (Ctypes.addr externs)
  in
  (externs, clean_after_instantiation, clean)

let create store module_ externs =
  let open Lwt.Syntax in
  let externs_vec, clean_after_instantiation, clean =
    externs
    |> List.map (fun (module_, name, extern) ->
           ((module_, name, Extern.to_externkind extern), extern))
    |> List.to_seq |> Resolver.of_seq
    |> resolve_imports store module_
  in

  let trap = Ctypes.allocate_n (Ctypes.ptr Types.Trap.t) ~count:1 in
  Ctypes.(trap <-@ Trap.none) ;

  let instantiate () =
    Lwt_preemptive.detach
      (fun (store, module_, externs_vec, trap) ->
        Functions.Instance.new_ store module_ (Ctypes.addr externs_vec) trap)
      (store, module_, externs_vec, trap)
  in

  let+ instance =
    Lwt.finalize instantiate (fun () ->
        (* At this point we can clean up some objects because the instantiation has
           acquired its own handles to relevant objects. *)
        clean_after_instantiation () ;
        Lwt.return_unit)
  in

  let trap = Ctypes.(!@trap) in
  Trap.check trap ;

  check_null_ptr Error.(make_exception Instantiate_module) instance ;

  {module_; instance; clean}

let delete inst =
  Functions.Instance.delete inst.instance ;
  inst.clean ()
