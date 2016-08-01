(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Resto_impl
open RestoDirectory_impl

module Answer = RestoDirectory_impl.Answer

open RestoDirectory_impl.Answer

exception Cannot_parse = RestoDirectory_impl.Cannot_parse
type step = RestoDirectory_impl.step =
  | Static of string
  | Dynamic of Arg.descr

type conflict = RestoDirectory_impl.conflict =
  | CService | CDir | CBuilder | CCustom
  | CTypes of Arg.descr * Arg.descr
  | CType of Arg.descr * string list

exception Conflict = RestoDirectory_impl.Conflict

module Make(Repr : Json_repr.Repr) = struct

  module Impl = RestoDirectory_impl.Make(Repr)
  open Impl

  type directory = unit Impl.directory
  let empty = empty
  let prefix path dir = (prefix path (map (fun _ -> ()) dir))
  let merge = merge

  let lookup tree = lookup tree ()

  let register d s h = register d s h
  let register0 d s h = register0 d s h
  let register1 d s h = register1 d s h
  let register2 d s h = register2 d s h
  let register3 d s h = register3 d s h
  let register4 d s h = register4 d s h
  let register5 d s h = register5 d s h

  let register_dynamic_directory ?descr dir path builder =
    register_dynamic_directory ?descr dir path
      (fun p -> builder p >>= fun dir -> Lwt.return (map (fun _ -> ()) dir))

  let register_dynamic_directory1 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S Z) f)
  let register_dynamic_directory2 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S (S Z)) f)
  let register_dynamic_directory3 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S (S (S Z))) f)

  type custom_lookup = Impl.custom_lookup =
    | CustomService of Description.service_descr *
                       (Repr.value -> Repr.value answer Lwt.t)
    | CustomDirectory of Description.directory_descr

  let register_custom_lookup = register_custom_lookup
  let register_custom_lookup1 = register_custom_lookup1
  let register_custom_lookup2 = register_custom_lookup2
  let register_custom_lookup3 = register_custom_lookup3

  let register_describe_directory_service =
    register_describe_directory_service

end

include Make(Json_repr.Ezjsonm)
