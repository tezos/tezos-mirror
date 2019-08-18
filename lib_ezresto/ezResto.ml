(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Resto

type json = Json_repr.Ezjsonm.value
module Arg = Arg
module Path = struct
  type 'params path = (unit, 'params) Path.path
  let root = Path.root
  let add_suffix = Path.add_suffix
  let add_arg = Path.add_arg
  let (/) = add_suffix
  let (/:) = add_arg
  let map = Path.map
end
type ('params, 'input, 'output) service =
  (unit, 'params, 'input, 'output) Resto.service
let service = service
let forge_request = forge_request
let read_answer = read_answer
module Description = Description
module Make = Make
