(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

(*

  A helper module to preprocess sys args before it can
  hand it down to the toplevel process.

  tztop expects the path to the sub-library under development
  as the first argv (non-optional)

*)
module Arg : sig
  type t

  val of_sys_argv : string array -> t option

  val get_working_directory : t -> string

  val to_toplevel_sys_argv : t -> string array
end = struct
  type t = string array

  (* First argument has to be a path to tezos library under development *)
  let of_sys_argv argv = if Array.length argv < 2 then None else Some Sys.argv

  let[@inline] get_working_directory arg = arg.(1)

  let to_toplevel_sys_argv argv =
    let new_argv = Array.sub argv 1 (Array.length argv - 1) in
    new_argv.(0) <- argv.(0) ;
    new_argv
end

let () =
  match Arg.of_sys_argv Sys.argv with
  | Some arg ->
      let directory = Arg.get_working_directory arg in
      let directory_does_exist =
        try Sys.is_directory directory with _ -> false
      in
      if not directory_does_exist then (
        Printf.printf
          "stdout:%s is either not a directory or doesn't exist\n"
          directory ;
        exit (-1)) ;
      let new_argv = Arg.to_toplevel_sys_argv arg in
      Toploop.override_sys_argv new_argv ;
      Tztop_common.patch_env_loading () ;
      Tztop_common.load_stdlib () ;
      Tztop_common.load_dune_libs directory ;
      Tztop.main ()
  | None ->
      Printf.printf
        {|
  Tztop expects path to sub-library

    $ dune exec -- tztop src/proto_alpha

|} ;
      exit (-1)
