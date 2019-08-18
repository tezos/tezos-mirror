(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open EzResto

(** Shared part *)

let repeat_service =
  service
    ~input:Json_encoding.any_ezjson_value
    ~output:Json_encoding.any_ezjson_value
    Path.(root / "foo" /: Arg.int / "repeat")

let add_service =
  service
    ~input:Json_encoding.int
    ~output:Json_encoding.int
    Path.(root / "foo" /: Arg.int / "add")

let alternate_add_service =
  service
    ~input:Json_encoding.null
    ~output:Json_encoding.float
    Path.(root / "bar" /: Arg.int /: Arg.float / "add")

let alternate_add_service' =
  service
    ~input:Json_encoding.null
    ~output:Json_encoding.int
    Path.(map
       (fun (((),i),f) -> (i,int_of_float f))
       (fun (i,f) -> (((),i),float_of_int f))
       (root / "bar" /: Arg.int /: Arg.float / "add'"))

let minus_service root =
  service
    ~input:Json_encoding.null
    ~output:Json_encoding.float
    Path.(root /: Arg.int / "minus")

let describe_service =
  Description.service Path.(root / "describe")

(** Server only *)

module Directory = EzRestoDirectory
open Directory.Answer

let rec repeat i json =
  if i <= 0 then []
  else json :: repeat (i-1) json

let dir = Directory.empty
let dir =
  Directory.register1 dir repeat_service
    (fun i json -> return (`A (repeat i json)))
let dir =
  Directory.register1 dir add_service
    (fun i j -> return (i+j))
let dir =
  Directory.register2 dir alternate_add_service
    (fun i j () -> return (float_of_int i+.j))
let dir =
  Directory.register dir alternate_add_service'
    (fun (i,j) () -> return (i+j))
let dir =
  Directory.register_describe_directory_service
    dir describe_service


(** Testing faked client/server communication. *)

let request service args arg =
  let args, arg = forge_request service args arg in
  match Lwt.state (Directory.lookup dir args) with
  | Lwt.Return handler -> begin
    match Lwt.state (handler arg) with
      | Lwt.Return { code = 200 ; body = Single x } -> begin
          match read_answer service x with
          | Ok x -> x
          | Error msg -> failwith ("Parse error: " ^ msg)
        end
      | _ -> failwith "Unexpected lwt result"
    end
  | _ -> failwith "Unexpected lwt result"

let () =
  let dir = request describe_service () (Some true) in
  Format.printf "@[<v>%a@]@." Description.pp_print_directory_descr dir

let () =
  let test service args arg expected =
    request service args arg = expected in
  assert (test repeat_service ((), 3) (`A []) (`A (repeat 3 (`A [])))) ;
  assert (test add_service ((), 2) 3 5) ;
  assert (test alternate_add_service (((), 1), 2.5) () 3.5) ;
  assert (test alternate_add_service' (1, 2) () 3) ;
  ()

let () =
  Printf.printf "\n### OK EzResto ###\n\n%!"
