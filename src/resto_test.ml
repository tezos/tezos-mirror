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

let minus_service custom_root =
  service
    ~input:Json_encoding.null
    ~output:Json_encoding.float
    Path.(custom_root /: Arg.int / "minus")

let describe_service =
  Description.service Path.(root / "describe")

let dummy_service =
  service
    ~input:Json_encoding.null
    ~output:Json_encoding.null
    Path.(root / "a" / "path" / "long" / "enough" /
          "for" / "<hov>" / "to" / "trigger"
          /: Arg.float /: Arg.float /: Arg.float /: Arg.float
          /: Arg.float /: Arg.float /: Arg.float)

let prefix_dir1 = Path.(root / "tartine" /: Arg.float / "chaussure")
let prefix_dir2 = Path.(root / "epice" /: Arg.int)


(** Client only *)

let real_minus_service1 = minus_service prefix_dir1
let real_minus_service2 = minus_service prefix_dir2


(** Server only *)

module Directory = RestoDirectory
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
  Directory.register dir dummy_service
    (fun ((((((((),a), b), c), d), e), f), g) () -> return ())

let prefixed_dir = Directory.empty
let prefixed_dir =
  Directory.register2 prefixed_dir (minus_service Path.root)
    (fun i j () -> return (i -. float_of_int j))

let dir =
  Directory.register_dynamic_directory1 dir prefix_dir1
    (fun _ -> Lwt.return prefixed_dir)
let dir =
  Directory.register_dynamic_directory1 dir
    prefix_dir2
    (fun _ ->
       Lwt.return
         (Directory.map
            (fun ((), x) -> ((), float_of_int x))
            prefixed_dir))

let dir =
  Directory.register_describe_directory_service
    dir describe_service


(** Testing faked client/server communication. *)


let request service args arg =
  let args, arg = forge_request service args arg in
  match Lwt.state (Directory.lookup dir () args) with
  | Lwt.Return handler -> begin
    match Lwt.state (handler arg) with
      | Lwt.Return { code = 200 ; body = Some x } -> begin
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
  let test service args arg expected = request service args arg = expected in
  assert (test repeat_service ((), 3) (`A []) (`A (repeat 3 (`A [])))) ;
  assert (test add_service ((), 2) 3 5) ;
  assert (test alternate_add_service (((), 1), 2.5) () 3.5) ;
  assert (test real_minus_service1 (((), 2.5), 1) () 1.5) ;
  assert (test real_minus_service2 (((), 2), 1) () 1.) ;
  assert (test alternate_add_service' (1, 2) () 3) ;
  ()

let () =
  Printf.printf "\n### OK Resto ###\n\n%!"
