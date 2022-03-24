(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* We let some exceptions propagate. They are caught when running tests and
   logged as errors. We want to print a human-readable version of those errors.
   Exceptions that should be caught, such as [Not_found] or [End_of_file],
   do not need a human-readable version. *)
let () =
  Printexc.register_printer @@ function
  | Unix.Unix_error (error, _, _) -> Some (Unix.error_message error)
  | Failure error -> Some error
  | Sys_error error -> Some error
  | _ -> None

let ( // ) = Filename.concat

let sf = Printf.sprintf

let ( let* ) = Lwt.bind

let ( and* ) = Lwt.both

let lwt_both_fail_early a b =
  let (main_promise, main_awakener) = Lwt.task () in
  let already_woke_up = ref false in
  Lwt.on_failure a (fun exn ->
      if not !already_woke_up then (
        already_woke_up := true ;
        Lwt.wakeup_exn main_awakener exn) ;
      Lwt.cancel b) ;
  Lwt.on_failure b (fun exn ->
      if not !already_woke_up then (
        already_woke_up := true ;
        Lwt.wakeup_exn main_awakener exn) ;
      Lwt.cancel a) ;
  let both = Lwt.both a b in
  Lwt.on_success both (fun x -> Lwt.wakeup main_awakener x) ;
  Lwt.on_cancel main_promise (fun () -> Lwt.cancel both) ;
  main_promise

let return = Lwt.return

let unit = Lwt.return_unit

let none = Lwt.return_none

let some = Lwt.return_some

let mandatory name = function
  | None -> failwith ("no value for " ^ name)
  | Some x -> x

type ('a, 'b) runnable = {value : 'a; run : 'a -> 'b Lwt.t}

let run {value; run} = run value

let ( let*! ) x f =
  let* res = run x in
  f res

let ( let*? ) x f = f x.value

let map_runnable f {value; run} =
  let run x =
    let* output = run x in
    return (f output)
  in
  {value; run}

let range a b =
  let rec range ?(acc = []) a b =
    if b < a then acc else range ~acc:(b :: acc) a (b - 1)
  in
  range a b

let rec list_find_map f = function
  | [] -> None
  | head :: tail -> (
      match f head with None -> list_find_map f tail | Some _ as x -> x)

type rex = string * Re.re

let rec take n l =
  if n < 0 then invalid_arg "Tezt.Base.take: argument cannot be negative"
  else if n = 0 then []
  else match l with [] -> [] | hd :: rest -> hd :: take (n - 1) rest

let rec drop n l =
  if n < 0 then invalid_arg "Tezt.Base.drop: argument cannot be negative"
  else if n = 0 then l
  else match l with [] -> [] | _ :: rest -> drop (n - 1) rest

let rex ?opts r = (r, Re.compile (Re.Perl.re ?opts r))

let show_rex = fst

let ( =~ ) s (_, r) = Re.execp r s

let ( =~! ) s (_, r) = not (Re.execp r s)

let get_group group index =
  match Re.Group.get group index with
  | exception Not_found ->
      invalid_arg
        "regular expression has not enough capture groups for its usage, did \
         you forget parentheses?"
  | value -> value

let ( =~* ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group -> Some (get_group group 1)

let ( =~** ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group -> Some (get_group group 1, get_group group 2)

let ( =~*** ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group -> Some (get_group group 1, get_group group 2, get_group group 3)

let ( =~**** ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group ->
      Some
        ( get_group group 1,
          get_group group 2,
          get_group group 3,
          get_group group 4 )

let matches s (_, r) = Re.all r s |> List.map (fun g -> get_group g 1)

let replace_string ?pos ?len ?all (_, r) ~by s =
  Re.replace_string ?pos ?len ?all r ~by s

let rec repeat n f =
  if n <= 0 then unit
  else
    let* () = f () in
    repeat (n - 1) f

let fold n init f =
  let rec aux k accu =
    if k >= n then return accu
    else
      let* accu = f k accu in
      aux (k + 1) accu
  in
  aux 0 init

let with_open_out file write_f =
  let chan = open_out file in
  try
    write_f chan ;
    close_out chan
  with x ->
    close_out chan ;
    raise x

let with_open_in file read_f =
  let chan = open_in file in
  try
    let value = read_f chan in
    close_in chan ;
    value
  with x ->
    close_in chan ;
    raise x

let write_file filename contents =
  with_open_out filename @@ fun ch -> output_string ch contents

let read_file filename =
  with_open_in filename @@ fun ch ->
  let buffer = Buffer.create 512 in
  let bytes = Bytes.create 512 in
  let rec loop () =
    let len = input ch bytes 0 512 in
    if len > 0 then (
      Buffer.add_subbytes buffer bytes 0 len ;
      loop ())
  in
  loop () ;
  Buffer.contents buffer

module String_map = Map.Make (String)

module String_set = struct
  include Set.Make (String)

  let pp fmt set =
    if is_empty set then Format.fprintf fmt "{}"
    else
      Format.fprintf
        fmt
        "@[<hov 2>{ %a }@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt -> Format.fprintf fmt "%S"))
        (elements set)
end
