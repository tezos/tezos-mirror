(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** [assert] contains Alcotest convenience assertions. *)

let fail ~msg expected given =
  Format.kasprintf
    Stdlib.failwith
    "@[%s@ expected: %s@ got: %s@]"
    msg
    expected
    given

let fail_msg fmt = Format.kasprintf (fun msg -> fail ~msg "" "") fmt

let assert_true msg b = Alcotest.check Alcotest.bool msg true b

let assert_false msg b = Alcotest.check Alcotest.bool msg false b

let assert_none ?(msg = "") x = if x <> None then fail ~msg "None" "Some _"

let impossible str = assert_true str false

let check_any ?(msg = "No value in the list satifies the condition.") f l =
  if not (List.exists f l) then Alcotest.fail msg

let contains (type a) (m : a Alcotest.testable) msg x ls =
  let (module M) = m in
  let (module L) = Alcotest.list m in
  if not @@ List.exists (M.equal x) ls then
    Alcotest.failf "%s. Could not find %a in %a" msg M.pp x L.pp ls

let default_printer _ = ""

let equal ?(eq = ( = )) ?(prn = default_printer) ?(msg = "") x y =
  if not (eq x y) then fail ~msg (prn x) (prn y)

let equal_bytes ?msg s1 s2 = equal ?msg ~prn:Bytes.to_string s1 s2

let equal_bytes_option ?msg o1 o2 =
  let prn = function None -> "None" | Some s -> Bytes.to_string s in
  equal ?msg ~prn o1 o2

let equal_bool ?msg b1 b2 = equal ?msg ~prn:string_of_bool b1 b2

let equal_string_option ?msg o1 o2 =
  let prn = function None -> "None" | Some s -> s in
  equal ?msg ~prn o1 o2

let make_equal_list eq prn ?(msg = "") (x : 'a list) (y : 'a list) =
  let pp_list ppf l =
    Format.fprintf
      ppf
      "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
         (fun ppf e -> Format.fprintf ppf "%s" (prn e)))
      l
  in

  let rec iter i x y =
    match (x, y) with
    | (hd_x :: tl_x, hd_y :: tl_y) ->
        if eq hd_x hd_y then iter (succ i) tl_x tl_y
        else
          let msg = Printf.sprintf "%s (at index %d)" msg i in
          fail ~msg (prn hd_x) (prn hd_y)
    | (_ :: _, []) | ([], _ :: _) ->
        fail_msg
          "@[<v 2>%s. Lists have different sizes: %d <> %d. The lists are@,\
           @[<hov>%a@]@,\
           and@,\
           @[<hov>%a@]@]"
          msg
          (List.length x)
          (List.length y)
          pp_list
          x
          pp_list
          y
    | ([], []) -> ()
  in
  iter 0 x y

let equal_string_list = make_equal_list String.equal Fun.id

let equal_string_list_list ?msg l1 l2 =
  let pr_persist l =
    let res = String.concat ";" (List.map (fun s -> Printf.sprintf "%S" s) l) in
    Printf.sprintf "[%s]" res
  in
  equal_string_list ?msg (List.map pr_persist l1) (List.map pr_persist l2)
