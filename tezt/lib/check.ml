(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Base

type 'a typ =
  | E of {pp : Format.formatter -> 'a -> unit; equal : 'a -> 'a -> bool}
  | C of {pp : Format.formatter -> 'a -> unit; compare : 'a -> 'a -> int}

let pp_print_unit fmt () = Format.pp_print_string fmt "()"

let unit = C {pp = pp_print_unit; compare = Unit.compare}

let bool = C {pp = Format.pp_print_bool; compare = Bool.compare}

let pp_print_quoted_char fmt char = Format.fprintf fmt "%C" char

let char = C {pp = pp_print_quoted_char; compare = Char.compare}

let int = C {pp = Format.pp_print_int; compare = Int.compare}

let pp_print_int32 fmt i = Format.pp_print_string fmt (Int32.to_string i)

let int32 = C {pp = pp_print_int32; compare = Int32.compare}

let pp_print_int64 fmt i = Format.pp_print_string fmt (Int64.to_string i)

let int64 = C {pp = pp_print_int64; compare = Int64.compare}

let float = C {pp = Format.pp_print_float; compare = Float.compare}

let compare_floats_epsilon epsilon a b =
  if abs_float (a -. b) <= epsilon then 0 else Float.compare a b

let float_epsilon epsilon =
  C {pp = Format.pp_print_float; compare = compare_floats_epsilon epsilon}

let pp_print_quoted_string fmt string =
  Format.pp_print_char fmt '"' ;
  Format.pp_print_string fmt (String.escaped string) ;
  Format.pp_print_char fmt '"'

let string = C {pp = pp_print_quoted_string; compare = String.compare}

let pp_option pp_item fmt = function
  | None -> Format.pp_print_string fmt "None"
  | Some item ->
      Format.pp_print_string fmt "Some " ;
      pp_item fmt item

let option = function
  | E {pp = pp_item; equal = eq_items} ->
      E {pp = pp_option pp_item; equal = Option.equal eq_items}
  | C {pp = pp_item; compare = cmp_items} ->
      C {pp = pp_option pp_item; compare = Option.compare cmp_items}

let pp_list ?(left = "[") ?(right = "]") pp_item fmt list =
  Format.pp_print_string fmt left ;
  if list <> [] then (
    Format.pp_open_box fmt 1 ;
    Format.pp_print_char fmt ' ' ;
    let pp_sep fmt () =
      Format.pp_print_char fmt ';' ;
      Format.pp_print_space fmt ()
    in
    Format.pp_print_list ~pp_sep pp_item fmt list ;
    Format.pp_print_char fmt ' ' ;
    Format.pp_close_box fmt ()) ;
  Format.pp_print_string fmt right

(* Note: available as List.equal in OCaml 4.12. *)
let rec equal_lists eq_items a b =
  match (a, b) with
  | ([], []) -> true
  | ([], _ :: _) | (_ :: _, []) -> false
  | (hda :: tla, hdb :: tlb) -> eq_items hda hdb && equal_lists eq_items tla tlb

(* Note: available as List.compare in OCaml 4.12. *)
let rec compare_lists cmp_items a b =
  match (a, b) with
  | ([], []) -> 0
  | ([], _ :: _) -> -1
  | (_ :: _, []) -> 1
  | (hda :: tla, hdb :: tlb) ->
      let c = cmp_items hda hdb in
      if c = 0 then compare_lists cmp_items tla tlb else c

let list = function
  | E {pp = pp_item; equal = eq_items} ->
      E {pp = pp_list pp_item; equal = equal_lists eq_items}
  | C {pp = pp_item; compare = cmp_items} ->
      C {pp = pp_list pp_item; compare = compare_lists cmp_items}

let pp_array pp_item fmt array =
  pp_list ~left:"[|" ~right:"|]" pp_item fmt (Array.to_list array)

let equal_arrays eq_items a b =
  let len_a = Array.length a in
  let len_b = Array.length b in
  len_a = len_b
  &&
  let rec loop i =
    if i >= len_a then true
    else if eq_items a.(i) b.(i) then loop (i + 1)
    else false
  in
  loop 0

let compare_arrays cmp_items a b =
  let len_a = Array.length a in
  let len_b = Array.length b in
  let rec loop i =
    (* All items up to [i - 1] are equal. *)
    match (i >= len_a, i >= len_b) with
    | (true, true) ->
        (* Both arrays have the same size. *)
        0
    | (true, false) ->
        (* [a] is smaller than [b]. *)
        -1
    | (false, true) ->
        (* [a] is longer than [b]. *)
        1
    | (false, false) ->
        let c = cmp_items a.(i) b.(i) in
        if c = 0 then loop (i + 1) else c
  in
  loop 0

let array = function
  | E {pp = pp_item; equal = eq_items} ->
      E {pp = pp_array pp_item; equal = equal_arrays eq_items}
  | C {pp = pp_item; compare = cmp_items} ->
      C {pp = pp_array pp_item; compare = compare_arrays cmp_items}

type _ tuple_item = Item : 'b typ * ('a -> 'b) -> 'a tuple_item

let get_pp = function E {pp; _} | C {pp; _} -> pp

let get_equal = function
  | E {equal; _} -> equal
  | C {compare; _} -> fun a b -> compare a b = 0

let tuple (type a) (items : a tuple_item list) : a typ =
  let pp fmt value =
    Format.pp_open_box fmt 1 ;
    Format.pp_print_string fmt "(" ;
    List.iteri
      (fun i (Item (item_type, get_item)) ->
        if i > 0 then (
          Format.pp_print_char fmt ',' ;
          Format.pp_print_space fmt ()) ;
        get_pp item_type fmt (get_item value))
      items ;
    Format.pp_print_string fmt ")" ;
    Format.pp_close_box fmt ()
  in
  let comparable =
    List.for_all
      (function Item (E _, _) -> false | Item (C _, _) -> true)
      items
  in
  if comparable then
    let compare a b =
      let rec loop = function
        | [] -> 0
        | Item (item_type, get_item) :: tail -> (
            match item_type with
            | E _ -> assert false (* [comparable] was [true] *)
            | C {compare; _} ->
                let c = compare (get_item a) (get_item b) in
                if c = 0 then loop tail else c)
      in
      loop items
    in
    C {pp; compare}
  else
    let equal a b =
      let rec loop = function
        | [] -> true
        | Item (item_type, get_item) :: tail ->
            get_equal item_type (get_item a) (get_item b) && loop tail
      in
      loop items
    in
    E {pp; equal}

let tuple2 a b = tuple [Item (a, fst); Item (b, snd)]

let tuple3 a b c =
  tuple
    [
      Item (a, fun (x, _, _) -> x);
      Item (b, fun (_, x, _) -> x);
      Item (c, fun (_, _, x) -> x);
    ]

let tuple4 a b c d =
  tuple
    [
      Item (a, fun (x, _, _, _) -> x);
      Item (b, fun (_, x, _, _) -> x);
      Item (c, fun (_, _, x, _) -> x);
      Item (d, fun (_, _, _, x) -> x);
    ]

let tuple5 a b c d e =
  tuple
    [
      Item (a, fun (x, _, _, _, _) -> x);
      Item (b, fun (_, x, _, _, _) -> x);
      Item (c, fun (_, _, x, _, _) -> x);
      Item (d, fun (_, _, _, x, _) -> x);
      Item (e, fun (_, _, _, _, x) -> x);
    ]

let tuple8 a1 a2 a3 a4 a5 a6 a7 a8 =
  tuple
    [
      Item (a1, fun (x, _, _, _, _, _, _, _) -> x);
      Item (a2, fun (_, x, _, _, _, _, _, _) -> x);
      Item (a3, fun (_, _, x, _, _, _, _, _) -> x);
      Item (a4, fun (_, _, _, x, _, _, _, _) -> x);
      Item (a5, fun (_, _, _, _, x, _, _, _) -> x);
      Item (a6, fun (_, _, _, _, _, x, _, _) -> x);
      Item (a7, fun (_, _, _, _, _, _, x, _) -> x);
      Item (a8, fun (_, _, _, _, _, _, _, x) -> x);
    ]

let convert encode = function
  | E {pp; equal} ->
      let pp fmt x = pp fmt (encode x) in
      let equal a b = equal (encode a) (encode b) in
      E {pp; equal}
  | C {pp; compare} ->
      let pp fmt x = pp fmt (encode x) in
      let compare a b = compare (encode a) (encode b) in
      C {pp; compare}

let equalable pp equal = E {pp; equal}

let comparable pp compare = C {pp; compare}

module type EQUALABLE = sig
  type t

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

let equalable_module (type a) (m : (module EQUALABLE with type t = a)) =
  let module M = (val m) in
  equalable M.pp M.equal

module type COMPARABLE = sig
  type t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

let comparable_module (type a) (m : (module COMPARABLE with type t = a)) =
  let module M = (val m) in
  comparable M.pp M.compare

let fail ?__LOC__ error_msg pp_a a pp_b b =
  let error_msg =
    let substitution group =
      match Re.Group.get group 0 with
      | exception Not_found ->
          (* Should not happen: groups always contain at least group 0
             denoting the whole match. *)
          ""
      | "%L" -> Format.asprintf "%a" pp_a a
      | "%R" -> Format.asprintf "%a" pp_b b
      | s -> s
    in
    Re.replace (Re.compile (Re.Perl.re "%[LR]")) ~f:substitution error_msg
  in
  Test.fail ?__LOC__ "%s" error_msg

let eq a b ?__LOC__ typ ~error_msg =
  if not (get_equal typ a b) then
    let pp = get_pp typ in
    fail ?__LOC__ error_msg pp a pp b

let neq a b ?__LOC__ typ ~error_msg =
  if get_equal typ a b then
    let pp = get_pp typ in
    fail ?__LOC__ error_msg pp a pp b

let comparison function_name predicate a b ?__LOC__ typ ~error_msg =
  match typ with
  | E _ ->
      let pp = get_pp typ in
      let message =
        Format.asprintf
          "Check.%s used on non-comparable type to compare %a and %a with \
           ~error_msg = %s"
          function_name
          pp
          a
          pp
          b
          error_msg
      in
      Test.fail ?__LOC__ "%s" message
  | C {pp = _; compare} ->
      if not (predicate (compare a b)) then
        let pp = get_pp typ in
        fail ?__LOC__ error_msg pp a pp b

let lt x = comparison "(<)" (fun c -> c < 0) x

let le x = comparison "(<=)" (fun c -> c <= 0) x

let gt x = comparison "(>)" (fun c -> c > 0) x

let ge x = comparison "(>=)" (fun c -> c >= 0) x

let pp_rex fmt rex = Format.pp_print_string fmt (show_rex rex)

let like a b ~error_msg =
  if a =~! b then fail error_msg pp_print_quoted_string a pp_rex b

let not_like a b ~error_msg =
  if a =~ b then fail error_msg pp_print_quoted_string a pp_rex b

let list_mem typ ?__LOC__ a l ~error_msg =
  let eq = get_equal typ in
  if not @@ List.exists (eq a) l then
    let pp = get_pp typ in
    let pp_list = get_pp (list typ) in
    fail ?__LOC__ error_msg pp a pp_list l

let list_not_mem typ ?__LOC__ a l ~error_msg =
  let eq = get_equal typ in
  if List.exists (eq a) l then
    let pp = get_pp typ in
    let pp_list = get_pp (list typ) in
    fail ?__LOC__ error_msg pp a pp_list l

let pp_exn fmt exn = Format.pp_print_string fmt (Printexc.to_string exn)

let pp_exn_option fmt = function
  | None -> Format.pp_print_string fmt "no exception"
  | Some exn -> pp_exn fmt exn

let raises ?__LOC__ expected_exn f ~error_msg =
  match f () with
  | exception exn when exn = expected_exn -> ()
  | exception exn ->
      fail ?__LOC__ error_msg pp_exn expected_exn pp_exn_option (Some exn)
  | _ -> fail ?__LOC__ error_msg pp_exn expected_exn pp_exn_option None

(* We define infix operators at the end to avoid using them accidentally. *)

let ( = ) = eq

let ( <> ) = neq

let ( < ) = lt

let ( <= ) = le

let ( > ) = gt

let ( >= ) = ge

let ( =~ ) = like

let ( =~! ) = not_like
