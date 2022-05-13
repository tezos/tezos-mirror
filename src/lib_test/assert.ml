(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type ('a, 'b) t = ?loc:string -> ?msg:string -> 'a -> 'a -> 'b

type ('ty, 'res) assertion = ('ty, 'res) t

type 'a check2 = ('a, unit) t

type 'a check1 = ?loc:string -> ?msg:string -> 'a -> unit

type ('base_ty, 'container_ty) equality_check =
  ?eq:('base_ty -> 'base_ty -> bool) ->
  ?pp:(Format.formatter -> 'base_ty -> unit) ->
  'container_ty check2

type ('base_ty, 'container_ty) comparison_check =
  ?cmp:('base_ty -> 'base_ty -> int) ->
  ?pp:(Format.formatter -> 'base_ty -> unit) ->
  'container_ty check2

let assert_true str b = Alcotest.check Alcotest.bool str true b

let assert_false str b = Alcotest.check Alcotest.bool str false b

let impossible str = assert_true str false

let check_any ?(msg = "No value in the list satifies the condition.") f l =
  if not (List.exists f l) then Alcotest.fail msg

let contains (type a) (m : a Alcotest.testable) msg (x : a) (ls : a list) : unit
    =
  let (module M) = m in
  let (module L) = Alcotest.list m in
  if not @@ List.exists (M.equal x) ls then
    Alcotest.failf "%s. Could not find %a in %a" msg M.pp x L.pp ls

let fail_msg fmt = Format.kasprintf Stdlib.failwith fmt

let pp_msg_opt ppf = function
  | None -> ()
  | Some s -> Format.fprintf ppf "%s:@ " s

let pp_loc_opt ppf = function
  | None -> ()
  | Some s -> Format.fprintf ppf "%s@ " s

let fail ?loc ?msg pp expected given =
  fail_msg
    "@[%a%aexpected: %a@ got: %a@]"
    pp_loc_opt
    loc
    pp_msg_opt
    msg
    pp
    expected
    pp
    given

module Relation = struct
  type 'a tn = {name : string; rel : 'a -> 'a -> bool}

  let create name rel = {name; rel}

  let eq rel = create "=" rel

  let uneq rel = create "<>" rel

  let leq rel = create "<=" rel

  let lt rel = create "<" rel

  let geq rel = create ">=" rel

  let gt rel = create ">" rel
end

let is_infix_predicate_true ?loc ?msg {Relation.name; rel} pp x y =
  if not @@ rel x y then
    fail_msg
      "@[%a%apredicate %a %s %a does not hold@]"
      pp_loc_opt
      loc
      pp_msg_opt
      msg
      pp
      x
      name
      pp
      y

module Base = struct
  let default_pp ppf _ = Format.fprintf ppf ""

  let equal ?(eq = ( = )) ?(pp = default_pp) ?loc ?msg x y =
    is_infix_predicate_true ?loc ?msg (Relation.eq eq) pp x y

  let unequal ?(eq = ( = )) ?(pp = default_pp) ?loc ?msg x y =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.uneq (fun x y -> not @@ eq x y))
      pp
      x
      y

  let leq ?(cmp = Stdlib.compare) ?(pp = default_pp) ?loc ?msg x y =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.leq (fun x y -> cmp x y <= 0))
      pp
      x
      y

  let lt ?(cmp = Stdlib.compare) ?(pp = default_pp) ?loc ?msg x y =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.leq (fun x y -> cmp x y < 0))
      pp
      x
      y

  let geq ?(cmp = Stdlib.compare) ?(pp = default_pp) ?loc ?msg x y =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.leq (fun x y -> cmp x y >= 0))
      pp
      x
      y

  let gt ?(cmp = Stdlib.compare) ?(pp = default_pp) ?loc ?msg x y =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.leq (fun x y -> cmp x y > 0))
      pp
      x
      y

  let pp_list pp_element ppf l =
    Format.fprintf
      ppf
      "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
         pp_element)
      l

  let equal_list ?(eq = ( = )) ?(pp = default_pp) ?loc ?msg x y =
    let pp_list = pp_list pp in
    let rec iter i x y =
      match (x, y) with
      | hd_x :: tl_x, hd_y :: tl_y ->
          if eq hd_x hd_y then iter (succ i) tl_x tl_y
          else
            let msg =
              Format.asprintf "@[<h>%a(at index %d)@]" pp_msg_opt msg i
            in
            fail pp hd_x hd_y ~msg ?loc
      | _ :: _, [] | [], _ :: _ ->
          fail_msg
            "@[<v 2>@[<h>%a%a@](lists of different sizes: %d <> %d. The lists \
             are %a and %a@]"
            pp_loc_opt
            loc
            pp_msg_opt
            msg
            (List.length x)
            (List.length y)
            pp_list
            x
            pp_list
            y
      | [], [] -> ()
    in
    iter 0 x y

  let equal_list_list ?(eq = ( = )) ?(pp = default_pp) ?loc ?msg l1 l2 =
    equal_list ~eq:(List.equal eq) ~pp:(pp_list pp) ?msg ?loc l1 l2
end

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

module type EQUALABLE = sig
  type t

  val pp : Format.formatter -> t -> unit

  val eq : t -> t -> bool
end

let default_opt_pp pp ppf = function
  | None -> Format.fprintf ppf "None"
  | Some v -> pp ppf v

module type EQUALITIES = sig
  type t

  val equal : t check2

  val unequal : t check2

  val fail : (t, 'a) assertion

  module Option : sig
    val equal : t option check2
  end

  module List : sig
    val equal : t list check2
  end

  module Array : sig
    val equal : t array check2
  end

  module List_list : sig
    val equal : t list list check2
  end
end

module type COMPARISONS = sig
  include EQUALITIES

  val leq : t check2

  val lt : t check2

  val geq : t check2

  val gt : t check2
end

module Make_equalities (X : EQUALABLE) = struct
  type t = X.t

  module T = X

  let equal ?loc ?msg a b = Base.equal ~eq:X.eq ?loc ?msg ~pp:X.pp a b

  let unequal ?loc ?msg a b =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.uneq (fun a b -> not @@ X.eq a b))
      X.pp
      a
      b

  let fail ?loc ?msg a b = fail X.pp a b ?msg ?loc

  module Option = struct
    let equal ?loc ?msg aopt bopt =
      let eq = Option.equal X.eq in
      let pp = default_opt_pp X.pp in
      Base.equal ?loc ~eq ~pp ?msg aopt bopt
  end

  module List = struct
    let equal = Base.equal_list ~eq:X.eq ~pp:X.pp
  end

  module Array = struct
    let equal ?loc ?msg a b =
      if Array.length a <> Array.length b then
        fail_msg
          "%a%a: arrays of different length"
          pp_loc_opt
          loc
          pp_msg_opt
          msg
      else
        let idx = ref 0 in
        Array.iter2
          (fun e_a e_b ->
            let extra_msg = Printf.sprintf "at index %d" !idx in
            let msg =
              match msg with
              | None -> extra_msg
              | Some m -> Format.sprintf "@[<h>%s (%s)@]" m extra_msg
            in
            equal ?loc ~msg e_a e_b)
          a
          b
  end

  (* Dedicated builtin for list of lists since these structures happen quite )
     often in the codebase *)
  module List_list = struct
    let equal =
      Base.equal_list ~eq:(Stdlib.List.equal X.eq) ~pp:(Base.pp_list X.pp)
  end
end

module Make_comparisons (X : COMPARABLE) = struct
  include Make_equalities (struct
    include X

    let eq a b = X.compare a b = 0
  end)

  let leq ?loc ?msg a b =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.leq (fun a b -> X.compare a b <= 0))
      X.pp
      a
      b

  let lt ?loc ?msg a b =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.lt (fun a b -> X.compare a b < 0))
      X.pp
      a
      b

  let geq ?loc ?msg a b =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.geq (fun a b -> X.compare a b >= 0))
      X.pp
      a
      b

  let gt ?loc ?msg a b =
    is_infix_predicate_true
      ?loc
      ?msg
      (Relation.gt (fun a b -> X.compare a b > 0))
      X.pp
      a
      b
end

module Bytes = Make_comparisons (struct
  type t = bytes

  let pp ppf b = Format.pp_print_string ppf (Bytes.to_string b)

  let compare = Bytes.compare
end)

module Bool = Make_comparisons (struct
  include Bool

  let pp = Format.pp_print_bool
end)

let is_true ?loc ?msg b = Bool.equal ?loc ?msg true b

let is_false ?loc ?msg b = Bool.equal ?loc ?msg false b

module String = struct
  include Make_comparisons (struct
    type t = string

    let compare = String.compare

    (* We want the string to appear between quotes to explicitly show its nature *)
    let pp ppf s = Format.fprintf ppf "%S" s
  end)

  let is_empty ?loc ?msg s = equal ?loc ?msg "" s
end

module Int32 = struct
  include Make_comparisons (struct
    include Int32

    let pp ppf = Format.fprintf ppf "%ld"
  end)

  let is_zero ?loc ?msg a = equal ?loc ?msg 0l a
end

module Int64 = struct
  include Make_comparisons (struct
    include Int64

    let pp ppf = Format.fprintf ppf "%Ld"
  end)

  let is_zero ?loc ?msg a = equal ?loc ?msg 0L a
end

module Int = struct
  include Make_comparisons (struct
    type t = int

    let compare = Stdlib.compare

    let pp = Format.pp_print_int
  end)

  let is_zero ?loc ?msg a = equal ?loc ?msg 0 a
end

let is_none ?loc ?msg ?pp x =
  if x <> None then
    let pp =
      Format.pp_print_option
        ~none:(fun ppf () -> Format.pp_print_string ppf "None")
        (fun ppf v ->
          match pp with
          | None -> Format.pp_print_string ppf "Some _"
          | Some pp -> pp ppf v)
    in
    fail pp None x ?msg ?loc

include Base
