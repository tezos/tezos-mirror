(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Traits
open Test_fuzzing_helpers
open Support.Lib.Monad

(* In the following, in order to reduce the time, output and complexity and
   testing, we only test for the most general case (i.e., when testing an
   error-aware traversor, we do not make distinct tests for
   always-successful stepper, always-failing stepper, and sometimes-successful
   stepper).

   This offers as much coverage (because the generic steppers may
   be generated to be always-successful or always-failing or
   sometimes-successful) and thus as much assurance as to the correction of the
   traversors.

   It does mean that, should a test fail, it would be more difficult to
   pin-point the origin of the failure. If that were to happen, we invite the
   person debugging the code to write additional specialised tests. *)

module TestIterFold (M : sig
  include Traits.BASE with type 'a elt := int

  include Traits.ITER_SEQUENTIAL with type 'a elt := int and type 'a t := int t

  include FOLDLEFT_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) =
struct
  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{iter,fold_left}" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        let input = M.of_list input in
        eq
          (let acc = ref init in
           M.iter (IterOf.fn acc fn) input ;
           !acc)
          (M.fold_left (FoldOf.fn fn) init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{iter,fold_left}_e" M.name)
      [Fn.arith_e; one; many]
      (fun fn init input ->
        let input = M.of_list input in
        eq_e
          (let acc = ref init in
           M.iter_e (IterEOf.fn_e acc fn) input >|? fun () -> !acc)
          (M.fold_left_e (FoldEOf.fn_e fn) init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{iter,fold_left}_s" M.name)
      [Fn.arith_s; one; many]
      (fun fn init input ->
        let input = M.of_list input in
        eq_s
          (let acc = ref init in
           M.iter_s (IterSOf.fn_s acc fn) input >|= fun () -> !acc)
          (M.fold_left_s (FoldSOf.fn_s fn) init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{iter,fold_left}_es" M.name)
      [Fn.arith_es; one; many]
      (fun fn init input ->
        let input = M.of_list input in
        eq_es
          (let acc = ref init in
           M.iter_es (IterESOf.fn_es acc fn) input >|=? fun () -> !acc)
          (M.fold_left_es (FoldESOf.fn_es fn) init input))
end

module TestRevMapRevMap (M : sig
  include BASE

  include Traits.MAP_PARALLEL with type 'a t := 'a t

  include Traits.REVMAP_PARALLEL with type 'a t := 'a t
end) =
struct
  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{rev map,rev_map}" M.name)
      [Fn.arith; one; many]
      (fun fn const input ->
        let input = M.of_list input in
        let fn = MapOf.fn const fn in
        eq (M.map fn input |> M.rev) (M.rev_map fn input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{rev map,rev_map}_e" M.name)
      [Fn.arith_e; one; many]
      (fun fn const input ->
        let input = M.of_list input in
        let fn = MapEOf.fn_e const fn in
        eq_e (M.map_e fn input >|? M.rev) (M.rev_map_e fn input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{rev map,rev_map}_s" M.name)
      [Fn.arith_s; one; many]
      (fun fn const input ->
        let input = M.of_list input in
        let fn = MapSOf.fn_s const fn in
        eq_s (M.map_s fn input >|= M.rev) (M.rev_map_s fn input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{rev map,rev_map}_es" M.name)
      [Fn.arith_es; one; many]
      (fun fn const input ->
        let input = M.of_list input in
        let fn = MapESOf.fn_es const fn in
        eq_es (M.map_es fn input >|=? M.rev) (M.rev_map_es fn input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{rev map,rev_map}_p" M.name)
      [Fn.arith_s; one; many]
      (fun fn const input ->
        let input = M.of_list input in
        let fn = MapSOf.fn_s const fn in
        eq_s (M.map_p fn input >|= M.rev) (M.rev_map_p fn input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.{rev map,rev_map}_ep" M.name)
      [Fn.arith_es; one; many]
      (fun fn const input ->
        let input = M.of_list input in
        let fn_ep = MapEPOf.fn_ep const fn in
        eq_ep
          ~pp:M.pp
          (M.map_ep fn_ep input >|=? M.rev)
          (M.rev_map_ep fn_ep input))
end

module TestIterAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include Traits.ITER_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) =
struct
  let with_stdlib_iter fn init input =
    let acc = ref init in
    Stdlib.List.iter (IterOf.fn acc fn) input ;
    !acc

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter, Stdlib.List.iter" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq
          (let acc = ref init in
           M.iter (IterOf.fn acc fn) (M.of_list input) ;
           !acc)
          (with_stdlib_iter fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter_e, Stdlib.List.iter" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_e
          (let acc = ref init in
           M.iter_e (IterEOf.fn acc fn) (M.of_list input) >|? fun () -> !acc)
          (Ok (with_stdlib_iter fn init input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter_s, Stdlib.List.iter" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_s
          (let acc = ref init in
           M.iter_s (IterSOf.fn acc fn) (M.of_list input) >|= fun () -> !acc)
          (Lwt.return @@ with_stdlib_iter fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter_es, Stdlib.List.iter" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_es
          (let acc = ref init in
           M.iter_es (IterESOf.fn acc fn) (M.of_list input) >|=? fun () -> !acc)
          (Lwt.return_ok @@ with_stdlib_iter fn init input))
end

module TestIteriAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include
    Traits.ITERI_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) =
struct
  let with_stdlib_iteri fn init input =
    let acc = ref init in
    Stdlib.List.iteri (IteriOf.fn acc fn) input ;
    !acc

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iteri, Stdlib.List.iteri" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq
          (let acc = ref init in
           M.iteri (IteriOf.fn acc fn) (M.of_list input) ;
           !acc)
          (with_stdlib_iteri fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iteri_e, Stdlib.List.iteri" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_e
          (let acc = ref init in
           M.iteri_e (IteriEOf.fn acc fn) (M.of_list input) >|? fun () -> !acc)
          (Ok (with_stdlib_iteri fn init input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iteri_s, Stdlib.List.iteri" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_s
          (let acc = ref init in
           M.iteri_s (IteriSOf.fn acc fn) (M.of_list input) >|= fun () -> !acc)
          (Lwt.return @@ with_stdlib_iteri fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iteri_es, Stdlib.List.iteri" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_es
          (let acc = ref init in
           M.iteri_es (IteriESOf.fn acc fn) (M.of_list input)
           >|=? fun () -> !acc)
          (Lwt.return_ok @@ with_stdlib_iteri fn init input))
end

module TestIterMonotoneAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include Traits.ITER_PARALLEL with type 'a elt := int and type 'a t := int t
end) =
struct
  (* For collections without a specified ordering, or for out-of-order traversal
     we can only test iteration if the accumulator moves monotonically and the
     stepper doesn't depend on the accumulator. We do this here with a custom
     stepper. *)

  let with_stdlib_iter init fn const input =
    let acc = ref init in
    Stdlib.List.iter (fun elt -> acc := !acc + MapOf.fn const fn elt) input ;
    !acc

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter, Stdlib.List.iter" M.name)
      [one; Fn.arith; one; many]
      (fun init fn const input ->
        eq
          (let acc = ref init in
           M.iter
             (fun elt ->
               MapOf.fn const fn elt |> fun delta -> acc := !acc + delta)
             (M.of_list input)
           |> fun () -> !acc)
          (with_stdlib_iter init fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter_s, Stdlib.List.iter" M.name)
      [one; Fn.arith; one; many]
      (fun init fn const input ->
        eq_s
          (let acc = ref init in
           M.iter_s
             (fun elt ->
               MapSOf.fn const fn elt >|= fun delta -> acc := !acc + delta)
             (M.of_list input)
           >|= fun () -> !acc)
          (Lwt.return @@ with_stdlib_iter init fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter_es, Stdlib.List.iter" M.name)
      [one; Fn.arith; one; many]
      (fun init fn const input ->
        eq_es
          (let acc = ref init in
           M.iter_es
             (fun elt ->
               MapESOf.fn const fn elt >|=? fun delta -> acc := !acc + delta)
             (M.of_list input)
           >|=? fun () -> !acc)
          (Lwt.return_ok @@ with_stdlib_iter init fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter_p, Stdlib.List.iter" M.name)
      [one; Fn.arith; one; many]
      (fun init fn const input ->
        eq_s
          (let acc = ref init in
           M.iter_p
             (fun elt ->
               MapSOf.fn const fn elt >|= fun delta -> acc := !acc + delta)
             (M.of_list input)
           >|= fun () -> !acc)
          (Lwt.return @@ with_stdlib_iter init fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter_ep, Stdlib.List.iter" M.name)
      [one; Fn.arith; one; many]
      (fun init fn const input ->
        eq_es
          (let acc = ref init in
           M.iter_ep
             (fun elt ->
               MapESOf.fn const fn elt >|=? fun delta -> acc := !acc + delta)
             (M.of_list input)
           >|=? fun () -> !acc)
          (Lwt.return_ok @@ with_stdlib_iter init fn const input))
end

module TestMapAgainstStdlibList (M : sig
  include BASE

  include Traits.MAP_SEQUENTIAL with type 'a t := 'a t
end) =
struct
  let with_stdlib_map fn const input =
    Stdlib.List.map (MapOf.fn const fn) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map, Stdlib.List.map" M.name)
      [Fn.arith; one; many]
      (fun fn const input ->
        eq
          (M.to_list @@ M.map (MapOf.fn const fn) (M.of_list input))
          (with_stdlib_map fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map_e, Stdlib.List.map" M.name)
      [Fn.arith; one; many]
      (fun fn const input ->
        eq_e
          (M.map_e (MapEOf.fn const fn) (M.of_list input) >|? M.to_list)
          (Ok (with_stdlib_map fn const input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map_s, Stdlib.List.map" M.name)
      [Fn.arith; one; many]
      (fun fn const input ->
        eq_s
          (M.map_s (MapSOf.fn const fn) (M.of_list input) >|= M.to_list)
          (Lwt.return @@ with_stdlib_map fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map_es, Stdlib.List.map" M.name)
      [Fn.arith; one; many]
      (fun fn const input ->
        eq_es
          (M.map_es (MapESOf.fn const fn) (M.of_list input) >|=? M.to_list)
          (Lwt.return_ok @@ with_stdlib_map fn const input))
end

module TestMappAgainstStdlibList (M : sig
  include BASE

  include Traits.MAP_PARALLEL with type 'a t := 'a t
end) =
struct
  let with_stdlib_map fn const input =
    Stdlib.List.map (MapOf.fn const fn) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map_p, Stdlib.List.map" M.name)
      [Fn.arith; one; many]
      (fun fn const input ->
        eq_s
          (M.map_p (MapSOf.fn const fn) (M.of_list input) >|= M.to_list)
          (Lwt.return @@ with_stdlib_map fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map_ep, Stdlib.List.map" M.name)
      [Fn.arith; one; many]
      (fun fn const input ->
        eq_es
          (M.map_ep (MapESOf.fn const fn) (M.of_list input) >|=? M.to_list)
          (Lwt.return_ok @@ with_stdlib_map fn const input))
end

module TestFoldAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include FOLDLEFT_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) =
struct
  let with_stdlib_fold_left fn init input =
    Stdlib.List.fold_left (FoldOf.fn fn) init input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_left, Stdlib.List.fold_left" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq
          (M.fold_left (FoldOf.fn fn) init (M.of_list input))
          (with_stdlib_fold_left fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_left_e, Stdlib.List.fold_left" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_e
          (M.fold_left_e (FoldEOf.fn fn) init (M.of_list input))
          (Ok (with_stdlib_fold_left fn init input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_left_s, Stdlib.List.fold_left" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_s
          (M.fold_left_s (FoldSOf.fn fn) init (M.of_list input))
          (Lwt.return @@ with_stdlib_fold_left fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_left_es, Stdlib.List.fold_left" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_es
          (M.fold_left_es (FoldESOf.fn fn) init (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_fold_left fn init input))
end

module TestFoldMonotonicAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include FOLDOOO_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) =
struct
  let with_stdlib_fold_left const fn init input =
    Stdlib.List.fold_left (fun acc x -> acc + FoldOf.fn fn const x) init input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold, Stdlib.List.fold_left" M.name)
      [one; Fn.arith; one; many]
      (fun const fn init input ->
        eq
          (M.fold
             (fun x acc -> FoldOf.fn fn const x |> fun delta -> acc + delta)
             (M.of_list input)
             init)
          (with_stdlib_fold_left const fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_e, Stdlib.List.fold_left" M.name)
      [one; Fn.arith; one; many]
      (fun const fn init input ->
        eq_e
          (M.fold_e
             (fun x acc -> FoldEOf.fn fn const x >|? fun delta -> acc + delta)
             (M.of_list input)
             init)
          (Ok (with_stdlib_fold_left const fn init input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_s, Stdlib.List.fold_left" M.name)
      [one; Fn.arith; one; many]
      (fun const fn init input ->
        eq_s
          (M.fold_s
             (fun x acc -> FoldSOf.fn fn const x >|= fun delta -> acc + delta)
             (M.of_list input)
             init)
          (Lwt.return @@ with_stdlib_fold_left const fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_es, Stdlib.List.fold_left" M.name)
      [one; Fn.arith; one; many]
      (fun const fn init input ->
        eq_es
          (M.fold_es
             (fun x acc ->
               FoldESOf.fn fn const x >|=? fun delta -> acc + delta)
             (M.of_list input)
             init)
          (Lwt.return_ok @@ with_stdlib_fold_left const fn init input))
end

module TestFoldRightAgainstStdlibList (M : sig
  include BASE

  include Traits.FOLDRIGHT_SEQUENTIAL with type 'a t := 'a t
end) =
struct
  let with_stdlib_fold_right fn init input =
    Stdlib.List.fold_right (FoldOf.fn fn) input init

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_right, Stdlib.List.fold_right" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq
          (M.fold_right (FoldOf.fn fn) (M.of_list input) init)
          (with_stdlib_fold_right fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_right_e, Stdlib.List.fold_right" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_e
          (M.fold_right_e (FoldEOf.fn fn) (M.of_list input) init)
          (Ok (with_stdlib_fold_right fn init input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_right_s, Stdlib.List.fold_right" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_s
          (M.fold_right_s (FoldSOf.fn fn) (M.of_list input) init)
          (Lwt.return @@ with_stdlib_fold_right fn init input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_right_es, Stdlib.List.fold_right" M.name)
      [Fn.arith; one; many]
      (fun fn init input ->
        eq_es
          (M.fold_right_es (FoldESOf.fn fn) (M.of_list input) init)
          (Lwt.return_ok @@ with_stdlib_fold_right fn init input))
end

module TestExistForallAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include
    Traits.EXISTFORALL_PARALLEL with type 'a elt := int and type 'a t := int t
end) =
struct
  let with_stdlib_exists fn const input =
    Stdlib.List.exists (CondOf.fn fn const) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists, Stdlib.List.exists" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq
          (M.exists (CondOf.fn fn const) (M.of_list input))
          (with_stdlib_exists fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists_e, Stdlib.List.exists" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_e
          (M.exists_e (CondEOf.fn fn const) (M.of_list input))
          (Ok (with_stdlib_exists fn const input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists_s, Stdlib.List.exists" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_s
          (M.exists_s (CondSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_exists fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists_es, Stdlib.List.exists" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_es
          (M.exists_es (CondESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_exists fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists_s, Stdlib.List.exists" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_s
          (M.exists_p (CondSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_exists fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists_es, Stdlib.List.exists" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_es
          (M.exists_ep (CondESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_exists fn const input))

  let with_stdlib_for_all fn const input =
    Stdlib.List.for_all (CondOf.fn fn const) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all, Stdlib.List.for_all" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq
          (M.for_all (CondOf.fn fn const) (M.of_list input))
          (with_stdlib_for_all fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all_e, Stdlib.List.for_all" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_e
          (M.for_all_e (CondEOf.fn fn const) (M.of_list input))
          (Ok (with_stdlib_for_all fn const input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all_s, Stdlib.List.for_all" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_s
          (M.for_all_s (CondSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_for_all fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all_es, Stdlib.List.for_all" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_es
          (M.for_all_es (CondESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_for_all fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all_s, Stdlib.List.for_all" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_s
          (M.for_all_p (CondSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_for_all fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all_es, Stdlib.List.for_all" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_es
          (M.for_all_ep (CondESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_for_all fn const input))
end

module TestFilterAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTER_SEQUENTIAL with type 'a t := 'a t
end) =
struct
  let with_stdlib_filter fn const input =
    Stdlib.List.filter (CondOf.fn fn const) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter, Stdlib.List.filter" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq
          (M.filter (CondOf.fn fn const) (M.of_list input) |> M.to_list)
          (with_stdlib_filter fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_e, Stdlib.List.filter" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_e
          (M.filter_e (CondEOf.fn fn const) (M.of_list input) >|? M.to_list)
          (Ok (with_stdlib_filter fn const input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_s, Stdlib.List.filter" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_s
          (M.filter_s (CondSOf.fn fn const) (M.of_list input) >|= M.to_list)
          (Lwt.return @@ with_stdlib_filter fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_es, Stdlib.List.filter" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_es
          (M.filter_es (CondESOf.fn fn const) (M.of_list input) >|=? M.to_list)
          (Lwt.return_ok @@ with_stdlib_filter fn const input))
end

module TestFilterpAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTER_PARALLEL with type 'a t := 'a t
end) =
struct
  let with_stdlib_filter fn const input =
    Stdlib.List.filter (CondOf.fn fn const) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_p, Stdlib.List.filter" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_s
          (M.filter_p (CondSOf.fn fn const) (M.of_list input) >|= M.to_list)
          (Lwt.return @@ with_stdlib_filter fn const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_ep, Stdlib.List.filter" M.name)
      [Fn.pred; one; many]
      (fun fn const input ->
        eq_es
          (M.filter_ep (CondESOf.fn fn const) (M.of_list input) >|=? M.to_list)
          (Lwt.return_ok @@ with_stdlib_filter fn const input))
end

module TestFiltermapAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTERMAP_SEQUENTIAL with type 'a t := 'a t
end) =
struct
  let with_stdlib_filter_map pred arith const input =
    Stdlib.List.filter_map (FilterMapOf.fns pred arith const) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_map, Stdlib.List.filter_map" M.name)
      [Fn.pred; Fn.arith; one; many]
      (fun pred arith const input ->
        eq
          ( M.filter_map (FilterMapOf.fns pred arith const) (M.of_list input)
          |> M.to_list )
          (with_stdlib_filter_map pred arith const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_map_e, Stdlib.List.filter_map" M.name)
      [Fn.pred; Fn.arith; one; many]
      (fun pred arith const input ->
        eq_e
          ( M.filter_map_e (FilterMapEOf.fns pred arith const) (M.of_list input)
          >|? M.to_list )
          (Ok (with_stdlib_filter_map pred arith const input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_map_s, Stdlib.List.filter_map" M.name)
      [Fn.pred; Fn.arith; one; many]
      (fun pred arith const input ->
        eq_s
          ( M.filter_map_s (FilterMapSOf.fns pred arith const) (M.of_list input)
          >|= M.to_list )
          (Lwt.return @@ with_stdlib_filter_map pred arith const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_map_es, Stdlib.List.filter_map" M.name)
      [Fn.pred; Fn.arith; one; many]
      (fun pred arith const input ->
        eq_es
          ( M.filter_map_es
              (FilterMapESOf.fns pred arith const)
              (M.of_list input)
          >|=? M.to_list )
          (Lwt.return_ok @@ with_stdlib_filter_map pred arith const input))
end

module TestFiltermappAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTERMAP_PARALLEL with type 'a t := 'a t
end) =
struct
  let with_stdlib_filter_map pred arith const input =
    Stdlib.List.filter_map (FilterMapOf.fns pred arith const) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_map_p, Stdlib.List.filter_map" M.name)
      [Fn.pred; Fn.arith; one; many]
      (fun pred arith const input ->
        eq_s
          ( M.filter_map_p (FilterMapSOf.fns pred arith const) (M.of_list input)
          >|= M.to_list )
          (Lwt.return @@ with_stdlib_filter_map pred arith const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.filter_map_ep, Stdlib.List.filter_map" M.name)
      [Fn.pred; Fn.arith; one; many]
      (fun pred arith const input ->
        eq_es
          ( M.filter_map_ep
              (FilterMapESOf.fns pred arith const)
              (M.of_list input)
          >|=? M.to_list )
          (Lwt.return_ok @@ with_stdlib_filter_map pred arith const input))
end

module TestFindStdlibList (M : sig
  include BASE

  include Traits.FIND_SEQUENTIAL with type 'a t := 'a t
end) =
struct
  let with_stdlib_find pred const input =
    Stdlib.List.find_opt (CondOf.fn pred const) input

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.find, Stdlib.List.find_opt" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq
          (M.find (CondOf.fn pred const) (M.of_list input))
          (with_stdlib_find pred const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.find_e, Stdlib.List.find_opt" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq
          (M.find_e (CondEOf.fn pred const) (M.of_list input))
          (Ok (with_stdlib_find pred const input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.find_s, Stdlib.List.find_opt" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq_s
          (M.find_s (CondSOf.fn pred const) (M.of_list input))
          (Lwt.return @@ with_stdlib_find pred const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.find_es, Stdlib.List.find_opt" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq_s
          (M.find_es (CondESOf.fn pred const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_find pred const input))
end

module TestPartitionStdlibList (M : sig
  include BASE

  include Traits.PARTITION_PARALLEL with type 'a t := 'a t
end) =
struct
  let with_stdlib_partition pred const input =
    Stdlib.List.partition (CondOf.fn pred const) input

  let to_list_pair (a, b) = (M.to_list a, M.to_list b)

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.partition, Stdlib.List.partition" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq
          (M.partition (CondOf.fn pred const) (M.of_list input) |> to_list_pair)
          (with_stdlib_partition pred const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.partition_e, Stdlib.List.partition" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq
          ( M.partition_e (CondEOf.fn pred const) (M.of_list input)
          >|? to_list_pair )
          (Ok (with_stdlib_partition pred const input)))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.partition_s, Stdlib.List.partition" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq_s
          ( M.partition_s (CondSOf.fn pred const) (M.of_list input)
          >|= to_list_pair )
          (Lwt.return @@ with_stdlib_partition pred const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.partition_es, Stdlib.List.partition" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq_s
          ( M.partition_es (CondESOf.fn pred const) (M.of_list input)
          >|=? to_list_pair )
          (Lwt.return_ok @@ with_stdlib_partition pred const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.partition_p, Stdlib.List.partition" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq_s
          ( M.partition_p (CondSOf.fn pred const) (M.of_list input)
          >|= to_list_pair )
          (Lwt.return @@ with_stdlib_partition pred const input))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.partition_ep, Stdlib.List.partition" M.name)
      [Fn.pred; one; many]
      (fun pred const input ->
        eq_s
          ( M.partition_ep (CondESOf.fn pred const) (M.of_list input)
          >|=? to_list_pair )
          (Lwt.return_ok @@ with_stdlib_partition pred const input))
end

module TestDoubleTraversorsStdlibList (M : sig
  include BASE

  include Traits.COMBINE_VANILLA with type 'a t := 'a t

  include Traits.ITER_PARALLEL with type 'a elt := 'a and type 'a t := 'a t

  include Traits.MAP_PARALLEL with type 'a t := 'a t

  include Traits.REVMAP_PARALLEL with type 'a t := 'a t

  include
    Traits.FOLDLEFT_SEQUENTIAL with type 'a elt := 'a and type 'a t := 'a t

  include Traits.FOLDRIGHT_SEQUENTIAL with type 'a t := 'a t

  include
    Traits.EXISTFORALL_PARALLEL with type 'a elt := 'a and type 'a t := 'a t

  include Traits.ALLDOUBLE_SEQENTIAL with type 'a t := 'a t
end) =
struct
  let uncurry f (x, y) = f x y

  let uncurry_l f acc (x, y) = f acc x y

  let uncurry_r f (x, y) acc = f x y acc

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter{2,}" M.name)
      [Fn.arith; one; manymany]
      (fun fn init (left, right) ->
        eq_e
          (let acc = ref init in
           M.iter2
             ~when_different_lengths:101
             (Iter2Of.fn acc fn)
             (M.of_list left)
             (M.of_list right)
           >|? fun () -> !acc)
          (let acc = ref init in
           let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.iter (uncurry @@ Iter2Of.fn acc fn) leftright ;
           match leftovers with None -> Ok !acc | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter{2,}_e" M.name)
      [Fn.arith_e; one; manymany]
      (fun fn init (left, right) ->
        eq_e
          (let acc = ref init in
           M.iter2_e
             ~when_different_lengths:101
             (Iter2EOf.fn_e acc fn)
             (M.of_list left)
             (M.of_list right)
           >|? fun () -> !acc)
          (let acc = ref init in
           let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.iter_e (uncurry @@ Iter2EOf.fn_e acc fn) leftright
           >>? fun () ->
           match leftovers with None -> Ok !acc | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter{2,}_s" M.name)
      [Fn.arith_s; one; manymany]
      (fun fn init (left, right) ->
        eq_s
          (let acc = ref init in
           M.iter2_s
             ~when_different_lengths:101
             (Iter2SOf.fn_s acc fn)
             (M.of_list left)
             (M.of_list right)
           >|=? fun () -> !acc)
          (let acc = ref init in
           let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.iter_s (uncurry @@ Iter2SOf.fn_s acc fn) leftright
           >>= fun () ->
           match leftovers with
           | None ->
               Lwt.return_ok !acc
           | Some _ ->
               Lwt.return_error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.iter{2,}_es" M.name)
      [Fn.arith_e; one; manymany]
      (fun fn init (left, right) ->
        eq_es
          (let acc = ref init in
           M.iter2_es
             ~when_different_lengths:101
             (Iter2ESOf.fn_e acc fn)
             (M.of_list left)
             (M.of_list right)
           >|=? fun () -> !acc)
          (let acc = ref init in
           let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.iter_es (uncurry @@ Iter2ESOf.fn_e acc fn) leftright
           >>=? fun () ->
           match leftovers with
           | None ->
               Lwt.return_ok !acc
           | Some _ ->
               Lwt.return_error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map{2,}" M.name)
      [Fn.arith; manymany]
      (fun fn (left, right) ->
        eq_e
          (M.map2
             ~when_different_lengths:101
             (Map2Of.fn fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let t = M.map (uncurry @@ Map2Of.fn fn) leftright in
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map{2,}_e" M.name)
      [Fn.arith_e; manymany]
      (fun fn (left, right) ->
        eq_e
          (M.map2_e
             ~when_different_lengths:101
             (Map2EOf.fn_e fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.map_e (uncurry @@ Map2EOf.fn_e fn) leftright
           >>? fun t ->
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map{2,}_s" M.name)
      [Fn.arith; manymany]
      (fun fn (left, right) ->
        eq_s
          (M.map2_s
             ~when_different_lengths:101
             (Map2SOf.fn fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.map_s (uncurry @@ Map2SOf.fn fn) leftright
           >|= fun t ->
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.map{2,}_es" M.name)
      [Fn.arith_e; manymany]
      (fun fn (left, right) ->
        eq_es
          (M.map2_es
             ~when_different_lengths:101
             (Map2ESOf.fn_e fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.map_es (uncurry @@ Map2ESOf.fn_e fn) leftright
           >>=? fun t ->
           match leftovers with
           | None ->
               Lwt.return_ok t
           | Some _ ->
               Lwt.return_error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.rev_map{2,}" M.name)
      [Fn.arith; manymany]
      (fun fn (left, right) ->
        eq_e
          (M.rev_map2
             ~when_different_lengths:101
             (Map2Of.fn fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let t = M.rev_map (uncurry @@ Map2Of.fn fn) leftright in
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.rev_map{2,}_e" M.name)
      [Fn.arith_e; manymany]
      (fun fn (left, right) ->
        eq_e
          (M.rev_map2_e
             ~when_different_lengths:101
             (Map2EOf.fn_e fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.rev_map_e (uncurry @@ Map2EOf.fn_e fn) leftright
           >>? fun t ->
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.rev_map{2,}_s" M.name)
      [Fn.arith; manymany]
      (fun fn (left, right) ->
        eq_s
          (M.rev_map2_s
             ~when_different_lengths:101
             (Map2SOf.fn fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.rev_map_s (uncurry @@ Map2SOf.fn fn) leftright
           >|= fun t ->
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.rev_map{2,}_es" M.name)
      [Fn.arith_e; manymany]
      (fun fn (left, right) ->
        eq_es
          (M.rev_map2_es
             ~when_different_lengths:101
             (Map2ESOf.fn_e fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.rev_map_es (uncurry @@ Map2ESOf.fn_e fn) leftright
           >>=? fun t ->
           match leftovers with
           | None ->
               Lwt.return_ok t
           | Some _ ->
               Lwt.return_error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_left{2,}" M.name)
      [Fn.arith; one; manymany]
      (fun fn init (left, right) ->
        eq_e
          (M.fold_left2
             ~when_different_lengths:101
             (Fold2Of.fn fn)
             init
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let t = M.fold_left (uncurry_l @@ Fold2Of.fn fn) init leftright in
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_left{2,}_e" M.name)
      [Fn.arith_e; one; manymany]
      (fun fn init (left, right) ->
        eq_e
          (M.fold_left2_e
             ~when_different_lengths:101
             (Fold2EOf.fn_e fn)
             init
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.fold_left_e (uncurry_l @@ Fold2EOf.fn_e fn) init leftright
           >>? fun t ->
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_left{2,}_s" M.name)
      [Fn.arith; one; manymany]
      (fun fn init (left, right) ->
        eq_s
          (M.fold_left2_s
             ~when_different_lengths:101
             (Fold2SOf.fn fn)
             init
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.fold_left_s (uncurry_l @@ Fold2SOf.fn fn) init leftright
           >|= fun t ->
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_left{2,}_es" M.name)
      [Fn.arith_e; one; manymany]
      (fun fn init (left, right) ->
        eq_es
          (M.fold_left2_es
             ~when_different_lengths:101
             (Fold2ESOf.fn_e fn)
             init
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.fold_left_es (uncurry_l @@ Fold2ESOf.fn_e fn) init leftright
           >>=? fun t ->
           match leftovers with
           | None ->
               Lwt.return_ok t
           | Some _ ->
               Lwt.return_error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_right{2,}" M.name)
      [Fn.arith; one; manymany]
      (fun fn init (left, right) ->
        eq_e
          (M.fold_right2
             ~when_different_lengths:101
             (Fold2Of.fn fn)
             (M.of_list left)
             (M.of_list right)
             init)
          ( M.combine
              ~when_different_lengths:101
              (M.of_list left)
              (M.of_list right)
          >|? fun leftright ->
          M.fold_right (uncurry_r @@ Fold2Of.fn fn) leftright init ))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_right{2,}_e" M.name)
      [Fn.arith_e; one; manymany]
      (fun fn init (left, right) ->
        eq_e
          (M.fold_right2_e
             ~when_different_lengths:101
             (Fold2EOf.fn_e fn)
             (M.of_list left)
             (M.of_list right)
             init)
          ( M.combine
              ~when_different_lengths:101
              (M.of_list left)
              (M.of_list right)
          >>? fun leftright ->
          M.fold_right_e (uncurry_r @@ Fold2EOf.fn_e fn) leftright init ))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_right{2,}_s" M.name)
      [Fn.arith; one; manymany]
      (fun fn init (left, right) ->
        eq_s
          (M.fold_right2_s
             ~when_different_lengths:101
             (Fold2SOf.fn fn)
             (M.of_list left)
             (M.of_list right)
             init)
          ( match
              M.combine
                ~when_different_lengths:101
                (M.of_list left)
                (M.of_list right)
            with
          | Ok leftright ->
              M.fold_right_s (uncurry_r @@ Fold2SOf.fn fn) leftright init
              >>= Lwt.return_ok
          | Error _ as err ->
              Lwt.return err ))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.fold_right{2,}_es" M.name)
      [Fn.arith_es; one; manymany]
      (fun fn init (left, right) ->
        eq_es
          (M.fold_right2_es
             ~when_different_lengths:101
             (Fold2ESOf.fn_es fn)
             (M.of_list left)
             (M.of_list right)
             init)
          ( Lwt.return
            @@ M.combine
                 ~when_different_lengths:101
                 (M.of_list left)
                 (M.of_list right)
          >>=? fun leftright ->
          M.fold_right_es (uncurry_r @@ Fold2ESOf.fn_es fn) leftright init ))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all{2,}" M.name)
      [Fn.pred; manymany]
      (fun pred (left, right) ->
        eq_e
          ~pp:PP.(res bool int)
          (M.for_all2
             ~when_different_lengths:101
             (Cond2Of.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let t = M.for_all (uncurry @@ Cond2Of.fn pred) leftright in
           match (t, leftovers) with
           | (false, _) ->
               Ok false
           | (true, None) ->
               Ok true
           | (true, Some _) ->
               Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all{2,}_e" M.name)
      [Fn.pred; manymany]
      (fun pred (left, right) ->
        eq_e
          ~pp:PP.(res bool int)
          (M.for_all2_e
             ~when_different_lengths:101
             (Cond2EOf.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.for_all_e (uncurry @@ Cond2EOf.fn pred) leftright
           >>? fun t ->
           match (t, leftovers) with
           | (false, _) ->
               Ok false
           | (true, None) ->
               Ok true
           | (true, Some _) ->
               Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all{2,}_s" M.name)
      [Fn.pred; manymany]
      (fun pred (left, right) ->
        eq_s
          ~pp:PP.(res bool int)
          (M.for_all2_s
             ~when_different_lengths:101
             (Cond2SOf.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.for_all_s (uncurry @@ Cond2SOf.fn pred) leftright
           >|= fun t ->
           match (t, leftovers) with
           | (false, _) ->
               Ok false
           | (true, None) ->
               Ok true
           | (true, Some _) ->
               Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.for_all{2,}_es" M.name)
      [Fn.pred; manymany]
      (fun pred (left, right) ->
        eq_es
          (M.for_all2_es
             ~when_different_lengths:101
             (Cond2ESOf.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.for_all_es (uncurry @@ Cond2ESOf.fn pred) leftright
           >>=? fun t ->
           match (t, leftovers) with
           | (false, _) ->
               Lwt.return_ok false
           | (true, None) ->
               Lwt.return_ok true
           | (true, Some _) ->
               Lwt.return_error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists{2,}" M.name)
      [Fn.pred; manymany]
      (fun pred (left, right) ->
        eq_e
          ~pp:PP.(res bool int)
          (M.exists2
             ~when_different_lengths:101
             (Cond2Of.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let t = M.exists (uncurry @@ Cond2Of.fn pred) leftright in
           match (t, leftovers) with
           | (true, _) ->
               Ok true
           | (false, None) ->
               Ok false
           | (false, Some _) ->
               Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists{2,}_e" M.name)
      [Fn.pred; manymany]
      (fun pred (left, right) ->
        eq_e
          ~pp:PP.(res bool int)
          (M.exists2_e
             ~when_different_lengths:101
             (Cond2EOf.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.exists_e (uncurry @@ Cond2EOf.fn pred) leftright
           >>? fun t ->
           match (t, leftovers) with
           | (true, _) ->
               Ok true
           | (false, None) ->
               Ok false
           | (false, Some _) ->
               Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists{2,}_s" M.name)
      [Fn.pred; manymany]
      (fun pred (left, right) ->
        eq_s
          ~pp:PP.(res bool int)
          (M.exists2_s
             ~when_different_lengths:101
             (Cond2SOf.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.exists_s (uncurry @@ Cond2SOf.fn pred) leftright
           >|= fun t ->
           match (t, leftovers) with
           | (true, _) ->
               Ok true
           | (false, None) ->
               Ok false
           | (false, Some _) ->
               Error 101))

  let () =
    Crowbar.add_test
      ~name:(Format.asprintf "%s.exists{2,}_es" M.name)
      [Fn.pred; manymany]
      (fun pred (left, right) ->
        eq_es
          (M.exists2_es
             ~when_different_lengths:101
             (Cond2ESOf.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.exists_es (uncurry @@ Cond2ESOf.fn pred) leftright
           >>=? fun t ->
           match (t, leftovers) with
           | (true, _) ->
               Lwt.return_ok true
           | (false, None) ->
               Lwt.return_ok false
           | (false, Some _) ->
               Lwt.return_error 101))
end
