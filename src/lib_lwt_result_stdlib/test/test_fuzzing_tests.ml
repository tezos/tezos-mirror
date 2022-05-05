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

module type Test = sig
  val tests : QCheck.Test.t list
end

module TestIterFold (M : sig
  include Traits.BASE with type 'a elt := int

  include Traits.ITER_SEQUENTIAL with type 'a elt := int and type 'a t := int t

  include FOLDLEFT_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) : Test = struct
  open QCheck

  let iter_fold_left =
    Test.make
      ~name:(Format.asprintf "%s.{iter,fold_left}" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let input = M.of_list input in
        eq
          (let acc = ref init in
           M.iter (IterOf.fn acc fn) input ;
           !acc)
          (M.fold_left (FoldOf.fn fn) init input))

  let iter_fold_left_e =
    Test.make
      ~name:(Format.asprintf "%s.{iter,fold_left}_e" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_e one many)
      (fun (fn, init, input) ->
        let input = M.of_list input in
        let open Result_syntax in
        eq_e
          (let acc = ref init in
           let+ () = M.iter_e (IterEOf.fn_e acc fn) input in
           !acc)
          (M.fold_left_e (FoldEOf.fn_e fn) init input))

  let iter_fold_left_s =
    Test.make
      ~name:(Format.asprintf "%s.{iter,fold_left}_s" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_s one many)
      (fun (fn, init, input) ->
        let input = M.of_list input in
        let open Lwt_syntax in
        eq_s
          (let acc = ref init in
           let+ () = M.iter_s (IterSOf.fn_s acc fn) input in
           !acc)
          (M.fold_left_s (FoldSOf.fn_s fn) init input))

  let iter_fold_left_es =
    Test.make
      ~name:(Format.asprintf "%s.{iter,fold_left}_es" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_es one many)
      (fun (fn, init, input) ->
        let input = M.of_list input in
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let+ () = M.iter_es (IterESOf.fn_es acc fn) input in
           !acc)
          (M.fold_left_es (FoldESOf.fn_es fn) init input))

  let tests =
    [iter_fold_left; iter_fold_left_e; iter_fold_left_s; iter_fold_left_es]
end

module TestRevMapRevMap (M : sig
  include BASE

  include Traits.REV_VANILLA with type 'a t := 'a t

  include Traits.MAP_PARALLEL with type 'a t := 'a t

  include Traits.REVMAP_PARALLEL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let rev_map =
    Test.make
      ~name:(Format.asprintf "%s.{rev map,rev_map}" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), const, input) ->
        let input = M.of_list input in
        let fn = MapOf.fn const fn in
        eq
          (let r = M.map fn input in
           M.rev r)
          (M.rev_map fn input))

  let rev_map_e =
    Test.make
      ~name:(Format.asprintf "%s.{rev map,rev_map}_e" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_e one many)
      (fun (fn, const, input) ->
        let input = M.of_list input in
        let fn = MapEOf.fn_e const fn in
        let open Result_syntax in
        eq_e
          (let+ r = M.map_e fn input in
           M.rev r)
          (M.rev_map_e fn input))

  let rev_map_s =
    Test.make
      ~name:(Format.asprintf "%s.{rev map,rev_map}_s" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_s one many)
      (fun (fn, const, input) ->
        let input = M.of_list input in
        let fn = MapSOf.fn_s const fn in
        let open Lwt_syntax in
        eq_s
          (let+ r = M.map_s fn input in
           M.rev r)
          (M.rev_map_s fn input))

  let rev_map_es =
    Test.make
      ~name:(Format.asprintf "%s.{rev map,rev_map}_es" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_es one many)
      (fun (fn, const, input) ->
        let input = M.of_list input in
        let fn = MapESOf.fn_es const fn in
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.map_es fn input in
           M.rev r)
          (M.rev_map_es fn input))

  let rev_map_p =
    Test.make
      ~name:(Format.asprintf "%s.{rev map,rev_map}_p" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_s one many)
      (fun (fn, const, input) ->
        let input = M.of_list input in
        let fn = MapSOf.fn_s const fn in
        let open Lwt_syntax in
        eq_s
          (let+ r = M.map_p fn input in
           M.rev r)
          (M.rev_map_p fn input))

  let rev_map_ep =
    Test.make
      ~name:(Format.asprintf "%s.{rev map,rev_map}_ep" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_es one many)
      (fun (fn, const, input) ->
        let input = M.of_list input in
        let fn_ep = MapEPOf.fn_ep const fn in
        let open Lwt_result_syntax in
        eq_ep
          ~pp:M.pp
          (let+ r = M.map_ep fn_ep input in
           M.rev r)
          (M.rev_map_ep fn_ep input))

  let tests = [rev_map; rev_map_e; rev_map_s; rev_map_es; rev_map_p; rev_map_ep]
end

module TestRevConcatMapRevConcatMap (M : sig
  include BASE

  include Traits.REV_VANILLA with type 'a t := 'a t

  include Traits.CONCATMAP_SEQUENTIAL with type 'a t := 'a t

  include Traits.REV_CONCATMAP_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let rev_concatmap =
    Test.make
      ~name:(Format.asprintf "%s.{rev concat_map,rev_concat_map}" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, fn), consta, constb, input) ->
        let input = M.of_list input in
        let fn = ConcatMapOf.fns M.of_list fn consta constb in
        eq
          (let r = M.concat_map fn input in
           M.rev r)
          (M.rev_concat_map fn input))

  let rev_concatmap_s =
    Test.make
      ~name:(Format.asprintf "%s.{rev concat_map_s,rev_concat_map_s}" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, fn), consta, constb, input) ->
        let input = M.of_list input in
        let fn = ConcatMapSOf.fns M.of_list fn consta constb in
        let open Lwt_syntax in
        eq_s
          (let+ r = M.concat_map_s fn input in
           M.rev r)
          (M.rev_concat_map_s fn input))

  let rev_concatmap_e =
    Test.make
      ~name:(Format.asprintf "%s.{rev concat_map_e,rev_concat_map_e}" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, fn), consta, constb, input) ->
        let input = M.of_list input in
        let fn = ConcatMapEOf.fns M.of_list fn consta constb in
        let open Result_syntax in
        eq_e
          (let+ r = M.concat_map_e fn input in
           M.rev r)
          (M.rev_concat_map_e fn input))

  let rev_concatmap_es =
    Test.make
      ~name:(Format.asprintf "%s.{rev concat_map_es,rev_concat_map_es}" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, fn), consta, constb, input) ->
        let input = M.of_list input in
        let fn = ConcatMapESOf.fns M.of_list fn consta constb in
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.concat_map_es fn input in
           M.rev r)
          (M.rev_concat_map_es fn input))

  let tests =
    [rev_concatmap; rev_concatmap_s; rev_concatmap_e; rev_concatmap_es]
end

module TestConcatMapConcatMap (M : sig
  include BASE

  include Traits.REV_VANILLA with type 'a t := 'a t

  include Traits.CONCATMAP_SEQUENTIAL with type 'a t := 'a t

  include Traits.CONCAT_VANILLA with type 'a t := 'a t

  include Traits.MAP_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let concatmap =
    Test.make
      ~name:(Format.asprintf "%s.{concat map,concat_map}" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, fn), consta, constb, input) ->
        let input = M.of_list input in
        let fn = ConcatMapOf.fns M.of_list fn consta constb in
        eq
          (let r = M.map fn input in
           M.concat r)
          (M.concat_map fn input))

  let concatmap_s =
    Test.make
      ~name:(Format.asprintf "%s.{concat map_s,concat_map_s}" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, fn), consta, constb, input) ->
        let input = M.of_list input in
        let fn = ConcatMapSOf.fns M.of_list fn consta constb in
        let open Lwt_syntax in
        eq_s
          (let+ r = M.map_s fn input in
           M.concat r)
          (M.concat_map_s fn input))

  let concatmap_e =
    Test.make
      ~name:(Format.asprintf "%s.{concat map_e,concat_map_e}" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, fn), consta, constb, input) ->
        let input = M.of_list input in
        let fn = ConcatMapEOf.fns M.of_list fn consta constb in
        let open Result_syntax in
        eq_e
          (let+ r = M.map_e fn input in
           M.concat r)
          (M.concat_map_e fn input))

  let concatmap_es =
    Test.make
      ~name:(Format.asprintf "%s.{concat map_es,concat_map_es}" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, fn), consta, constb, input) ->
        let input = M.of_list input in
        let fn = ConcatMapESOf.fns M.of_list fn consta constb in
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.map_es fn input in
           M.concat r)
          (M.concat_map_es fn input))

  let tests = [concatmap; concatmap_s; concatmap_e; concatmap_es]
end

module TestIterAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include Traits.ITER_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) : Test = struct
  open QCheck

  let with_stdlib_iter (fn, init, input) =
    let acc = ref init in
    Stdlib.List.iter (IterOf.fn acc fn) input ;
    !acc

  let iter =
    Test.make
      ~name:(Format.asprintf "%s.iter, Stdlib.List.iter" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq
          (let acc = ref init in
           M.iter (IterOf.fn acc fn) (M.of_list input) ;
           !acc)
          (with_stdlib_iter (fn, init, input)))

  let iter_e =
    Test.make
      ~name:(Format.asprintf "%s.iter_e, Stdlib.List.iter" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Result_syntax in
        eq_e
          (let acc = ref init in
           let+ () = M.iter_e (IterEOf.fn acc fn) (M.of_list input) in
           !acc)
          (Ok (with_stdlib_iter (fn, init, input))))

  let iter_s =
    Test.make
      ~name:(Format.asprintf "%s.iter_s, Stdlib.List.iter" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_syntax in
        eq_s
          (let acc = ref init in
           let+ () = M.iter_s (IterSOf.fn acc fn) (M.of_list input) in
           !acc)
          (Lwt.return @@ with_stdlib_iter (fn, init, input)))

  let iter_es =
    Test.make
      ~name:(Format.asprintf "%s.iter_es, Stdlib.List.iter" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let+ () = M.iter_es (IterESOf.fn acc fn) (M.of_list input) in
           !acc)
          (Lwt.return_ok @@ with_stdlib_iter (fn, init, input)))

  let tests = [iter; iter_e; iter_s; iter_es]
end

module TestIteriAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include Traits.ITERI_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) : Test = struct
  open QCheck

  let with_stdlib_iteri (fn, init, input) =
    let acc = ref init in
    Stdlib.List.iteri (IteriOf.fn acc fn) input ;
    !acc

  let iteri =
    Test.make
      ~name:(Format.asprintf "%s.iteri, Stdlib.List.iteri" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq
          (let acc = ref init in
           M.iteri (IteriOf.fn acc fn) (M.of_list input) ;
           !acc)
          (with_stdlib_iteri (fn, init, input)))

  let iteri_e =
    Test.make
      ~name:(Format.asprintf "%s.iteri_e, Stdlib.List.iteri" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Result_syntax in
        eq_e
          (let acc = ref init in
           let+ () = M.iteri_e (IteriEOf.fn acc fn) (M.of_list input) in
           !acc)
          (Ok (with_stdlib_iteri (fn, init, input))))

  let iteri_s =
    Test.make
      ~name:(Format.asprintf "%s.iteri_s, Stdlib.List.iteri" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_syntax in
        eq_s
          (let acc = ref init in
           let+ () = M.iteri_s (IteriSOf.fn acc fn) (M.of_list input) in
           !acc)
          (Lwt.return @@ with_stdlib_iteri (fn, init, input)))

  let iteri_es =
    Test.make
      ~name:(Format.asprintf "%s.iteri_es, Stdlib.List.iteri" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let+ () = M.iteri_es (IteriESOf.fn acc fn) (M.of_list input) in
           !acc)
          (Lwt.return_ok @@ with_stdlib_iteri (fn, init, input)))

  let tests = [iteri; iteri_e; iteri_s; iteri_es]
end

module TestIterMonotoneAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include Traits.ITER_PARALLEL with type 'a elt := int and type 'a t := int t
end) : Test = struct
  (* For collections without a specified ordering, or for out-of-order traversal
     we can only test iteration if the accumulator moves monotonically and the
     stepper doesn't depend on the accumulator. We do this here with a custom
     stepper. *)
  open QCheck

  let with_stdlib_iter init (fn, const, input) =
    let acc = ref init in
    Stdlib.List.iter (fun elt -> acc := !acc + MapOf.fn const fn elt) input ;
    !acc

  let iter =
    Test.make
      ~name:(Format.asprintf "%s.iter, Stdlib.List.iter" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (init, Fun (_, fn), const, input) ->
        eq
          (let acc = ref init in
           let () =
             M.iter
               (fun elt ->
                 let delta = MapOf.fn const fn elt in
                 acc := !acc + delta)
               (M.of_list input)
           in
           !acc)
          (with_stdlib_iter init (fn, const, input)))

  let iter_s =
    Test.make
      ~name:(Format.asprintf "%s.iter_s, Stdlib.List.iter" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (init, Fun (_, fn), const, input) ->
        let open Lwt_syntax in
        eq_s
          (let acc = ref init in
           let+ () =
             M.iter_s
               (fun elt ->
                 let+ delta = MapSOf.fn const fn elt in
                 acc := !acc + delta)
               (M.of_list input)
           in
           !acc)
          (Lwt.return @@ with_stdlib_iter init (fn, const, input)))

  let iter_es =
    Test.make
      ~name:(Format.asprintf "%s.iter_es, Stdlib.List.iter" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (init, Fun (_, fn), const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let+ () =
             M.iter_es
               (fun elt ->
                 let+ delta = MapESOf.fn const fn elt in
                 acc := !acc + delta)
               (M.of_list input)
           in
           !acc)
          (Lwt.return_ok @@ with_stdlib_iter init (fn, const, input)))

  let iter_p =
    Test.make
      ~name:(Format.asprintf "%s.iter_p, Stdlib.List.iter" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (init, Fun (_, fn), const, input) ->
        let open Lwt_syntax in
        eq_s
          (let acc = ref init in
           let+ () =
             M.iter_p
               (fun elt ->
                 let+ delta = MapSOf.fn const fn elt in
                 acc := !acc + delta)
               (M.of_list input)
           in
           !acc)
          (Lwt.return @@ with_stdlib_iter init (fn, const, input)))

  let iter_ep =
    Test.make
      ~name:(Format.asprintf "%s.iter_ep, Stdlib.List.iter" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (init, Fun (_, fn), const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let+ () =
             M.iter_ep
               (fun elt ->
                 let+ delta = MapESOf.fn const fn elt in
                 acc := !acc + delta)
               (M.of_list input)
           in
           !acc)
          (Lwt.return_ok @@ with_stdlib_iter init (fn, const, input)))

  let tests = [iter; iter_s; iter_es; iter_p; iter_ep]
end

module TestMapAgainstStdlibList (M : sig
  include BASE

  include Traits.MAP_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_map (fn, const, input) =
    Stdlib.List.map (MapOf.fn const fn) input

  let map =
    Test.make
      ~name:(Format.asprintf "%s.map, Stdlib.List.map" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), const, input) ->
        eq
          (M.to_list @@ M.map (MapOf.fn const fn) (M.of_list input))
          (with_stdlib_map (fn, const, input)))

  let map_e =
    Test.make
      ~name:(Format.asprintf "%s.map_e, Stdlib.List.map" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), const, input) ->
        let open Result_syntax in
        eq_e
          (let+ r = M.map_e (MapEOf.fn const fn) (M.of_list input) in
           M.to_list r)
          (Ok (with_stdlib_map (fn, const, input))))

  let map_s =
    Test.make
      ~name:(Format.asprintf "%s.map_s, Stdlib.List.map" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), const, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r = M.map_s (MapSOf.fn const fn) (M.of_list input) in
           M.to_list r)
          (Lwt.return @@ with_stdlib_map (fn, const, input)))

  let map_es =
    Test.make
      ~name:(Format.asprintf "%s.map_es, Stdlib.List.map" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.map_es (MapESOf.fn const fn) (M.of_list input) in
           M.to_list r)
          (Lwt.return_ok @@ with_stdlib_map (fn, const, input)))

  let tests = [map; map_e; map_s; map_es]
end

module TestMappAgainstStdlibList (M : sig
  include BASE

  include Traits.MAP_PARALLEL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_map (fn, const, input) =
    Stdlib.List.map (MapOf.fn const fn) input

  let map_p =
    Test.make
      ~name:(Format.asprintf "%s.map_p, Stdlib.List.map" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), const, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r = M.map_p (MapSOf.fn const fn) (M.of_list input) in
           M.to_list r)
          (Lwt.return @@ with_stdlib_map (fn, const, input)))

  let map_ep =
    Test.make
      ~name:(Format.asprintf "%s.map_ep, Stdlib.List.map" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.map_ep (MapESOf.fn const fn) (M.of_list input) in
           M.to_list r)
          (Lwt.return_ok @@ with_stdlib_map (fn, const, input)))

  let tests = [map_p; map_ep]
end

module TestFoldAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include FOLDLEFT_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) : Test = struct
  open QCheck

  let with_stdlib_fold_left (fn, init, input) =
    Stdlib.List.fold_left (FoldOf.fn fn) init input

  let fold_left =
    Test.make
      ~name:(Format.asprintf "%s.fold_left, Stdlib.List.fold_left" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq
          (M.fold_left (FoldOf.fn fn) init (M.of_list input))
          (with_stdlib_fold_left (fn, init, input)))

  let fold_left_e =
    Test.make
      ~name:(Format.asprintf "%s.fold_left_e, Stdlib.List.fold_left" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq_e
          (M.fold_left_e (FoldEOf.fn fn) init (M.of_list input))
          (Ok (with_stdlib_fold_left (fn, init, input))))

  let fold_left_s =
    Test.make
      ~name:(Format.asprintf "%s.fold_left_s, Stdlib.List.fold_left" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq_s
          (M.fold_left_s (FoldSOf.fn fn) init (M.of_list input))
          (Lwt.return @@ with_stdlib_fold_left (fn, init, input)))

  let fold_left_es =
    Test.make
      ~name:(Format.asprintf "%s.fold_left_es, Stdlib.List.fold_left" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq_es
          (M.fold_left_es (FoldESOf.fn fn) init (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_fold_left (fn, init, input)))

  let tests = [fold_left; fold_left_e; fold_left_s; fold_left_es]
end

module TestFoldLeftMapAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include FOLDLEFTMAP_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) : Test = struct
  open QCheck

  let with_stdlib_fold_left_map (accum, init, input) =
    Stdlib.List.fold_left_map (FoldOf.fn accum) init input

  let accum = fun2 Observable.int Observable.int (pair int int)

  let fold_left_map =
    Test.make
      ~name:
        (Format.asprintf "%s.fold_left_map, Stdlib.List.fold_left_map" M.name)
      (triple accum one many)
      (fun (Fun (_, fn), init, input) ->
        let (a, xs) = M.fold_left_map (FoldOf.fn fn) init (M.of_list input) in
        eq (a, xs) (with_stdlib_fold_left_map (fn, init, input)))

  let fold_left_map_e =
    Test.make
      ~name:
        (Format.asprintf "%s.fold_left_map_e, Stdlib.List.fold_left_map" M.name)
      (triple accum one many)
      (fun (Fun (_, fn), init, input) ->
        eq_e
          (M.fold_left_map_e (FoldEOf.fn fn) init (M.of_list input))
          (Result.ok @@ with_stdlib_fold_left_map (fn, init, input)))

  let fold_left_map_s =
    Test.make
      ~name:
        (Format.asprintf "%s.fold_left_map_s, Stdlib.List.fold_left_map" M.name)
      (triple accum one many)
      (fun (Fun (_, fn), init, input) ->
        eq_s
          (M.fold_left_map_s (FoldSOf.fn fn) init (M.of_list input))
          (Lwt.return @@ with_stdlib_fold_left_map (fn, init, input)))

  let fold_left_map_es =
    Test.make
      ~name:
        (Format.asprintf
           "%s.fold_left_map_es, Stdlib.List.fold_left_map"
           M.name)
      (triple accum one many)
      (fun (Fun (_, fn), init, input) ->
        eq_es
          (M.fold_left_map_es (FoldESOf.fn fn) init (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_fold_left_map (fn, init, input)))

  let tests =
    [fold_left_map; fold_left_map_e; fold_left_map_s; fold_left_map_es]
end

module TestFoldMonotonicAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include FOLDOOO_SEQUENTIAL with type 'a elt := int and type 'a t := int t
end) : Test = struct
  open QCheck

  let with_stdlib_fold_left const (fn, init, input) =
    Stdlib.List.fold_left (fun acc x -> acc + FoldOf.fn fn const x) init input

  let fold =
    Test.make
      ~name:(Format.asprintf "%s.fold, Stdlib.List.fold_left" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (const, Fun (_, fn), init, input) ->
        eq
          (M.fold
             (fun x acc ->
               let delta = FoldOf.fn fn const x in
               acc + delta)
             (M.of_list input)
             init)
          (with_stdlib_fold_left const (fn, init, input)))

  let fold_e =
    Test.make
      ~name:(Format.asprintf "%s.fold_e, Stdlib.List.fold_left" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (const, Fun (_, fn), init, input) ->
        let open Result_syntax in
        eq_e
          (M.fold_e
             (fun x acc ->
               let+ delta = FoldEOf.fn fn const x in
               acc + delta)
             (M.of_list input)
             init)
          (Ok (with_stdlib_fold_left const (fn, init, input))))

  let fold_s =
    Test.make
      ~name:(Format.asprintf "%s.fold_s, Stdlib.List.fold_left" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (const, Fun (_, fn), init, input) ->
        let open Lwt_syntax in
        eq_s
          (M.fold_s
             (fun x acc ->
               let+ delta = FoldSOf.fn fn const x in
               acc + delta)
             (M.of_list input)
             init)
          (Lwt.return @@ with_stdlib_fold_left const (fn, init, input)))

  let fold_es =
    Test.make
      ~name:(Format.asprintf "%s.fold_es, Stdlib.List.fold_left" M.name)
      (quad one Test_fuzzing_helpers.Fn.arith one many)
      (fun (const, Fun (_, fn), init, input) ->
        let open Lwt_result_syntax in
        eq_es
          (M.fold_es
             (fun x acc ->
               let+ delta = FoldESOf.fn fn const x in
               acc + delta)
             (M.of_list input)
             init)
          (Lwt.return_ok @@ with_stdlib_fold_left const (fn, init, input)))

  let tests = [fold; fold_e; fold_s; fold_es]
end

module TestFoldRightAgainstStdlibList (M : sig
  include BASE

  include Traits.FOLDRIGHT_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_fold_right (fn, init, input) =
    Stdlib.List.fold_right (FoldOf.fn fn) input init

  let fold_right =
    Test.make
      ~name:(Format.asprintf "%s.fold_right, Stdlib.List.fold_right" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq
          (M.fold_right (FoldOf.fn fn) (M.of_list input) init)
          (with_stdlib_fold_right (fn, init, input)))

  let fold_right_e =
    Test.make
      ~name:(Format.asprintf "%s.fold_right_e, Stdlib.List.fold_right" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq_e
          (M.fold_right_e (FoldEOf.fn fn) (M.of_list input) init)
          (Ok (with_stdlib_fold_right (fn, init, input))))

  let fold_right_s =
    Test.make
      ~name:(Format.asprintf "%s.fold_right_s, Stdlib.List.fold_right" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq_s
          (M.fold_right_s (FoldSOf.fn fn) (M.of_list input) init)
          (Lwt.return @@ with_stdlib_fold_right (fn, init, input)))

  let fold_right_es =
    Test.make
      ~name:(Format.asprintf "%s.fold_right_es, Stdlib.List.fold_right" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one many)
      (fun (Fun (_, fn), init, input) ->
        eq_es
          (M.fold_right_es (FoldESOf.fn fn) (M.of_list input) init)
          (Lwt.return_ok @@ with_stdlib_fold_right (fn, init, input)))

  let tests = [fold_right; fold_right_e; fold_right_s; fold_right_es]
end

module TestExistForallAgainstStdlibList (M : sig
  include BASE with type 'a elt := int

  include
    Traits.EXISTFORALL_PARALLEL with type 'a elt := int and type 'a t := int t
end) : Test = struct
  open QCheck

  let with_stdlib_exists (fn, const, input) =
    Stdlib.List.exists (CondOf.fn fn const) input

  let exists =
    Test.make
      ~name:(Format.asprintf "%s.exists, Stdlib.List.exists" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq
          (M.exists (CondOf.fn fn const) (M.of_list input))
          (with_stdlib_exists (fn, const, input)))

  let exists_e =
    Test.make
      ~name:(Format.asprintf "%s.exists_e, Stdlib.List.exists" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_e
          (M.exists_e (CondEOf.fn fn const) (M.of_list input))
          (Ok (with_stdlib_exists (fn, const, input))))

  let exists_s =
    Test.make
      ~name:(Format.asprintf "%s.exists_s, Stdlib.List.exists" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_s
          (M.exists_s (CondSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_exists (fn, const, input)))

  let exists_es =
    Test.make
      ~name:(Format.asprintf "%s.exists_es, Stdlib.List.exists" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_es
          (M.exists_es (CondESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_exists (fn, const, input)))

  let exists_p =
    Test.make
      ~name:(Format.asprintf "%s.exists_p, Stdlib.List.exists" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_s
          (M.exists_p (CondSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_exists (fn, const, input)))

  let exists_ep =
    Test.make
      ~name:(Format.asprintf "%s.exists_ep, Stdlib.List.exists" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_es
          (M.exists_ep (CondESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_exists (fn, const, input)))

  let tests_exists =
    [exists; exists_e; exists_s; exists_es; exists_p; exists_ep]

  let with_stdlib_for_all (fn, const, input) =
    Stdlib.List.for_all (CondOf.fn fn const) input

  let for_all =
    Test.make
      ~name:(Format.asprintf "%s.for_all, Stdlib.List.for_all" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq
          (M.for_all (CondOf.fn fn const) (M.of_list input))
          (with_stdlib_for_all (fn, const, input)))

  let for_all_e =
    Test.make
      ~name:(Format.asprintf "%s.for_all_e, Stdlib.List.for_all" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_e
          (M.for_all_e (CondEOf.fn fn const) (M.of_list input))
          (Ok (with_stdlib_for_all (fn, const, input))))

  let for_all_s =
    Test.make
      ~name:(Format.asprintf "%s.for_all_s, Stdlib.List.for_all" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_s
          (M.for_all_s (CondSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_for_all (fn, const, input)))

  let for_all_es =
    Test.make
      ~name:(Format.asprintf "%s.for_all_es, Stdlib.List.for_all" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_es
          (M.for_all_es (CondESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_for_all (fn, const, input)))

  let for_all_p =
    Test.make
      ~name:(Format.asprintf "%s.for_all_p, Stdlib.List.for_all" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_s
          (M.for_all_p (CondSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_for_all (fn, const, input)))

  let for_all_ep =
    Test.make
      ~name:(Format.asprintf "%s.for_all_ep, Stdlib.List.for_all" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq_es
          (M.for_all_ep (CondESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_for_all (fn, const, input)))

  let tests_for_all =
    [for_all; for_all_e; for_all_s; for_all_es; for_all_p; for_all_ep]

  let tests = tests_exists @ tests_for_all
end

module TestFilterAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTER_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_filter (fn, const, input) =
    Stdlib.List.filter (CondOf.fn fn const) input

  let filter =
    Test.make
      ~name:(Format.asprintf "%s.filter, Stdlib.List.filter" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        eq
          (let r = M.filter (CondOf.fn fn const) (M.of_list input) in
           M.to_list r)
          (with_stdlib_filter (fn, const, input)))

  let filter_e =
    Test.make
      ~name:(Format.asprintf "%s.filter_e, Stdlib.List.filter" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        let open Result_syntax in
        eq_e
          (let+ r = M.filter_e (CondEOf.fn fn const) (M.of_list input) in
           M.to_list r)
          (Ok (with_stdlib_filter (fn, const, input))))

  let filter_s =
    Test.make
      ~name:(Format.asprintf "%s.filter_s, Stdlib.List.filter" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r = M.filter_s (CondSOf.fn fn const) (M.of_list input) in
           M.to_list r)
          (Lwt.return @@ with_stdlib_filter (fn, const, input)))

  let filter_es =
    Test.make
      ~name:(Format.asprintf "%s.filter_es, Stdlib.List.filter" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.filter_es (CondESOf.fn fn const) (M.of_list input) in
           M.to_list r)
          (Lwt.return_ok @@ with_stdlib_filter (fn, const, input)))

  let tests = [filter; filter_e; filter_s; filter_es]
end

module TestFilterpAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTER_PARALLEL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_filter (fn, const, input) =
    Stdlib.List.filter (CondOf.fn fn const) input

  let filter_p =
    Test.make
      ~name:(Format.asprintf "%s.filter_p, Stdlib.List.filter" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r = M.filter_p (CondSOf.fn fn const) (M.of_list input) in
           M.to_list r)
          (Lwt.return @@ with_stdlib_filter (fn, const, input)))

  let filter_ep =
    Test.make
      ~name:(Format.asprintf "%s.filter_ep, Stdlib.List.filter" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (fn, const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.filter_ep (CondESOf.fn fn const) (M.of_list input) in
           M.to_list r)
          (Lwt.return_ok @@ with_stdlib_filter (fn, const, input)))

  let tests = [filter_p; filter_ep]
end

module TestFilteriAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTERI_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_filteri (fn, input) = Stdlib.List.filteri (CondOf.fn fn) input

  let filteri =
    Test.make
      ~name:(Format.asprintf "%s.filteri, Stdlib.List.filteri" M.name)
      (pair Test_fuzzing_helpers.Fn.pred many)
      (fun (fn, input) ->
        eq
          (let r = M.filteri (CondOf.fn fn) (M.of_list input) in
           M.to_list r)
          (with_stdlib_filteri (fn, input)))

  let filteri_e =
    Test.make
      ~name:(Format.asprintf "%s.filteri_e, Stdlib.List.filteri" M.name)
      (pair Test_fuzzing_helpers.Fn.pred many)
      (fun (fn, input) ->
        let open Result_syntax in
        eq_e
          (let+ r = M.filteri_e (CondEOf.fn fn) (M.of_list input) in
           M.to_list r)
          (Ok (with_stdlib_filteri (fn, input))))

  let filteri_s =
    Test.make
      ~name:(Format.asprintf "%s.filteri_s, Stdlib.List.filteri" M.name)
      (pair Test_fuzzing_helpers.Fn.pred many)
      (fun (fn, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r = M.filteri_s (CondSOf.fn fn) (M.of_list input) in
           M.to_list r)
          (Lwt.return @@ with_stdlib_filteri (fn, input)))

  let filteri_es =
    Test.make
      ~name:(Format.asprintf "%s.filteri_es, Stdlib.List.filteri" M.name)
      (pair Test_fuzzing_helpers.Fn.pred many)
      (fun (fn, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.filteri_es (CondESOf.fn fn) (M.of_list input) in
           M.to_list r)
          (Lwt.return_ok @@ with_stdlib_filteri (fn, input)))

  let tests = [filteri; filteri_e; filteri_s; filteri_es]
end

module TestFilteripAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTERI_PARALLEL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_filteri (fn, input) = Stdlib.List.filteri (CondOf.fn fn) input

  let filteri_p =
    Test.make
      ~name:(Format.asprintf "%s.filteri_p, Stdlib.List.filteri" M.name)
      (pair Test_fuzzing_helpers.Fn.pred many)
      (fun (fn, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r = M.filteri_p (CondSOf.fn fn) (M.of_list input) in
           M.to_list r)
          (Lwt.return @@ with_stdlib_filteri (fn, input)))

  let filteri_ep =
    Test.make
      ~name:(Format.asprintf "%s.filteri_ep, Stdlib.List.filteri" M.name)
      (pair Test_fuzzing_helpers.Fn.pred many)
      (fun (fn, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r = M.filteri_ep (CondESOf.fn fn) (M.of_list input) in
           M.to_list r)
          (Lwt.return_ok @@ with_stdlib_filteri (fn, input)))

  let tests = [filteri_p; filteri_ep]
end

module TestFiltermapAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTERMAP_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_filter_map (pred, arith, const, input) =
    Stdlib.List.filter_map (FilterMapOf.fns pred arith const) input

  let filter_map =
    Test.make
      ~name:(Format.asprintf "%s.filter_map, Stdlib.List.filter_map" M.name)
      (quad Test_fuzzing_helpers.Fn.pred Test_fuzzing_helpers.Fn.arith one many)
      (fun (pred, Fun (_, arith), const, input) ->
        eq
          (let r =
             M.filter_map (FilterMapOf.fns pred arith const) (M.of_list input)
           in
           M.to_list r)
          (with_stdlib_filter_map (pred, arith, const, input)))

  let filter_map_e =
    Test.make
      ~name:(Format.asprintf "%s.filter_map_e, Stdlib.List.filter_map" M.name)
      (quad Test_fuzzing_helpers.Fn.pred Test_fuzzing_helpers.Fn.arith one many)
      (fun (pred, Fun (_, arith), const, input) ->
        let open Result_syntax in
        eq_e
          (let+ r =
             M.filter_map_e
               (FilterMapEOf.fns pred arith const)
               (M.of_list input)
           in
           M.to_list r)
          (Ok (with_stdlib_filter_map (pred, arith, const, input))))

  let filter_map_s =
    Test.make
      ~name:(Format.asprintf "%s.filter_map_s, Stdlib.List.filter_map" M.name)
      (quad Test_fuzzing_helpers.Fn.pred Test_fuzzing_helpers.Fn.arith one many)
      (fun (pred, Fun (_, arith), const, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r =
             M.filter_map_s
               (FilterMapSOf.fns pred arith const)
               (M.of_list input)
           in
           M.to_list r)
          (Lwt.return @@ with_stdlib_filter_map (pred, arith, const, input)))

  let filter_map_es =
    Test.make
      ~name:(Format.asprintf "%s.filter_map_es, Stdlib.List.filter_map" M.name)
      (quad Test_fuzzing_helpers.Fn.pred Test_fuzzing_helpers.Fn.arith one many)
      (fun (pred, Fun (_, arith), const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r =
             M.filter_map_es
               (FilterMapESOf.fns pred arith const)
               (M.of_list input)
           in
           M.to_list r)
          (Lwt.return_ok @@ with_stdlib_filter_map (pred, arith, const, input)))

  let tests = [filter_map; filter_map_e; filter_map_s; filter_map_es]
end

module TestFiltermappAgainstStdlibList (M : sig
  include BASE

  include Traits.FILTERMAP_PARALLEL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_filter_map (pred, arith, const, input) =
    Stdlib.List.filter_map (FilterMapOf.fns pred arith const) input

  let filter_map_p =
    Test.make
      ~name:(Format.asprintf "%s.filter_map_p, Stdlib.List.filter_map" M.name)
      (quad Test_fuzzing_helpers.Fn.pred Test_fuzzing_helpers.Fn.arith one many)
      (fun (pred, Fun (_, arith), const, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r =
             M.filter_map_p
               (FilterMapSOf.fns pred arith const)
               (M.of_list input)
           in
           M.to_list r)
          (Lwt.return @@ with_stdlib_filter_map (pred, arith, const, input)))

  let filter_map_ep =
    Test.make
      ~name:(Format.asprintf "%s.filter_map_ep, Stdlib.List.filter_map" M.name)
      (quad Test_fuzzing_helpers.Fn.pred Test_fuzzing_helpers.Fn.arith one many)
      (fun (pred, Fun (_, arith), const, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r =
             M.filter_map_ep
               (FilterMapESOf.fns pred arith const)
               (M.of_list input)
           in
           M.to_list r)
          (Lwt.return_ok @@ with_stdlib_filter_map (pred, arith, const, input)))

  let tests = [filter_map_p; filter_map_ep]
end

module TestConcatmapAgainstStdlibList (M : sig
  include BASE

  include Traits.CONCATMAP_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_concat_map (arith, consta, constb, input) =
    Stdlib.List.concat_map (ConcatMapOf.fns Fun.id arith consta constb) input

  let concat_map =
    Test.make
      ~name:(Format.asprintf "%s.concat_map, Stdlib.List.concat_map" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, arith), consta, constb, input) ->
        eq
          (let r =
             M.concat_map
               (ConcatMapOf.fns M.of_list arith consta constb)
               (M.of_list input)
           in
           M.to_list r)
          (with_stdlib_concat_map (arith, consta, constb, input)))

  let concat_map_e =
    Test.make
      ~name:(Format.asprintf "%s.concat_map_e, Stdlib.List.concat_map" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, arith), consta, constb, input) ->
        let open Result_syntax in
        eq_e
          (let+ r =
             M.concat_map_e
               (ConcatMapEOf.fns M.of_list arith consta constb)
               (M.of_list input)
           in
           M.to_list r)
          (Ok (with_stdlib_concat_map (arith, consta, constb, input))))

  let concat_map_s =
    Test.make
      ~name:(Format.asprintf "%s.concat_map_s, Stdlib.List.concat_map" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, arith), consta, constb, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r =
             M.concat_map_s
               (ConcatMapSOf.fns M.of_list arith consta constb)
               (M.of_list input)
           in
           M.to_list r)
          (Lwt.return @@ with_stdlib_concat_map (arith, consta, constb, input)))

  let concat_map_es =
    Test.make
      ~name:(Format.asprintf "%s.concat_map_es, Stdlib.List.concat_map" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, arith), consta, constb, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r =
             M.concat_map_es
               (ConcatMapESOf.fns M.of_list arith consta constb)
               (M.of_list input)
           in
           M.to_list r)
          (Lwt.return_ok
          @@ with_stdlib_concat_map (arith, consta, constb, input)))

  let tests = [concat_map; concat_map_e; concat_map_s; concat_map_es]
end

module TestConcatmappAgainstStdlibList (M : sig
  include BASE

  include Traits.CONCATMAP_PARALLEL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_concat_map (arith, consta, constb, input) =
    Stdlib.List.concat_map (ConcatMapOf.fns Fun.id arith consta constb) input

  let concat_map_p =
    Test.make
      ~name:(Format.asprintf "%s.concat_map_p, Stdlib.List.concat_map" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, arith), consta, constb, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r =
             M.concat_map_p
               (ConcatMapSOf.fns M.of_list arith consta constb)
               (M.of_list input)
           in
           M.to_list r)
          (Lwt.return @@ with_stdlib_concat_map (arith, consta, constb, input)))

  let concat_map_ep =
    Test.make
      ~name:(Format.asprintf "%s.concat_map_ep, Stdlib.List.concat_map" M.name)
      (quad Test_fuzzing_helpers.Fn.arith one one many)
      (fun (Fun (_, arith), consta, constb, input) ->
        let open Lwt_result_syntax in
        eq_es
          (let+ r =
             M.concat_map_ep
               (ConcatMapESOf.fns M.of_list arith consta constb)
               (M.of_list input)
           in
           M.to_list r)
          (Lwt.return_ok
          @@ with_stdlib_concat_map (arith, consta, constb, input)))

  let tests = [concat_map_p; concat_map_ep]
end

module TestFindStdlibList (M : sig
  include BASE

  include Traits.FIND_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_find (pred, const, input) =
    Stdlib.List.find_opt (CondOf.fn pred const) input

  let find =
    Test.make
      ~name:(Format.asprintf "%s.find, Stdlib.List.find_opt" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        eq
          (M.find (CondOf.fn pred const) (M.of_list input))
          (with_stdlib_find (pred, const, input)))

  let find_e =
    Test.make
      ~name:(Format.asprintf "%s.find_e, Stdlib.List.find_opt" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        eq
          (M.find_e (CondEOf.fn pred const) (M.of_list input))
          (Ok (with_stdlib_find (pred, const, input))))

  let find_s =
    Test.make
      ~name:(Format.asprintf "%s.find_s, Stdlib.List.find_opt" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        eq_s
          (M.find_s (CondSOf.fn pred const) (M.of_list input))
          (Lwt.return @@ with_stdlib_find (pred, const, input)))

  let find_es =
    Test.make
      ~name:(Format.asprintf "%s.find_es, Stdlib.List.find_opt" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        eq_s
          (M.find_es (CondESOf.fn pred const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_find (pred, const, input)))

  let tests = [find; find_e; find_s; find_es]
end

module TestFindMapStdlibList (M : sig
  include BASE

  include Traits.FINDMAP_SEQUENTIAL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_find_map (fn, const, input) =
    Stdlib.List.find_map (CondMapOf.fn fn const) input

  let find_map =
    Test.make
      ~name:(Format.asprintf "%s.find_map, Stdlib.List.find_map" M.name)
      (triple Test_fuzzing_helpers.Fn.predarith one many)
      (fun (Fun (_, fn), const, input) ->
        eq
          (M.find_map (CondMapOf.fn fn const) (M.of_list input))
          (with_stdlib_find_map (fn, const, input)))

  let find_map_e =
    Test.make
      ~name:(Format.asprintf "%s.find_map_e, Stdlib.List.find" M.name)
      (triple Test_fuzzing_helpers.Fn.predarith one many)
      (fun (Fun (_, fn), const, input) ->
        eq
          (M.find_map_e (CondMapEOf.fn fn const) (M.of_list input))
          (Ok (with_stdlib_find_map (fn, const, input))))

  let find_map_s =
    Test.make
      ~name:(Format.asprintf "%s.find_map_s, Stdlib.List.find_opt" M.name)
      (triple Test_fuzzing_helpers.Fn.predarith one many)
      (fun (Fun (_, fn), const, input) ->
        eq_s
          (M.find_map_s (CondMapSOf.fn fn const) (M.of_list input))
          (Lwt.return @@ with_stdlib_find_map (fn, const, input)))

  let find_map_es =
    Test.make
      ~name:(Format.asprintf "%s.find_map_es, Stdlib.List.find_opt" M.name)
      (triple Test_fuzzing_helpers.Fn.predarith one many)
      (fun (Fun (_, fn), const, input) ->
        eq_s
          (M.find_map_es (CondMapESOf.fn fn const) (M.of_list input))
          (Lwt.return_ok @@ with_stdlib_find_map (fn, const, input)))

  let tests = [find_map; find_map_e; find_map_s; find_map_es]
end

module TestPartitions (M : sig
  include BASE

  include Traits.MAP_VANILLA with type 'a t := 'a t

  include Traits.PARTITION_EXTRAS with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let partition_either =
    Test.make
      ~name:(Format.asprintf "%s.partition, %s.partition_either" M.name M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (M.partition cond (M.of_list input))
          (M.partition_either
             (M.map
                (fun x -> if cond x then Either.Left x else Either.Right x)
                (M.of_list input))))

  let partition_result =
    Test.make
      ~name:(Format.asprintf "%s.partition, %s.partition_result" M.name M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (M.partition cond (M.of_list input))
          (M.partition_result
             (M.map
                (fun x -> if cond x then Ok x else Error x)
                (M.of_list input))))

  let tests = [partition_either; partition_result]
end

module TestPartitionMap (M : sig
  include BASE

  include Traits.PARTITIONMAP_VANILLA with type 'a t := 'a t

  include Traits.MAP_VANILLA with type 'a t := 'a t

  include Traits.PARTITION_VANILLA with type 'a t := 'a t

  include Traits.PARTITION_EXTRAS with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let mapper_of_fns pred fn const x =
    if pred const x then Either.Left (fn const x) else Either.Right (fn x const)

  let g =
    quad Test_fuzzing_helpers.Fn.pred Test_fuzzing_helpers.Fn.arith one many

  let partition_map =
    Test.make
      ~name:(Format.asprintf "%s.partition_map, %s.partition+map" M.name M.name)
      g
      (fun (pred, Fun (_, arith), const, input) ->
        let mapper = mapper_of_fns pred arith const in
        eq
          (M.partition_map mapper (M.of_list input))
          (M.partition_either (M.map mapper (M.of_list input))))

  let tests = [partition_map]
end

module TestPartitionStdlibList (M : sig
  include BASE

  include Traits.PARTITION_PARALLEL with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let with_stdlib_partition (pred, const, input) =
    Stdlib.List.partition (CondOf.fn pred const) input

  let to_list_pair (a, b) = (M.to_list a, M.to_list b)

  let partition =
    Test.make
      ~name:(Format.asprintf "%s.partition, Stdlib.List.partition" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        eq
          (let r = M.partition (CondOf.fn pred const) (M.of_list input) in
           to_list_pair r)
          (with_stdlib_partition (pred, const, input)))

  let partition_e =
    Test.make
      ~name:(Format.asprintf "%s.partition_e, Stdlib.List.partition" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let open Result_syntax in
        eq
          (let+ r = M.partition_e (CondEOf.fn pred const) (M.of_list input) in
           to_list_pair r)
          (Ok (with_stdlib_partition (pred, const, input))))

  let partition_s =
    Test.make
      ~name:(Format.asprintf "%s.partition_s, Stdlib.List.partition" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r = M.partition_s (CondSOf.fn pred const) (M.of_list input) in
           to_list_pair r)
          (Lwt.return @@ with_stdlib_partition (pred, const, input)))

  let partition_es =
    Test.make
      ~name:(Format.asprintf "%s.partition_es, Stdlib.List.partition" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let open Lwt_result_syntax in
        eq_s
          (let+ r = M.partition_es (CondESOf.fn pred const) (M.of_list input) in
           to_list_pair r)
          (Lwt.return_ok @@ with_stdlib_partition (pred, const, input)))

  let partition_p =
    Test.make
      ~name:(Format.asprintf "%s.partition_p, Stdlib.List.partition" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let open Lwt_syntax in
        eq_s
          (let+ r = M.partition_p (CondSOf.fn pred const) (M.of_list input) in
           to_list_pair r)
          (Lwt.return @@ with_stdlib_partition (pred, const, input)))

  let partition_ep =
    Test.make
      ~name:(Format.asprintf "%s.partition_ep, Stdlib.List.partition" M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let open Lwt_result_syntax in
        eq_s
          (let+ r = M.partition_ep (CondESOf.fn pred const) (M.of_list input) in
           to_list_pair r)
          (Lwt.return_ok @@ with_stdlib_partition (pred, const, input)))

  let tests =
    [
      partition;
      partition_e;
      partition_s;
      partition_es;
      partition_p;
      partition_ep;
    ]
end

module TestFilters (M : sig
  include BASE

  include Traits.MAP_VANILLA with type 'a t := 'a t

  include Traits.FILTER_EXTRAS with type 'a t := 'a t
end) : Test = struct
  open QCheck

  let filter_left =
    Test.make
      ~name:(Format.asprintf "%s.filter, %s.filter_left" M.name M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (M.filter cond (M.of_list input))
          (M.filter_left
             (M.map
                (fun x -> if cond x then Either.Left x else Either.Right x)
                (M.of_list input))))

  let filter_right =
    Test.make
      ~name:(Format.asprintf "%s.filter, %s.filter_right" M.name M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (M.filter cond (M.of_list input))
          (M.filter_right
             (M.map
                (fun x -> if cond x then Either.Right x else Either.Left x)
                (M.of_list input))))

  let filter_ok =
    Test.make
      ~name:(Format.asprintf "%s.filter, %s.filter_ok" M.name M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (M.filter cond (M.of_list input))
          (M.filter_ok
             (M.map
                (fun x -> if cond x then Ok x else Error x)
                (M.of_list input))))

  let filter_error =
    Test.make
      ~name:(Format.asprintf "%s.filter, %s.filter_error" M.name M.name)
      (triple Test_fuzzing_helpers.Fn.pred one many)
      (fun (pred, const, input) ->
        let cond = CondOf.fn pred const in
        eq
          (M.filter cond (M.of_list input))
          (M.filter_error
             (M.map
                (fun x -> if cond x then Error x else Ok x)
                (M.of_list input))))

  let tests = [filter_left; filter_right; filter_ok; filter_error]
end

module TestDoubleTraversorsStdlibList (M : sig
  include BASE

  include Traits.REV_VANILLA with type 'a t := 'a t

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
end) : Test = struct
  open QCheck

  let uncurry f (x, y) = f x y

  let uncurry_l f acc (x, y) = f acc x y

  let uncurry_r f (x, y) acc = f x y acc

  let iter =
    Test.make
      ~name:(Format.asprintf "%s.iter{2,}" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one manymany)
      (fun (Fun (_, fn), init, (left, right)) ->
        let open Result_syntax in
        eq_e
          (let acc = ref init in
           let+ () =
             M.iter2
               ~when_different_lengths:101
               (Iter2Of.fn acc fn)
               (M.of_list left)
               (M.of_list right)
           in
           !acc)
          (let acc = ref init in
           let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           M.iter (uncurry @@ Iter2Of.fn acc fn) leftright ;
           match leftovers with None -> Ok !acc | Some _ -> Error 101))

  let iter_e =
    Test.make
      ~name:(Format.asprintf "%s.iter{2,}_e" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_e one manymany)
      (fun (fn, init, (left, right)) ->
        let open Result_syntax in
        eq_e
          (let acc = ref init in
           let+ () =
             M.iter2_e
               ~when_different_lengths:101
               (Iter2EOf.fn_e acc fn)
               (M.of_list left)
               (M.of_list right)
           in
           !acc)
          (let acc = ref init in
           let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* () = M.iter_e (uncurry @@ Iter2EOf.fn_e acc fn) leftright in
           match leftovers with None -> Ok !acc | Some _ -> Error 101))

  let iter_s =
    Test.make
      ~name:(Format.asprintf "%s.iter{2,}_s" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_s one manymany)
      (fun (fn, init, (left, right)) ->
        let open Lwt_result_syntax in
        eq_s
          (let acc = ref init in
           let+ () =
             M.iter2_s
               ~when_different_lengths:101
               (Iter2SOf.fn_s acc fn)
               (M.of_list left)
               (M.of_list right)
           in
           !acc)
          (let acc = ref init in
           let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let*! () = M.iter_s (uncurry @@ Iter2SOf.fn_s acc fn) leftright in
           match leftovers with None -> return !acc | Some _ -> fail 101))

  let iter_es =
    Test.make
      ~name:(Format.asprintf "%s.iter{2,}_es" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_e one manymany)
      (fun (fn, init, (left, right)) ->
        let open Lwt_result_syntax in
        eq_es
          (let acc = ref init in
           let+ () =
             M.iter2_es
               ~when_different_lengths:101
               (Iter2ESOf.fn_e acc fn)
               (M.of_list left)
               (M.of_list right)
           in
           !acc)
          (let acc = ref init in
           let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* () = M.iter_es (uncurry @@ Iter2ESOf.fn_e acc fn) leftright in
           match leftovers with None -> return !acc | Some _ -> fail 101))

  let tests_iter = [iter; iter_e; iter_s; iter_es]

  let map =
    Test.make
      ~name:(Format.asprintf "%s.map{2,}" M.name)
      (pair Test_fuzzing_helpers.Fn.arith manymany)
      (fun (Fun (_, fn), (left, right)) ->
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

  let map_e =
    Test.make
      ~name:(Format.asprintf "%s.map{2,}_e" M.name)
      (pair Test_fuzzing_helpers.Fn.arith_e manymany)
      (fun (fn, (left, right)) ->
        let open Result_syntax in
        eq_e
          (M.map2_e
             ~when_different_lengths:101
             (Map2EOf.fn_e fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* t = M.map_e (uncurry @@ Map2EOf.fn_e fn) leftright in
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let map_s =
    Test.make
      ~name:(Format.asprintf "%s.map{2,}_s" M.name)
      (pair Test_fuzzing_helpers.Fn.arith manymany)
      (fun (Fun (_, fn), (left, right)) ->
        let open Lwt_syntax in
        eq_s
          (M.map2_s
             ~when_different_lengths:101
             (Map2SOf.fn fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* t = M.map_s (uncurry @@ Map2SOf.fn fn) leftright in
           match leftovers with
           | None -> return_ok t
           | Some _ -> return_error 101))

  let map_es =
    Test.make
      ~name:(Format.asprintf "%s.map{2,}_es" M.name)
      (pair Test_fuzzing_helpers.Fn.arith_e manymany)
      (fun (fn, (left, right)) ->
        let open Lwt_result_syntax in
        eq_es
          (M.map2_es
             ~when_different_lengths:101
             (Map2ESOf.fn_e fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* t = M.map_es (uncurry @@ Map2ESOf.fn_e fn) leftright in
           match leftovers with
           | None -> Lwt.return_ok t
           | Some _ -> Lwt.return_error 101))

  let tests_map = [map; map_e; map_s; map_es]

  let rev_map =
    Test.make
      ~name:(Format.asprintf "%s.rev_map{2,}" M.name)
      (pair Test_fuzzing_helpers.Fn.arith manymany)
      (fun (Fun (_, fn), (left, right)) ->
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

  let rev_map_e =
    Test.make
      ~name:(Format.asprintf "%s.rev_map{2,}_e" M.name)
      (pair Test_fuzzing_helpers.Fn.arith_e manymany)
      (fun (fn, (left, right)) ->
        let open Result_syntax in
        eq_e
          (M.rev_map2_e
             ~when_different_lengths:101
             (Map2EOf.fn_e fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* t = M.rev_map_e (uncurry @@ Map2EOf.fn_e fn) leftright in
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let rev_map_s =
    Test.make
      ~name:(Format.asprintf "%s.rev_map{2,}_s" M.name)
      (pair Test_fuzzing_helpers.Fn.arith manymany)
      (fun (Fun (_, fn), (left, right)) ->
        let open Lwt_syntax in
        eq_s
          (M.rev_map2_s
             ~when_different_lengths:101
             (Map2SOf.fn fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* t = M.rev_map_s (uncurry @@ Map2SOf.fn fn) leftright in
           match leftovers with
           | None -> return_ok t
           | Some _ -> return_error 101))

  let rev_map_es =
    Test.make
      ~name:(Format.asprintf "%s.rev_map{2,}_es" M.name)
      (pair Test_fuzzing_helpers.Fn.arith_e manymany)
      (fun (fn, (left, right)) ->
        let open Lwt_result_syntax in
        eq_es
          (M.rev_map2_es
             ~when_different_lengths:101
             (Map2ESOf.fn_e fn)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* t = M.rev_map_es (uncurry @@ Map2ESOf.fn_e fn) leftright in
           match leftovers with
           | None -> Lwt.return_ok t
           | Some _ -> Lwt.return_error 101))

  let tests_rev_map = [rev_map; rev_map_e; rev_map_s; rev_map_es]

  let fold_left =
    Test.make
      ~name:(Format.asprintf "%s.fold_left{2,}" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one manymany)
      (fun (Fun (_, fn), init, (left, right)) ->
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

  let fold_left_e =
    Test.make
      ~name:(Format.asprintf "%s.fold_left{2,}_e" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_e one manymany)
      (fun (fn, init, (left, right)) ->
        let open Result_syntax in
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
           let* t =
             M.fold_left_e (uncurry_l @@ Fold2EOf.fn_e fn) init leftright
           in
           match leftovers with None -> Ok t | Some _ -> Error 101))

  let fold_left_s =
    Test.make
      ~name:(Format.asprintf "%s.fold_left{2,}_s" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one manymany)
      (fun (Fun (_, fn), init, (left, right)) ->
        let open Lwt_syntax in
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
           let* t =
             M.fold_left_s (uncurry_l @@ Fold2SOf.fn fn) init leftright
           in
           match leftovers with
           | None -> return_ok t
           | Some _ -> return_error 101))

  let fold_left_es =
    Test.make
      ~name:(Format.asprintf "%s.fold_left{2,}_es" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_e one manymany)
      (fun (fn, init, (left, right)) ->
        let open Lwt_result_syntax in
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
           let* t =
             M.fold_left_es (uncurry_l @@ Fold2ESOf.fn_e fn) init leftright
           in
           match leftovers with
           | None -> Lwt.return_ok t
           | Some _ -> Lwt.return_error 101))

  let tests_fold_left = [fold_left; fold_left_e; fold_left_s; fold_left_es]

  let fold_right =
    Test.make
      ~name:(Format.asprintf "%s.fold_right{2,}" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one manymany)
      (fun (Fun (_, fn), init, (left, right)) ->
        let open Result_syntax in
        eq_e
          (M.fold_right2
             ~when_different_lengths:101
             (Fold2Of.fn fn)
             (M.of_list left)
             (M.of_list right)
             init)
          (let+ leftright =
             M.combine
               ~when_different_lengths:101
               (M.of_list left)
               (M.of_list right)
           in
           M.fold_right (uncurry_r @@ Fold2Of.fn fn) leftright init))

  let fold_right_e =
    Test.make
      ~name:(Format.asprintf "%s.fold_right{2,}_e" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_e one manymany)
      (fun (fn, init, (left, right)) ->
        let open Result_syntax in
        eq_e
          (M.fold_right2_e
             ~when_different_lengths:101
             (Fold2EOf.fn_e fn)
             (M.of_list left)
             (M.of_list right)
             init)
          (let* leftright =
             M.combine
               ~when_different_lengths:101
               (M.of_list left)
               (M.of_list right)
           in
           M.fold_right_e (uncurry_r @@ Fold2EOf.fn_e fn) leftright init))

  let fold_right_s =
    Test.make
      ~name:(Format.asprintf "%s.fold_right{2,}_s" M.name)
      (triple Test_fuzzing_helpers.Fn.arith one manymany)
      (fun (Fun (_, fn), init, (left, right)) ->
        let open Lwt_result_syntax in
        eq_s
          (M.fold_right2_s
             ~when_different_lengths:101
             (Fold2SOf.fn fn)
             (M.of_list left)
             (M.of_list right)
             init)
          (let* leftright =
             Lwt.return
             @@ M.combine
                  ~when_different_lengths:101
                  (M.of_list left)
                  (M.of_list right)
           in
           Lwt_result.ok
           @@ M.fold_right_s (uncurry_r @@ Fold2SOf.fn fn) leftright init))

  let fold_right_es =
    Test.make
      ~name:(Format.asprintf "%s.fold_right{2,}_es" M.name)
      (triple Test_fuzzing_helpers.Fn.arith_es one manymany)
      (fun (fn, init, (left, right)) ->
        let open Lwt_result_syntax in
        eq_es
          (M.fold_right2_es
             ~when_different_lengths:101
             (Fold2ESOf.fn_es fn)
             (M.of_list left)
             (M.of_list right)
             init)
          (let* leftright =
             Lwt.return
             @@ M.combine
                  ~when_different_lengths:101
                  (M.of_list left)
                  (M.of_list right)
           in
           M.fold_right_es (uncurry_r @@ Fold2ESOf.fn_es fn) leftright init))

  let tests_fold_right = [fold_right; fold_right_e; fold_right_s; fold_right_es]

  let for_all =
    Test.make
      ~name:(Format.asprintf "%s.for_all{2,}" M.name)
      (pair Test_fuzzing_helpers.Fn.pred manymany)
      (fun (pred, (left, right)) ->
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
           | (false, _) -> Ok false
           | (true, None) -> Ok true
           | (true, Some _) -> Error 101))

  let for_all_e =
    Test.make
      ~name:(Format.asprintf "%s.for_all{2,}_e" M.name)
      (pair Test_fuzzing_helpers.Fn.pred manymany)
      (fun (pred, (left, right)) ->
        let open Result_syntax in
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
           let* t = M.for_all_e (uncurry @@ Cond2EOf.fn pred) leftright in
           match (t, leftovers) with
           | (false, _) -> Ok false
           | (true, None) -> Ok true
           | (true, Some _) -> Error 101))

  let for_all_s =
    Test.make
      ~name:(Format.asprintf "%s.for_all{2,}_s" M.name)
      (pair Test_fuzzing_helpers.Fn.pred manymany)
      (fun (pred, (left, right)) ->
        let open Lwt_syntax in
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
           let+ t = M.for_all_s (uncurry @@ Cond2SOf.fn pred) leftright in
           match (t, leftovers) with
           | (false, _) -> Ok false
           | (true, None) -> Ok true
           | (true, Some _) -> Error 101))

  let for_all_es =
    Test.make
      ~name:(Format.asprintf "%s.for_all{2,}_es" M.name)
      (pair Test_fuzzing_helpers.Fn.pred manymany)
      (fun (pred, (left, right)) ->
        let open Lwt_result_syntax in
        eq_es
          (M.for_all2_es
             ~when_different_lengths:101
             (Cond2ESOf.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* t = M.for_all_es (uncurry @@ Cond2ESOf.fn pred) leftright in
           match (t, leftovers) with
           | (false, _) -> Lwt.return_ok false
           | (true, None) -> Lwt.return_ok true
           | (true, Some _) -> Lwt.return_error 101))

  let tests_for_all = [for_all; for_all_e; for_all_s; for_all_es]

  let exists =
    Test.make
      ~name:(Format.asprintf "%s.exists{2,}" M.name)
      (pair Test_fuzzing_helpers.Fn.pred manymany)
      (fun (pred, (left, right)) ->
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
           | (true, _) -> Ok true
           | (false, None) -> Ok false
           | (false, Some _) -> Error 101))

  let exists_e =
    Test.make
      ~name:(Format.asprintf "%s.exists{2,}_e" M.name)
      (pair Test_fuzzing_helpers.Fn.pred manymany)
      (fun (pred, (left, right)) ->
        let open Result_syntax in
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
           let* t = M.exists_e (uncurry @@ Cond2EOf.fn pred) leftright in
           match (t, leftovers) with
           | (true, _) -> Ok true
           | (false, None) -> Ok false
           | (false, Some _) -> Error 101))

  let exists_s =
    Test.make
      ~name:(Format.asprintf "%s.exists{2,}_s" M.name)
      (pair Test_fuzzing_helpers.Fn.pred manymany)
      (fun (pred, (left, right)) ->
        let open Lwt_syntax in
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
           let+ t = M.exists_s (uncurry @@ Cond2SOf.fn pred) leftright in
           match (t, leftovers) with
           | (true, _) -> Ok true
           | (false, None) -> Ok false
           | (false, Some _) -> Error 101))

  let exists_es =
    Test.make
      ~name:(Format.asprintf "%s.exists{2,}_es" M.name)
      (pair Test_fuzzing_helpers.Fn.pred manymany)
      (fun (pred, (left, right)) ->
        let open Lwt_result_syntax in
        eq_es
          (M.exists2_es
             ~when_different_lengths:101
             (Cond2ESOf.fn pred)
             (M.of_list left)
             (M.of_list right))
          (let (leftright, leftovers) =
             M.combine_with_leftovers (M.of_list left) (M.of_list right)
           in
           let* t = M.exists_es (uncurry @@ Cond2ESOf.fn pred) leftright in
           match (t, leftovers) with
           | (true, _) -> Lwt.return_ok true
           | (false, None) -> Lwt.return_ok false
           | (false, Some _) -> Lwt.return_error 101))

  let tests_exists = [exists; exists_e; exists_s; exists_es]

  let tests =
    tests_iter @ tests_map @ tests_rev_map @ tests_fold_left @ tests_fold_right
    @ tests_for_all @ tests_exists
end
