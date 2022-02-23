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

open Lwt.Infix

module type GEN = sig
  type 'a t

  val up : int -> int t

  val down : int -> int t
end

module SeqGen = struct
  include Support.Lib.Seq

  let rec down n : int t =
   fun () -> if n < 0 then Nil else Cons (n, down (pred n))

  let rec up n i : int t =
   fun () -> if i > n then Nil else Cons (i, up n (succ i))

  let up n = up n 0
end

module ListGen = struct
  include Support.Lib.List

  let rec down n : int t = if n < 0 then [] else n :: down (pred n)

  let rec up n i : int t = if i > n then [] else i :: up n (succ i)

  let up n = up n 0
end

module Testing = struct
  exception Nope of int

  module Prn = struct
    let int = string_of_int

    let res f g = function
      | Ok o -> "ok(" ^ f o ^ ")"
      | Error e -> "error(" ^ g e ^ ")"

    let unit _ = "()"

    let t _ _ = "T"
  end

  module Iter = struct
    let e n m = if n = m then Error m else Ok ()

    let es n m = Lwt.return @@ if n = m then Error m else Ok ()

    let exn_es n m = Lwt.return_ok @@ if n = m then raise (Nope m) else ()

    let exn_now _ = raise (Nope 2048)

    let prn = Prn.(res unit int)

    let eq a b = Assert.equal ~prn a b

    let eq_s a b = b >|= fun b -> Assert.equal ~prn a b

    let eq_s_catch a b =
      Lwt.catch
        (fun () -> b () >>= Lwt.return_ok)
        (function Nope d -> Lwt.return_error d | exc -> raise exc)
      >|= fun rb -> Assert.equal ~prn:Prn.(res prn int) a rb
  end

  module Folder = struct
    let e n _acc m = if n = m then Error m else Ok m

    let es n _acc m = Lwt.return @@ if n = m then Error m else Ok m

    let exn_es n _acc m = Lwt.return_ok @@ if n = m then raise (Nope m) else m

    let exn_now _ _ = raise (Nope 2048)

    let prn = Prn.(res int int)

    let eq a b = Assert.equal ~prn a b

    let eq_s a b = b >|= fun b -> Assert.equal ~prn a b

    let eq_s_catch a b =
      Lwt.catch
        (fun () -> b () >>= Lwt.return_ok)
        (function Nope d -> Lwt.return_error d | exc -> raise exc)
      >|= fun rb -> Assert.equal ~prn:Prn.(res prn int) a rb
  end

  (* NOTE: the functor is necessary to avoid a type escaping its scope latter on *)
  module Mapper (G : GEN) = struct
    let e n m = if n = m then Error m else Ok (m + 1000)

    let es n m = Lwt.return @@ if n = m then Error m else Ok (m + 1000)

    let exn_es n m = Lwt.return_ok @@ if n = m then raise (Nope m) else m + 1000

    let exn_now _ = raise (Nope 2048)

    let prn : ('a G.t, int) result -> string = Prn.(res (t int) int)

    let eq a b = Assert.equal ~prn a b

    let eq_s a b = b >|= fun b -> Assert.equal ~prn a b

    let eq_s_catch a b =
      Lwt.catch
        (fun () -> b () >>= Lwt.return_ok)
        (function Nope d -> Lwt.return_error d | exc -> raise exc)
      >|= fun rb -> Assert.equal ~prn:Prn.(res prn int) a rb
  end
end

module MakeItererTest (M : sig
  include GEN

  include Traits.ITER_SEQUENTIAL with type 'a elt := 'a and type 'a t := 'a t
end) =
struct
  open M
  open Testing.Iter

  let test_fail_early _ _ =
    (* error with error *)
    eq (Error 3) @@ iter_e (e 3) (up 100) ;
    (* lwt with exception *)
    (eq_s_catch (Error 4) @@ fun () -> iter_es (exn_es 4) (up 100))
    >>= fun () ->
    (* lwt with immediate exception *)
    (eq_s_catch (Error 2048) @@ fun () -> iter_es exn_now (up 100))
    >>= fun () ->
    (* error-lwt with exception *)
    (eq_s_catch (Error 5) @@ fun () -> iter_es (exn_es 5) (up 100))
    >>= fun () ->
    (* error-lwt with immediate exception *)
    (eq_s_catch (Error 2048) @@ fun () -> iter_es exn_now (up 100))
    >>= fun () ->
    (* error-lwt with error *)
    eq_s (Error 6) @@ iter_es (es 6) (up 100) >>= fun () -> Lwt.return_unit

  let test_has_side_effects _ _ =
    let witness = ref 0 in
    (* vanilla, uninterrupted iter *)
    iter (fun _ -> incr witness) (up 10) ;
    Assert.equal ~msg:"vanilla iter" ~prn:Testing.Prn.int 11 !witness ;
    (* error interrupted iter *)
    let ie =
      iter_e
        (fun m ->
          incr witness ;
          e 10 m)
        (up 23)
    in
    (match ie with
    | Error n ->
        Assert.equal
          ~msg:"unexpected error in result iter"
          ~prn:Testing.Prn.int
          10
          n ;
        Assert.equal ~msg:"result iter" ~prn:Testing.Prn.int 22 !witness
    | Ok () -> Assert.equal ~msg:"unexpected success in result iter" true false) ;
    (* lwt-error interrupted iter *)
    iter_es
      (fun m ->
        incr witness ;
        es 10 m)
      (up 29)
    >|= function
    | Error n ->
        Assert.equal
          ~msg:"unexpected error in lwt-result iter"
          ~prn:Testing.Prn.int
          10
          n ;
        Assert.equal ~msg:"lwt-result iter" ~prn:Testing.Prn.int 33 !witness
    | Ok () ->
        Assert.equal ~msg:"unexpected success in lwt-result iter" true false

  let tests =
    [
      Alcotest_lwt.test_case "fail-early" `Quick test_fail_early;
      Alcotest_lwt.test_case "has-side-effects" `Quick test_has_side_effects;
    ]
end

module SeqIterTest = MakeItererTest (SeqGen)
module ListIterTest = MakeItererTest (ListGen)

module MakeFolderTest (M : sig
  include GEN

  include
    Traits.FOLDLEFT_SEQUENTIAL with type 'a elt := 'a and type 'a t := 'a t
end) =
struct
  open M
  open Testing.Folder

  (* test that all sequential operators fail-early *)
  let test_fail_early _ _ =
    (* error with error *)
    eq (Error 3) @@ fold_left_e (e 3) (-10) (up 100) ;
    (* lwt with exception *)
    (eq_s_catch (Error 4) @@ fun () -> fold_left_es (exn_es 4) (-10) (up 100))
    >>= fun () ->
    (* lwt with immediate exception *)
    (eq_s_catch (Error 2048) @@ fun () -> fold_left_es exn_now (-10) (up 100))
    >>= fun () ->
    (* error-lwt with exception *)
    (eq_s_catch (Error 5) @@ fun () -> fold_left_es (exn_es 5) (-10) (up 100))
    >>= fun () ->
    (* error-lwt with immediate exception *)
    (eq_s_catch (Error 2048) @@ fun () -> fold_left_es exn_now (-10) (up 100))
    >>= fun () ->
    (* error-lwt with error *)
    eq_s (Error 6) @@ fold_left_es (es 6) (-10) (up 100) >>= fun () ->
    Lwt.return_unit

  let tests = [Alcotest_lwt.test_case "fail-early" `Quick test_fail_early]
end

module SeqFoldTest = MakeFolderTest (SeqGen)
module ListFoldTest = MakeFolderTest (ListGen)

module MakeMapperTest (M : sig
  include GEN

  include Traits.MAP_SEQUENTIAL with type 'a t := 'a t
end) =
struct
  open M

  open Testing.Mapper (M)

  (* test that all sequential operators fail-early *)
  let test_fail_early _ _ =
    (* error with error *)
    eq (Error 3) @@ map_e (e 3) (up 100) ;
    (* lwt with exception *)
    (eq_s_catch (Error 4) @@ fun () -> map_es (exn_es 4) (up 100)) >>= fun () ->
    (* lwt with immediate exception *)
    (eq_s_catch (Error 2048) @@ fun () -> map_es exn_now (up 100)) >>= fun () ->
    (* error-lwt with exception *)
    (eq_s_catch (Error 5) @@ fun () -> map_es (exn_es 5) (up 100)) >>= fun () ->
    (* error-lwt with immediate exception *)
    (eq_s_catch (Error 2048) @@ fun () -> map_es exn_now (up 100)) >>= fun () ->
    (* error-lwt with error *)
    eq_s (Error 6) @@ map_es (es 6) (up 100) >>= fun () -> Lwt.return_unit

  let tests = [Alcotest_lwt.test_case "fail-early" `Quick test_fail_early]
end

let flip_e seq_e =
  let open Support.Lib.Monad.Result_syntax in
  let+ r = Support.Lib.Seq_e.fold_left (fun acc item -> item :: acc) [] seq_e in
  List.to_seq (List.rev r)

let flip_s seq_s =
  let open Support.Lib.Monad.Lwt_syntax in
  let+ r = Support.Lib.Seq_s.fold_left (fun acc item -> item :: acc) [] seq_s in
  List.to_seq (List.rev r)

let flip_es seq_es =
  let open Support.Lib.Monad.Lwt_result_syntax in
  let+ r =
    Support.Lib.Seq_es.fold_left (fun acc item -> item :: acc) [] seq_es
  in
  List.to_seq (List.rev r)

module SeqMapTest = MakeMapperTest (struct
  include SeqGen

  let map_e f seq = flip_e @@ Support.Lib.Seq_e.(map_e f @@ of_seq seq)

  let map_s f seq = flip_s @@ Support.Lib.Seq_s.(map_s f @@ of_seq seq)

  let map_es f seq = flip_es @@ Support.Lib.Seq_es.(map_es f @@ of_seq seq)
end)

module ListMapTest = MakeMapperTest (ListGen)

let () =
  Alcotest_lwt.run
    "traversor-generic"
    [
      ("seq-iter", SeqIterTest.tests);
      ("seq-fold", SeqFoldTest.tests);
      ("seq-map", SeqMapTest.tests);
      ("list-iter", ListIterTest.tests);
      ("list-fold", ListFoldTest.tests);
      ("list-map", ListMapTest.tests);
    ]
  |> Lwt_main.run
