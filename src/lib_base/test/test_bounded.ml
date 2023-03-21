(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(* Testing
   -------
   Component:    Base
   Invocation:   dune exec src/lib_base/test/main.exe
   Subject:      Test the [Bounded] module.
*)

module Helpers : sig
  (** Generators of non-empty bounded intervals. *)

  module type S = sig
    (** The signature is extended with a generator and helpers for debugging tests. *)
    include Bounded.S

    val t : ocaml_type Bounded.Internal_for_tests.t

    val pp_ocaml_type : Format.formatter -> ocaml_type -> unit

    (** [gen] generates values [v] such that [S.min_value <= v <= S.max_value]. *)
    val gen : ocaml_type QCheck2.Gen.t
  end

  (** A value represent a bounded interval with an element in it. *)
  type value =
    | E' : {
        bounded : (module S with type ocaml_type = 'a);
        element : 'a;
      }
        -> value

  (** [print] can be used for debugging *)
  val print : value -> string

  (** A generator for [value] elements. *)
  val gen : value QCheck2.Gen.t
end = struct
  module type PARAMETERS = sig
    type t

    val rem : t -> t -> t

    val add : t -> t -> t

    val max : t -> t -> t

    val sub : t -> t -> t

    val abs : t -> t

    val one : t
  end

  module type S = sig
    include Bounded.S

    val t : ocaml_type Bounded.Internal_for_tests.t

    val pp_ocaml_type : Format.formatter -> ocaml_type -> unit

    val gen : ocaml_type QCheck2.Gen.t
  end

  (* The complexity of this function comes from a limitation of QCheck
     to provide ranged generators for datatypes such as int64,
     int32... and could be useful if the module Bounded is exported with
     other data types. *)
  let bounded_gen (type ty)
      (module B : Bounded.BOUNDS with type ocaml_type = ty)
      (module Parameters : PARAMETERS with type t = ty) basic_generator =
    let open Parameters in
    let open QCheck2.Gen in
    let* n = basic_generator in
    let size = max one (add (sub B.max_value B.min_value) one) in
    (* We use [abs] to ensure the value returned by [rem] is
       positive. The value [unsigned_rem] does not exist for module
       [Int]. *)
    return (add B.min_value (rem (abs n) size))

  (* This function is horrendously verbose because of the use of first
     class modules. There might be a way to rewrite this function in a
     less verbose manner. The purpose of this function is given a
     specification like [Uint16], to compute a generator for a bounded
     interval.

     The function is completely parametric so that it should be "easy"
     to extend. Easy meaning following the OCaml type checker. *)
  let gen_from_ty (type ty) :
      ty Bounded.Internal_for_tests.t -> (module S) QCheck2.Gen.t =
    let open QCheck2.Gen in
    fun (type a) (ty : a Bounded.Internal_for_tests.t) ->
      let (module Compare : Tezos_stdlib.Compare.S with type t = a) =
        Bounded.Internal_for_tests.compare ty
      in
      let min_value = Bounded.Internal_for_tests.min_value ty in
      let max_value = Bounded.Internal_for_tests.max_value ty in
      let gen : a t =
        match ty with
        | Int64 -> int64
        | Int32 -> int32
        | Int31 -> int_range min_value max_value
        | Int16 -> int_range min_value max_value
        | Uint16 -> int_range min_value max_value
        | Int8 -> int_range min_value max_value
        | Uint8 -> int_range min_value max_value
      in
      let* a = gen in
      let* b = gen in
      let min_value, max_value = if Compare.(a < b) then (a, b) else (b, a) in
      let bounds : (module Bounded.BOUNDS with type ocaml_type = a) =
        (module struct
          type ocaml_type = a

          let min_value = min_value

          let max_value = max_value
        end)
      in
      let parameters : (module PARAMETERS with type t = a) =
        match ty with
        | Int64 -> (module Int64)
        | Int32 -> (module Int32)
        | Int31 -> (module Int)
        | Int16 -> (module Int)
        | Uint16 -> (module Int)
        | Int8 -> (module Int)
        | Uint8 -> (module Int)
      in
      let bounded_gen = bounded_gen bounds parameters gen in

      let gen : (module S) =
        (* Because of the GADT, the code below cannot be factored easily. *)
        match (ty : a Bounded.Internal_for_tests.t) with
        | Int64 ->
            let (module Bounds) = bounds in
            (module struct
              type ocaml_type = int64

              include Bounded.Int64 (Bounds)

              let t = Bounded.Internal_for_tests.Int64

              let pp_ocaml_type fmt v = Format.fprintf fmt "%Ld" v

              let gen = bounded_gen
            end)
        | Int32 ->
            let (module Bounds) = bounds in
            (module struct
              type ocaml_type = int32

              include Bounded.Int32 (Bounds)

              let t = Bounded.Internal_for_tests.Int32

              let pp_ocaml_type fmt v = Format.fprintf fmt "%ld" v

              let gen = bounded_gen
            end)
        | Int31 ->
            let (module Bounds) = bounds in
            (module struct
              type ocaml_type = int

              include Bounded.Int31 (Bounds)

              let t = Bounded.Internal_for_tests.Int31

              let pp_ocaml_type fmt v = Format.fprintf fmt "%d" v

              let gen = bounded_gen
            end)
        | Int16 ->
            let (module Bounds) = bounds in
            (module struct
              type ocaml_type = int

              include Bounded.Int16 (Bounds)

              let t = Bounded.Internal_for_tests.Int16

              let pp_ocaml_type fmt v = Format.fprintf fmt "%d" v

              let gen = bounded_gen
            end)
        | Uint16 ->
            let (module Bounds) = bounds in
            (module struct
              type ocaml_type = int

              include Bounded.Uint16 (Bounds)

              let t = Bounded.Internal_for_tests.Uint16

              let pp_ocaml_type fmt v = Format.fprintf fmt "%d" v

              let gen = bounded_gen
            end)
        | Int8 ->
            let (module Bounds) = bounds in
            (module struct
              type ocaml_type = int

              include Bounded.Int8 (Bounds)

              let t = Bounded.Internal_for_tests.Int8

              let pp_ocaml_type fmt v = Format.fprintf fmt "%d" v

              let gen = bounded_gen
            end)
        | Uint8 ->
            let (module Bounds) = bounds in
            (module struct
              type ocaml_type = int

              include Bounded.Uint8 (Bounds)

              let t = Bounded.Internal_for_tests.Uint8

              let pp_ocaml_type fmt v = Format.fprintf fmt "%d" v

              let gen = bounded_gen
            end)
      in
      return gen

  type ty = E : _ Bounded.Internal_for_tests.t -> ty

  type value =
    | E' : {
        bounded : (module S with type ocaml_type = 'a);
        element : 'a;
      }
        -> value

  let gen : value QCheck2.Gen.t =
    let open Bounded.Internal_for_tests in
    let open QCheck2.Gen in
    let values =
      [E Int64; E Int32; E Int31; E Int16; E Uint16; E Int8; E Uint8]
    in
    let* ety = oneofl values in
    match ety with
    | E ty ->
        let* (module S : S) = gen_from_ty ty in
        let* element = S.gen in
        return (E' {bounded = (module S); element})

  let print (E' {bounded = (module S); element}) =
    Format.asprintf
      "type(%a) min: %a, max: %a, element: %a"
      Bounded.Internal_for_tests.pp_ty
      S.t
      S.pp_ocaml_type
      S.min_value
      S.pp_ocaml_type
      S.max_value
      S.pp_ocaml_type
      element
end

open Helpers

let roundtrips_json : QCheck2.Test.t =
  QCheck2.Test.make
    ~name:"Bounded: roundtrips in JSON"
    ~print
    gen
    (fun (E' {bounded = (module S); element}) ->
      match S.of_value element with
      | None -> false
      | Some x ->
          let b = Data_encoding.Json.construct S.encoding x in
          let tt = Data_encoding.Json.destruct S.encoding b in
          x = tt)

let roundtrips_binary : QCheck2.Test.t =
  QCheck2.Test.make
    ~name:"Bounded: roundtrips in binary"
    ~print
    gen
    (fun (E' {bounded = (module S); element}) ->
      match S.of_value element with
      | None -> false
      | Some x ->
          let b = Data_encoding.Binary.to_bytes_exn S.encoding x in
          let tt = Data_encoding.Binary.of_bytes_exn S.encoding b in
          x = tt)

let tests = [roundtrips_json; roundtrips_binary]

module Empty = Bounded.Int32 (struct
  let min_value = 1l

  let max_value = 0l
end)

module Small = Bounded.Int32 (struct
  let min_value = 1l

  let max_value = 3l
end)

module Small_with_neg = Bounded.Int32 (struct
  let min_value = -10l

  let max_value = 10l
end)

module Full = Bounded.Int32 (struct
  let min_value = Stdlib.Int32.min_int

  let max_value = Stdlib.Int32.max_int
end)

module Uint16 = Bounded.Uint16 (struct
  let min_value = 0

  let max_value = 32767
end)

let int32_checks =
  let open Alcotest in
  [
    test_case "0 not in empty" `Quick (fun () ->
        assert (Empty.of_value 0l = None));
    test_case "123 not in empty" `Quick (fun () ->
        assert (Empty.of_value 123l = None));
    test_case "Int32.min_int not in empty" `Quick (fun () ->
        assert (Empty.of_value Int32.min_int = None));
    test_case "0 not in Small" `Quick (fun () ->
        assert (Small.of_value 0l = None));
    test_case "1 in Small" `Quick (fun () ->
        assert (Option.map Small.to_value (Small.of_value 1l) = Some 1l));
    test_case "2 in Small" `Quick (fun () ->
        assert (Option.map Small.to_value (Small.of_value 2l) = Some 2l));
    test_case "4 not in Small" `Quick (fun () ->
        assert (Small.of_value 4l = None));
    test_case "0 in full" `Quick (fun () ->
        assert (Option.map Full.to_value (Full.of_value 0l) = Some 0l));
    test_case "123 in full" `Quick (fun () ->
        assert (Option.map Full.to_value (Full.of_value 123l) = Some 123l));
    test_case "Int32.min_int in full" `Quick (fun () ->
        assert (
          Option.map Full.to_value (Full.of_value Int32.min_int)
          = Some Int32.min_int));
    test_case "Int32.max_int in full" `Quick (fun () ->
        assert (
          Option.map Full.to_value (Full.of_value Int32.max_int)
          = Some Int32.max_int));
    test_case "Uint8.bad instantiation lower value" `Quick (fun () ->
        try
          let (_ : (module Bounded.S)) =
            (module struct
              (* A bit hackish but this is due to a limitation of
                 OCaml that not accept substitutions in signatures of
                 first-class modules at the moment. *)
              type ocaml_type = int

              include Bounded.Uint8 (struct
                let min_value = -1

                let max_value = 1
              end)
            end)
          in
          assert false
        with Invalid_argument _ -> ());
    test_case "Uint8.bad instantiation upper value" `Quick (fun () ->
        try
          let (_ : (module Bounded.S)) =
            (module struct
              (* A bit hackish but this is due to a limitation of
                 OCaml that not accept substitutions in signatures of
                 first-class modules at the moment. *)
              type ocaml_type = int

              include Bounded.Uint8 (struct
                let min_value = 0

                let max_value = 256
              end)
            end)
          in
          assert false
        with Invalid_argument _ -> ());
    test_case "Uint16.tight bounds" `Quick (fun () ->
        assert (
          Uint16.of_value (-1) = None
          && Uint16.of_value 32768 = None
          && Uint16.of_value 0 <> None
          && Uint16.of_value 32767 <> None));
  ]

let () =
  Alcotest.run
    "Bounded"
    [
      ("Int32", int32_checks);
      ("Round-trip property", Qcheck2_helpers.qcheck_wrap tests);
    ]
