(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Protocol Library
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/pbt/main.exe \
                  -- --file test_sampler.ml
    Subject:      Operations in Saturation_repr
*)

open Qcheck2_helpers
open Protocol.Sampler

(* ------------------------------------------------------------------------- *)
(* Helpers *)

module Int = struct
  include Int

  let hash = Hashtbl.hash
end

let equal_array elt_eq arr1 arr2 =
  Array.length arr1 = Array.length arr2
  && Stdlib.List.for_all2 elt_eq (Array.to_list arr1) (Array.to_list arr2)

(* Support of a distribution on Z (sorted, with potential duplicates) *)
let support cmp array =
  Array.to_seq array |> Seq.map fst |> List.of_seq |> List.sort cmp
  |> Array.of_list

(* Support of a distribution on Z (sorted, without duplicates) *)
let support_uniq cmp array =
  Array.to_seq array |> Seq.map fst |> List.of_seq |> List.sort_uniq cmp
  |> Array.of_list

module type Std = sig
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val hash : t -> int
end

module Helpers = struct
  let sample_n_times (total : int) sample =
    let rec loop n acc =
      if n = 0 then acc
      else
        let res = sample () in
        loop (n - 1) (res :: acc)
    in
    loop total []

  let empirical_distribution : type a.
      (module Std with type t = a) ->
      nsamples:int ->
      (unit -> a) ->
      (a * int) array =
   fun (module H) ~nsamples sampler ->
    let module Table = Hashtbl.Make (H) in
    let samples = sample_n_times nsamples sampler in
    let table = Table.create 127 in
    List.iter
      (fun sample ->
        let count = Option.value ~default:0 (Table.find table sample) in
        Table.replace table sample (count + 1))
      samples ;
    let result = Table.to_seq table |> Array.of_seq in
    (* check that the support of [result] has no duplicate elements (should
       be true since we use [replace]). *)
    assert (
      equal_array
        H.equal
        (support H.compare result)
        (support_uniq H.compare result)) ;
    result
end

let normalize : ('a * int) array -> ('a * Q.t) array =
 fun empirical ->
  let total =
    Array.fold_left
      (fun acc (_, weight) -> Z.add (Z.of_int weight) acc)
      Z.zero
      empirical
  in
  Array.map (fun (n, weight) -> (n, Q.(Z.of_int weight /// total))) empirical

let pp_dist pp fmtr dist =
  let l = Array.to_list dist in
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
    (fun fmtr (elt, w) -> Format.fprintf fmtr "(%a, %f)" pp elt (Q.to_float w))
    fmtr
    l

let linf (dist : ('a * Q.t) array) pmf =
  Array.fold_left (fun acc (n, q) -> Q.(max acc (abs (pmf n - q)))) Q.zero dist

(* ------------------------------------------------------------------------- *)

let state =
  Random.State.make
    [|
      0x1337533D;
      71287309;
      666932349;
      719132214;
      461480042;
      387006837;
      443018964;
      450865457;
      901711679;
      833353016;
      397060904;
    |]

module Make_test (Mass : sig
  include Internal_for_tests.SMass

  val to_float : t -> float
end) (S : sig
  val sample : int_bound:int -> mass_bound:Mass.t -> int * Mass.t
end) =
struct
  let make p =
    let module Probability = Internal_for_tests.Make (Mass) in
    let measure = List.mapi (fun i p -> (i, p)) p in
    let total_mass = List.fold_left Mass.add Mass.zero p in
    let state = Probability.create measure in
    let sampler = Probability.sample state in
    let empirical =
      normalize
      @@ Helpers.empirical_distribution
           (module Int)
           ~nsamples:5_000_000
           (fun () -> sampler S.sample)
    in
    (* We need to rescale the empirical to match that the total mass is not necessarily one. *)
    let empirical =
      let rescaling = Q.of_float (Mass.to_float total_mass) in
      Array.map (fun (x, q) -> (x, Q.mul q rescaling)) empirical
    in
    (* map the mass to Q to better measure the error *)
    let truth =
      let array =
        measure |> List.to_seq
        |> Seq.map (fun (_, mass) -> Q.of_float (Mass.to_float mass))
        |> Array.of_seq
      in
      fun i -> array.(i)
    in
    let error = linf empirical truth in
    let max_error = 0.001 *. Mass.to_float total_mass in
    if not Q.(error < Q.of_float max_error) then
      QCheck2.Test.fail_reportf
        "didn't converge (%f)@.%a"
        (Q.to_float error)
        (pp_dist Format.pp_print_int)
        empirical ;
    true
end

(* Testing the alias sampler with float-valued measures *)

module Probability_mass_float : Internal_for_tests.SMass with type t = float =
struct
  type t = float

  let encoding = Data_encoding.float

  let zero = 0.0

  let of_int = float_of_int

  let mul = ( *. )

  let add = ( +. )

  let sub = ( -. )

  let ( = ) = Float.equal

  let ( <= ) (x : t) (y : t) = x <= y

  let ( < ) (x : t) (y : t) = x < y
end

module Test_float =
  Make_test
    (struct
      include Probability_mass_float

      let to_float x = x
    end)
    (struct
      let sample ~int_bound ~mass_bound =
        (Random.State.int state int_bound, Random.State.float state mass_bound)
    end)

(* Testing the alias sampler with Z-valued measures *)

module Probability_mass_z : Internal_for_tests.SMass with type t = Z.t = struct
  let encoding = Data_encoding.z

  include Z
  include Z.Compare
end

module Test_z =
  Make_test
    (struct
      include Probability_mass_z

      let to_float = Z.to_float
    end)
    (struct
      let sample ~int_bound ~mass_bound =
        ( Random.State.int state int_bound,
          Z.of_int64 (Random.State.int64 state (Z.to_int64 mass_bound)) )
    end)

let qcheck_wrap = qcheck_wrap ~rand:state

let alias_float_test =
  QCheck2.Test.make
    ~count:100
    ~name:"alias_float"
    QCheck2.Gen.(list_size (int_range 1 20) pfloat)
    Test_float.make

let alias_z_test =
  QCheck2.Test.make
    ~count:100
    ~name:"alias_z"
    QCheck2.Gen.(
      list_size (int_range 1 20) (nat >>= fun n -> return (Z.of_int n)))
    Test_z.make

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [("sampling", qcheck_wrap [alias_float_test; alias_z_test])]
