(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Lang_core
open Lang_stdlib

module type PARAMETERS = sig
  val variant : Variants.t

  val width : int

  val nb_full_rounds : int

  val nb_partial_rounds : int

  val nb_skips : int

  val mds_matrix : string array array

  val round_constants : string array

  val partial_round_idx_to_permute : int
end

module Make (PP : PARAMETERS) (L : LIB) = struct
  open PP
  open L
  module Poly = Polynomial.MakeUnivariate (S)

  type poly = Poly.t

  (* Map between scalar variables and an identifier used as degree in their
     polynomial representation. Identifiers are guarateed to be unique. *)
  module Map : sig
    val init : unit -> unit

    val degree_of_wire : scalar repr -> int

    val wire_of_degree : int -> scalar repr
  end = struct
    let assoc : ((scalar repr * int) list * int) ref = ref ([], 1)

    let init () = assoc := ([], 1)

    let degree_of_wire w =
      List.find_opt (fun (w', _d) -> eq w w') (fst !assoc) |> function
      | Some (_, d) -> d
      | None ->
          let l, d = !assoc in
          let l = (w, d) :: l in
          assoc := (l, d + 1) ;
          d

    let wire_of_degree d =
      List.find_opt (fun (_w, d') -> d = d') (fst !assoc) |> function
      | Some (w, _) -> w
      | None -> assert false
  end

  type polys = poly array

  let initial_state = Array.init width (fun _ -> Poly.zero)

  let print_state state =
    Array.iter (fun p -> Format.printf "%s\n" (Poly.to_string p)) state ;
    Format.printf "\n"

  let mds_matrix = mds_matrix |> Array.map (Array.map S.of_string)

  let round_constants = round_constants |> Array.map S.of_string

  (* Linear composition trick:

     - Core idea: The composition of two linear functions is a linear function.

     - Application to Poseidon:
       Partial rounds are linear functions for (width-1) elements of the state.
       We can compose two or more partial rounds (configured with [nb_skips])
       before calculating (creating wires for) the actual value of the
       elements that are linearly afected.

     - Execution:
       We store pending linear computations as polynomials, whose monomials
       represent the wires that need to be linearly combined.

       The i-th wire is represented by monomial x^{i+1}.
       The independent term x^{0} is reserved for the constant ~qc if any. *)

  (* A polynomial represents a linear combination of wires (and a constant).
     The i-th wire is associated to monomial x^{i+1} so that wire 0 is not in
     conflict with the constant (which is stored in the independent term) *)
  let poly_of_wire w = Poly.of_coefficients [(S.one, Map.degree_of_wire w)]

  let wire_of_poly p =
    with_label ~label:"Poseidon.wire_of_poly"
    @@
    let coeffs = Poly.get_list_coefficients p |> List.rev in
    let qc, ws =
      match coeffs with (qc, 0) :: l -> (qc, l) | l -> (S.zero, l)
    in
    let combined_lists =
      List.map (fun (q, d) -> (q, Map.wire_of_degree d)) ws
    in
    let coeffs, xs = List.split combined_lists in
    Num.add_list ~qc ~coeffs (to_list xs)

  let s_box p : poly t =
    with_label ~label:"Poseidon.s_box"
    @@
    if Poly.is_constant p then ret Poly.(p * p * p * p * p)
    else
      let open Num in
      let* x = wire_of_poly p in
      let* x5 = pow5 x in
      ret @@ poly_of_wire x5

  let rec repeat : n:int -> ('a -> 'a t) -> 'a -> 'a t =
   fun ~n f e ->
    if n <= 0 then ret e
    else
      let* x = f e in
      repeat ~n:(n - 1) f x

  let state_map : polys -> (poly -> poly t) -> polys t =
   fun state f ->
    with_label ~label:"Poseidon.state_map"
    @@
    let rec aux : polys -> int -> polys t =
     fun state j ->
      if j = width then ret state
      else
        let* p = f state.(j) in
        state.(j) <- p ;
        aux state (j + 1)
    in
    aux state 0

  (* Simplify the state into single-monomial polynomials by evaluating
     pending the linear combination that they store *)
  let checkpoint : polys -> polys t =
   fun state ->
    with_label ~label:"Poseidon.checkpoint"
    @@
    let f p =
      if Poly.is_constant p then ret p
      else
        let* w = wire_of_poly p in
        ret @@ poly_of_wire w
    in
    state_map state f

  let apply_matrix state =
    let x = Array.copy state in
    for j = 0 to width - 1 do
      state.(j) <- Poly.zero ;
      for i = 0 to width - 1 do
        state.(j) <- Poly.(state.(j) + mult_by_scalar mds_matrix.(j).(i) x.(i))
      done
    done ;
    state

  let apply_round_key (state, i_round_key) =
    for i = 0 to width - 1 do
      state.(i) <-
        Poly.add state.(i) (Poly.constants @@ round_constants.(i_round_key + i))
    done ;
    (state, i_round_key + width)

  let full_round : ?skip_ark:bool -> polys * int -> (polys * int) t =
   fun ?(skip_ark = false) (state, i_round_key) ->
    with_label ~label:"Poseidon.full_round"
    @@ let* state = state_map state s_box in
       let state = apply_matrix state in
       if skip_ark then ret (state, i_round_key)
       else ret @@ apply_round_key (state, i_round_key)

  let full_round128 : ?skip_ark:bool -> polys * int -> (polys * int) t =
   fun ?(skip_ark = false) (state, i_round_key) ->
    assert (width = 3) ;
    with_label ~label:"Poseidon.full_round128"
    @@ let* state = checkpoint state in
       let* x0 = wire_of_poly state.(0) in
       let* x1 = wire_of_poly state.(1) in
       let* x2 = wire_of_poly state.(2) in
       let k = [|S.zero; S.zero; S.zero|] in
       if not skip_ark then
         for i = 0 to width - 1 do
           k.(i) <- round_constants.(i_round_key + i)
         done ;
       let* output =
         Poseidon.poseidon128_full_round
           ~matrix:mds_matrix
           ~k
           ~variant
           (x0, x1, x2)
       in
       (match of_list output with
       | [y0; y1; y2] ->
           state.(0) <- poly_of_wire y0 ;
           state.(1) <- poly_of_wire y1 ;
           state.(2) <- poly_of_wire y2
       | _ -> assert false) ;
       ret @@ (state, i_round_key + width)

  let partial_round : batch:int -> polys * int -> (polys * int) t =
   fun ~batch (state, i_round_key) ->
    with_label ~label:"Poseidon.partial_round"
    @@
    let f (state, i_round_key) =
      let* p = s_box state.(partial_round_idx_to_permute) in
      state.(partial_round_idx_to_permute) <- p ;
      let state = apply_matrix state in
      ret @@ apply_round_key (state, i_round_key)
    in
    let* state, i_round_key = repeat ~n:batch f (state, i_round_key) in
    let* state = checkpoint state in
    ret (state, i_round_key)

  let partial_round128 : batch:int -> polys * int -> (polys * int) t =
   fun ~batch (state, i_round_key) ->
    assert (width = 3) ;
    assert (batch = 4) ;
    with_label ~label:"Poseidon.partial_round128"
    @@ let* state = checkpoint state in
       let* x0 = wire_of_poly state.(0) in
       let* x1 = wire_of_poly state.(1) in
       let* x2 = wire_of_poly state.(2) in
       let ks = Array.make_matrix width batch S.zero in
       for j = 0 to batch - 1 do
         for i = 0 to width - 1 do
           ks.(i).(j) <- round_constants.(i_round_key + (width * j) + i)
         done
       done ;
       let* output =
         Poseidon.poseidon128_four_partial_rounds
           ~matrix:mds_matrix
           ~ks
           ~variant
           (x0, x1, x2)
       in
       (match of_list output with
       | [y0; y1; y2] ->
           state.(0) <- poly_of_wire y0 ;
           state.(1) <- poly_of_wire y1 ;
           state.(2) <- poly_of_wire y2
       | _ -> assert false) ;
       ret @@ (state, i_round_key + (batch * width))

  let apply_permutation (state, _) =
    with_label ~label:"Poseidon.apply_permutation"
    @@
    let batch = nb_skips + 1 in

    let full = if width = 3 then full_round128 else full_round in
    let partial =
      if width = 3 && batch = 4 then partial_round128 else partial_round
    in

    let state, i_round_key = apply_round_key (state, 0) in
    let* state = repeat ~n:(nb_full_rounds / 2) full (state, i_round_key) in
    let* state = repeat ~n:(nb_partial_rounds / batch) (partial ~batch) state in
    let* state = partial_round ~batch:(nb_partial_rounds mod batch) state in
    let* state = repeat ~n:((nb_full_rounds / 2) - 1) full state in
    full ~skip_ark:true state

  let prepare_block with_padding (state, blocks_read) r nb_chunks inputs =
    let block_size =
      if blocks_read < nb_chunks - 1 then width - 1
      else if with_padding then r
      else Array.length inputs - (blocks_read * (width - 1))
    in
    let offset = blocks_read * (width - 1) in

    for j = 0 to block_size - 1 do
      let p = Poly.add state.(j + 1) @@ poly_of_wire inputs.(offset + j) in
      state.(j + 1) <- p
    done ;
    if blocks_read = nb_chunks - 1 && with_padding then
      state.(r + 1) <- Poly.add state.(r + 1) Poly.one ;
    (state, blocks_read + 1)

  let digest : ?input_length:int -> scalar list repr -> scalar repr t =
   fun ?input_length inputs ->
    Map.init () ;
    let inputs = Array.of_list @@ of_list inputs in
    let l = Array.length inputs in
    let assert_length expected =
      let error_msg =
        Format.sprintf "digest expects data of length %d, %d given" expected l
      in
      if l <> expected then raise @@ Invalid_argument error_msg
    in
    Option.iter assert_length input_length ;
    let with_padding = Option.is_none input_length in

    let polys = Array.init width (fun _ -> Poly.zero) in

    let nb_blocks = ((l - if with_padding then 0 else 1) / (width - 1)) + 1 in
    let r = l mod (width - 1) in
    with_label ~label:"Poseidon.digest"
    @@
    let block_iteration (state, blocks_read, i_round_key) =
      let state, blocks_read =
        prepare_block with_padding (state, blocks_read) r nb_blocks inputs
      in
      let* state, i_round_key = apply_permutation (state, i_round_key) in
      ret (state, blocks_read, i_round_key)
    in
    let* state, _, _ = repeat ~n:nb_blocks block_iteration (polys, 0, 0) in
    wire_of_poly state.(1)
end

module Poseidon128 = struct
  module P : Hash_sig.P_HASH = struct
    module H = Mec.Hash.Poseidon128.Make (S)
    include H.Hash

    let direct ?input_length inputs =
      let ctx = init ?input_length () in
      let ctx = digest ctx inputs in
      get ctx
  end

  module V : Hash_sig.HASH = Make (struct
    let variant = Variants.P128

    let width = 3

    let nb_full_rounds = 8

    let nb_partial_rounds = 56

    let nb_skips = 3

    let mds_matrix = Mds_128.v

    let round_constants = Ark_128.v

    let partial_round_idx_to_permute = 2
  end)
end

module Poseidon252 = struct
  module P : Hash_sig.P_HASH = struct
    module H = Mec.Hash.Poseidon252.Make (S)
    include H.Hash

    let direct ?input_length inputs =
      let ctx = init ?input_length () in
      let ctx = digest ctx inputs in
      get ctx
  end

  module V : Hash_sig.HASH = Make (struct
    let variant = Variants.P252

    let width = 5

    let nb_full_rounds = 8

    let nb_partial_rounds = 59

    let nb_skips = 4

    let mds_matrix = Mds_252.v

    let round_constants = Ark_252.v

    let partial_round_idx_to_permute = 4
  end)
end

module PoseidonFull = struct
  module P : Hash_sig.P_HASH = struct
    module H = Mec.Hash.Neptunus.Make (S)
    include H.Hash

    let direct ?input_length inputs =
      let ctx = init ?input_length () in
      let ctx = digest ctx inputs in
      get ctx
  end

  module V : Hash_sig.HASH = Make (struct
    let variant = Variants.PFull128

    let width = 3

    let nb_full_rounds = 60

    let nb_partial_rounds = 0

    let nb_skips = 0

    let mds_matrix = Mds_full.v

    let round_constants = Ark_full.v

    let partial_round_idx_to_permute = 0
  end)
end
