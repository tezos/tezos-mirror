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

open Csir
open Optimizer_helpers

(** The optimizer simplifies a constraint system, producing an equivalent one
    with fewer constraints in essentially three ways:

    - 1. {!next_gate_selectors} [qlg], [qrg] and/or [qog], which
    lead to a more compact representation of sums (linear combinations).

    - 2. {!shared_wires} among different constraints.

    - 3. {!compacting_blocks} which operate on independent wires.

    {1 Optimization Rules}

    {2:next_gate_selectors Introducing next-gate selectors}

    In a system with 3-wires architecture, adding (in a linear combination) [n]
    elements requires [n-1] constraints. For example, computing
    [out = 1 + 2 x1 + 4 x2 + 8 x3 + 16 x4 + 32 x5] can be implemented in 4
    constraints as follows:
    {[
      { a = x1;   b = x2;   c = aux1; identity = [ql 2; qr 4; qo -1; qc 1] };
      { a = x3;   b = x4;   c = aux2; identity = [ql 8; qr 16; qo -1] };
      { a = x5;   b = aux1; c = aux3; identity = [ql 32; qr 1; qo -1] };
      { a = aux2; b = aux3; c = out;  identity = [ql 1; qr 1; qo -1] };
    ]}
    Instead, by using next-gate selectors, a linear combination of [n] terms
    can be implemented in [(n-1)/2] constraints. In this case, just two:
    {[
      { a = x1; b = x2; c = out; identity = [ql 2; qr 4; qlg 8; qrg 16; qog 32; qo -1; qc 1] };
      { a = x3; b = x4; c = x5;  identity = [] };
    ]}

    {2:shared_wires Reusing shared-wires}

    Remarkably, two linear combinations that involve some common wires can be
    implemented more efficiently if they are handled together.
    For example, [out1 = 5 x + 3 y + 9 z] and [out2 = 2 x - 3 y + 7 t] can be
    implemented with 3 constraints (instead of 4) as follows:
    {[
      { a = z;        c = out1; identity = [ql 9; qlg 5; qrg 3; qog -1] };
      { a = x; b = y; c = out2; identity = [ql 2; qr -3; qrg 7; qog -1] };
      {        b = t;         ; identity = [] };
    ]}
    Furthermore, observe how, conveniently, some wires in the above constraints
    are unbound, which can lead to our next optimization.

    {2:compacting_blocks Compacting blocks}

    Consider a gate which only takes one wire as input, e.g. for computing
    [out3 = 3 w + 1], implemented with constraint:
    {[{ a = w;        c = out3; identity = [ql 3; qo -1; qc 1] }]}
    This constraint can be merged with our previous block, which only uses
    wire b in its last constraint and has unbound selectors for it:
    {[
      { a = z;        c = out1; identity = [ql 9; qlg 5; qrg 3; qog -1] };
      { a = x; b = y; c = out2; identity = [ql 2; qr -3; qrg 7; qog -1] };
      { a = w; b = t; c = out3; identity = [ql 3; qo -1; qc 1] };
    ]}

    {1 Algorithm}

    The optimization proceeds in several steps:

    1. Collect information about all linear constraints/gates (see {!is_linear}),
    including the number of occurrences of their output wire among the circuit.

    2. Inline any {!type:linear_computation} whose output is used only once
    among the circuit, in the computation where it is used (only if the latter
    is also linear).

    3. Transform any non-inlined linear computation into a {!type:pseudo_block}
    and join pseudo blocks that share at least a wire (preferably two wires).

    4. Transform pseudo blocks to actual blocks (also considering the non-linear
    computations that were left aside) and join blocks that operate on
    independent wires together. *)

let nb_wires_arch = Csir.nb_wires_arch

type constr = Csir.CS.raw_constraint

type scalar = Csir.Scalar.t [@@deriving repr]

(* The goal of blocks and pseudo-blocks is to keep together constraints that
   depend on each other, e.g. those using next selectors. *)
type block = constr array

(* coeff * wire *)
type term = scalar * int [@@deriving repr]

(** Type to store the trace_updater information.
    [free_wires] represents a list of indices of wires that have been
    removed from the system and have not been used again for auxiliary
    computations. If a free wire [w] is used to store an auxiliary linear
    computation on some [terms], it is removed from [free_wires] and the pair
    [(v, terms)] is added to the list of [assignments]. *)
type trace_info = {free_wires : int list; assignments : (int * term list) list}
[@@deriving repr]

(** A pseudo constraint represents an equation between an arbitrary number of
    terms. In particular, adding all terms from its head, body, tail and its
    constant term should produce 0.
    The length of pc_head and pc_tail should never exceed [nb_wires_arch]. *)
type pseudo_constr = {
  pc_head : term list;
  pc_body : term list;
  pc_tail : term list;
  pc_const : Scalar.t;
}

(** A pseudo block is a list of pseudo constraints that have been groupped
    in order to apply the {!shared_wires} heuristic.Alcotest
    The constraints in a pseudo block satisfy (by construction) the invariant
    the pc_head of a constraint starts with the pc_tail of the previous one
    (when considering the term variables, coefficients may be different).
    Note that the pc_head may contain extra terms that do not appear in the
    previous pc_tail.
    This invariant is important for [block_of_pseudo_block] to be correct *)
type pseudo_block = pseudo_constr list

(** [join_list ~join scope x xs] joins [x] with another element among the first
    [scope] elements of the list of candidates [xs], or returns [None] if no
    union was possible. This function may not preserve the order in [xs] *)
let join_list :
    join:('a -> 'a -> 'a option) -> int -> 'a -> 'a list -> 'a list option =
 fun ~join scope x xs ->
  let rec aux cnt seen_xs = function
    | [] -> None
    | x' :: xs -> (
        if cnt >= scope then None
        else
          match join x x' with
          | Some union -> Some (List.rev_append seen_xs (union :: xs))
          | None -> (
              match join x' x with
              | Some union -> Some (List.rev_append seen_xs (union :: xs))
              | None -> aux (cnt + 1) (x' :: seen_xs) xs))
  in
  aux 0 [] xs

(** [combine_quadratic ~join xs] takes a list of elements [xs] and returns
    a (potentially) shorter list [xs'] where some of the elements have been
    combined according to [join]. [join x x'] is [None] if [x] and [x'] cannot
    be combined and it is [Some union] otherwise.
    This algorithm tries to combine every element of [xs] with any other element
    by greedily trying all possible [n choose 2] different pairings of elements,
    where [n] is the length of [xs]. *)
let combine_quadratic :
    scope:int -> join:('a -> 'a -> 'a option) -> 'a list -> 'a list =
 fun ~scope ~join groups ->
  let rec aux seen_xs = function
    | [] -> seen_xs
    | x :: xs -> (
        match join_list scope ~join x xs with
        | None -> aux (x :: seen_xs) xs
        | Some xs -> aux seen_xs xs)
  in
  aux [] groups

(** Similar to [combine_quadratic], but more efficient.
    Takes a list of [top_candidates] that will each be tried to be combined
    with [rest]. This function may not preserve the order in [rest] *)
let combine_quadratic_efficient ~scope ~join top_candidates rest =
  let rec aux seen top rest =
    match top with
    | [] -> List.rev_append (combine_quadratic ~scope ~join seen) rest
    | x :: xs -> (
        match join_list scope ~join x rest with
        | None -> aux (x :: seen) xs rest
        | Some rest -> aux seen xs rest)
  in
  aux [] top_candidates rest

(** Concatenates two lists of terms, merging terms with the same wire identifier
    (by adding their coefficients) *)
let add_terms : term list -> term list -> term list =
 fun terms1 terms2 ->
  (* [aux] requires that the terms be first sorted wrt the wire identifier *)
  let rec aux terms (q, w) = function
    | [] -> (q, w) :: terms
    | (q', w') :: rest ->
        if w = w' then aux terms (Scalar.add q q', w) rest
        else aux ((q, w) :: terms) (q', w') rest
  in
  let compare (_, w1) (_, w2) = Int.compare w2 w1 in
  let sorted = List.sort compare (List.rev_append terms1 terms2) in
  match sorted with [] -> [] | _ -> aux [] (List.hd sorted) (List.tl sorted)

(** [inline w ~from ~into linear] takes an array of linear constraints
    (a linear constraint is represented by list of terms), solves for wire
    [w] in constraint [linear.(from)] and substitutes wire [w] by the result
    in constraint [linear.(into)]. [linear.(from)] is left empty.
    This routine should only be called if [w] does not appear in any other
    constraint. In that case, constraint [linear.(from)] can then be removed
    and wire [w] is said to be "free". *)
let inline w ~from ~into linear : unit =
  let equals_w t = w = snd t in
  let (q1, _), ts1 = find_and_remove equals_w linear.(from) in
  let (q2, _), ts2 = find_and_remove equals_w linear.(into) in
  let f (q, w) = (Scalar.(negate q * q2 / q1), w) in
  linear.(from) <- [] ;
  linear.(into) <- add_terms (List.map f ts1) ts2

(** The keys are wire indices and the values lists of constraints indices, such
    that the wire appears in each of the constraints. *)
module IMap = Map.Make (Int)
(* TODO: sometimes for clarity I like to make the type of the values monomorphic
   This module can also be called Wire_occurrences *)

module ISet = Set.Make (Int)

(** Auxiliary function used in [inline_linear].

    [increase_occurrences map i terms] updates [map] by adding [i] to the
    list of occurrences of wire [w] for every [(_, w)] in [terms]. *)
let increase_occurrences map i terms =
  List.fold_left
    (fun map (_, w) ->
      IMap.update w (fun o -> Some (i :: Option.value ~default:[] o)) map)
    map
    terms

(** Creates a pseudo block with all terms in the body except for the wire -1
    which is set as constant. *)
let pseudo_block_of_terms terms =
  (* The list of terms contains no duplicates with respect to the wire
     identifier [w], thanks to function [add_terms]. *)
  let qcs, terms = List.partition (fun (_, w) -> w < 0) terms in
  let qc =
    match qcs with
    | [] -> Scalar.zero
    | [(q, -1)] -> q
    | _ ->
        (* Two different terms should not have the same second component *)
        assert false
  in
  [{pc_head = []; pc_body = terms; pc_tail = []; pc_const = qc}]

(** Returns a list of linear gates that have possibly been inlined, the list of
    wires that were inlined (and thus freed) and the list of unchanged non-linear
    gates. The wires selected for inlining must:
    - appear in exactly two linear gates of size one
    - not be inputs
    The inlined linear gates are returned as pseudo blocks because they may need
    to be split again to fit 3 wires. *)
let inline_linear ~nb_inputs gates =
  let linear, non_linear =
    List.partition_map
      (fun gate ->
        if Array.length gate = 1 && CS.is_linear_raw_constr gate.(0) then
          Left gate.(0)
        else Right gate)
      gates
  in
  let linear = Array.(map CS.linear_terms @@ of_list linear) in

  let non_linear_wires =
    List.concat_map CS.gate_wires non_linear |> ISet.of_list
  in

  (* Associate each wire to the indices (of array [linear]) of the constraints
     where the wire appears. The only wires considered must not:
     - be a circuit input
     - appear in a non-linear gate. *)
  let wire_occurrences =
    Array.fold_left
      (fun (map, i) gate -> (increase_occurrences map i gate, succ i))
      (IMap.empty, 0)
      linear
    |> fst
    |> IMap.filter (fun w _ ->
           w >= nb_inputs && (not @@ ISet.mem w non_linear_wires))
  in

  (* During the process of inlining, some wire occurrences will point to
     constraints that have been removed (inlined).
     In [pointers] we carry the information of where they were inlined into. *)
  let pointers = ref IMap.empty in
  let rec updated_pointer occ =
    match IMap.find_opt occ !pointers with
    | Some p ->
        let p' = updated_pointer p in
        (* Create a direct connection between [occ] and [p'] *)
        pointers := IMap.add occ p' !pointers ;
        p'
    | None -> occ
  in

  (* If a wire occurs only on two constraints, we remove one
     of them, "inlining" it in the other, the wire disappears *)
  let free_wires, removable_constrs =
    IMap.fold
      (fun w occs (free_wires, removable_constrs) ->
        match occs with
        | [occ] ->
            (* We could raise an exception saying that wire [w] is unnecessary *)
            let occ = updated_pointer occ in
            (w :: free_wires, occ :: removable_constrs)
        | [occ1; occ2] ->
            let from = updated_pointer occ1 in
            let into = updated_pointer occ2 in
            inline w ~from ~into linear ;
            (* References to [from] should refer to [into] instead *)
            pointers := IMap.add from into !pointers ;
            (w :: free_wires, removable_constrs)
        | _ -> (free_wires, removable_constrs))
      wire_occurrences
      ([], [])
  in

  (* Drop removable constraints (only now that the inlining has finished) *)
  List.iter (fun c -> linear.(c) <- []) removable_constrs ;
  let linear = List.filter (fun ts -> ts <> []) (Array.to_list linear) in

  (* Sort free_vars to reach allocation order. Is this really needed? *)
  let free_wires = List.sort Int.compare free_wires in

  let pseudo_blocks = List.map pseudo_block_of_terms linear in

  (pseudo_blocks, non_linear, free_wires)

(** [place_over_pseudo_block ~perfectly b1 b2] tries to join pseudo block [b1]
    on top of [b2], i.e., link the last pseudo constraint of [b1] with the
    first pseudo constraint of [b2]. It returns an optional argument containing
    the combined pseudo block if the combination was successful.
    [perfectly] is a Boolean argument that specifies whether only "perfect"
    matches should be accepted. In this context, "perfect" means that the two
    constraints to be combined share exactly [nb_wires_arch] wires. *)
let place_over_pseudo_block ~perfectly b1 b2 =
  let b1_rest, b1_k = split_last b1 in
  let b2_0, b2_rest = split_first b2 in

  if b1_k.pc_tail <> [] || b2_0.pc_head <> [] then None
  else
    let b1_body = List.map snd b1_k.pc_body in
    let b2_body = List.map snd b2_0.pc_body in
    let common_body = list_intersection ~equal:( = ) b1_body b2_body in

    (* Given that the head of [b2_0] is empty, we can place [nb_wires_arch]
       terms on it. The total room depends on how occupied its tail is *)
    let b2_free_room = (2 * nb_wires_arch) - List.length b2_0.pc_tail in

    (* We will pick [n] common terms between the bodies of [b1_k] and [b2_0].
       If the body of [b2_0] fits completely into its head, we do not need
       to leave an extra space for an auxiliary variable *)
    let n =
      if
        List.compare_length_with b2_body b2_free_room <= 0
        && List.compare_length_with b2_body nb_wires_arch = 0
      then nb_wires_arch
      else nb_wires_arch - 1
    in
    let common = rev_first_n ~n common_body |> ISet.of_list in

    let tight = ISet.cardinal common = nb_wires_arch in
    if perfectly && not tight then None
    else if nb_wires_arch - ISet.cardinal common > 1 then None
    else
      let split_body = List.partition (fun (_, w) -> ISet.mem w common) in
      let qcommon1, body1 = split_body b1_k.pc_body in
      let qcommon2, body2 = split_body b2_0.pc_body in

      (* We sort them so that they have the same order *)
      let compare_terms (_, w1) (_, w2) = Int.compare w1 w2 in
      let qcommon1 = List.sort compare_terms qcommon1 in
      let qcommon2 = List.sort compare_terms qcommon2 in
      let pc1 = {b1_k with pc_tail = qcommon1; pc_body = body1} in
      let pc2 = {b2_0 with pc_head = qcommon2; pc_body = body2} in
      Some (b1_rest @ (pc1 :: pc2 :: b2_rest))

let combine_pseudo_blocks ~scope ~perfectly pseudo_blocks =
  let try_both_ways b1 b2 =
    match place_over_pseudo_block ~perfectly b1 b2 with
    | None -> place_over_pseudo_block ~perfectly b2 b1
    | Some union -> Some union
  in
  combine_quadratic ~scope ~join:try_both_ways pseudo_blocks

let linear_combination ~this ~next qc =
  let pad = List.init (nb_wires_arch - List.length this) (fun _ -> 0) in
  let wires = List.map snd this @ pad in

  let combine l1 l2 = List.combine (list_sub (List.length l2) l1) l2 in
  let this_sels = combine CS.this_constr_linear_selectors (List.map fst this) in
  let next_sels = combine CS.next_constr_linear_selectors (List.map fst next) in
  CS.mk_linear_constr (wires, (("qc", qc) :: this_sels) @ next_sels)

let dummy_linear_combination terms =
  let pad = List.init (nb_wires_arch - List.length terms) (fun _ -> 0) in
  CS.mk_linear_constr (List.map snd terms @ pad, [])

(** Given a pseudo block, i.e., a list of pseudo constraints, transform it into
    a block. Thanks to the invariant that the pc_head of a pseudo constraint
    starts with the pc_tail of the previous one, it is enough to add a constraint
    for each pc_head (and encode each pc_tail with next-gate selectors).
    Note that pc_bodies may need to be freed through auxiliary variables. *)
let block_of_pseudo_block trace_info pseudo_block =
  let pseudo_block = Array.of_list pseudo_block in
  Array.fold_left
    (fun (trace_info, block, residue_pseudo_blocks, i) b ->
      (* We need to move all terms from b.pc_body to its head or tail.
         If there are more terms that those that can be moved, we will
         need an auxiliary variable, to be put in the head *)

      (* TODO: select heuristically what to place and where, e.g., leave
         for the residue the terms that are more frequent among the circuit *)
      let is_last = i = Array.length pseudo_block - 1 in
      let free_room = nb_wires_arch - List.length b.pc_head in
      let free_room =
        free_room + if is_last then nb_wires_arch - List.length b.pc_tail else 0
      in
      let aux_var_needed = List.compare_length_with b.pc_body free_room > 0 in
      let n = if aux_var_needed then nb_wires_arch - 1 else nb_wires_arch in
      let head, pending = complete ~n b.pc_head b.pc_body in
      let tail, pending =
        if is_last then complete ~n:nb_wires_arch b.pc_tail pending
        else (b.pc_tail, pending)
      in
      let trace_info, head, residues =
        if not aux_var_needed then (
          assert (pending = []) ;
          (trace_info, head, []))
        else
          let v, free_wires = split_first trace_info.free_wires in
          let assignments = (v, pending) :: trace_info.assignments in
          assert (List.compare_length_with head nb_wires_arch < 0) ;
          let head = head @ [(Scalar.one, v)] in
          let trace_info = {free_wires; assignments} in
          let residues =
            [pseudo_block_of_terms @@ ((Scalar.(negate one), v) :: pending)]
          in
          (trace_info, head, residues)
      in
      (* The next constraint will have a head that starts with this tail *)
      let constr1 = linear_combination ~this:head ~next:tail b.pc_const in
      let constrs =
        if is_last && tail <> [] then [constr1; dummy_linear_combination tail]
        else [constr1]
      in
      (trace_info, block @ constrs, residues @ residue_pseudo_blocks, succ i))
    (trace_info, [], [], 0)
    pseudo_block
  |> fun (trace_info, block, residue_pseudo_blocks, _) ->
  (trace_info, Array.of_list block, residue_pseudo_blocks)

(** This function takes a list of pseudo blocks, combines them if possible and
    converts them into blocks. This conversion may lead to residue pseudo blocks
    (if auxiliary variables were involved to free a pc_body).
    It then applies the same process on the residue pseudo blocks.
    Observe that it is relatively rare that a residue pseudo block is produced
    after the convertion. Consequently the second iteration of this function
    will be among many fewer pseudo blocks and it quickly reaches termination *)
let blocks_of_pseudo_blocks ~scope free_wires pseudo_blocks =
  let rec iteration (trace_info, blocks, pseudo_blocks) =
    match pseudo_blocks with
    | [] -> (trace_info, blocks)
    | _ ->
        let joined =
          pseudo_blocks
          |> combine_pseudo_blocks ~scope ~perfectly:true
          |> combine_pseudo_blocks ~scope ~perfectly:false
        in
        List.fold_left
          (fun (trace_info, blocks, new_pseudoblks) pb ->
            let trace_info, block, new_pseudo =
              block_of_pseudo_block trace_info pb
            in
            ( trace_info,
              block :: blocks,
              List.rev_append new_pseudo new_pseudoblks ))
          (trace_info, blocks, [])
          joined
        |> iteration
  in
  let trace_info = {free_wires; assignments = []} in
  iteration (trace_info, [], pseudo_blocks)

(** [place_over_block ~perfectly pb pb'] tries to join block [b1] on top
    of [b2]. It works only if the last constraint of [b1] has no selectors.
    [perfectly] returns only matches that have no empty selectors (no holes).
    It is relevant for a heuristic where we first want to find perfect matches
    and then any match.

    The joining is done by
    - finding a permutation that makes the wires of b1.last and b2.first match
      (being relaxed about unset wires)
    - checking if the permutation if perfect and failing if required
    - renaming the selectors of b1.last
    - renaming the next selectors of b1.second_last

    Example:
    [ (1 1 1)  [ql:a qlg:b]
      (3 2 -1) [] ]
    [ (2 3 4)  [ql:a]
      (1 1 1)  [ql:a] ]
    becomes
    [ (1 1 1)  [ql:a qrg:b]
      (2 3 4)  [ql:a]
      (1 1 1)  [ql:a] ]
*)
let place_over_block : perfectly:bool -> block -> block -> block option =
 fun ~perfectly b1 b2 ->
  let b1_k = b1.(Array.length b1 - 1) in

  if b1_k.sels <> [] then None
  else
    let wires1 = CS.wires_of_constr_i b1 (Array.length b1 - 1) in
    let wires2 = CS.wires_of_constr_i b2 0 in

    match adapt wires1 wires2 with
    | None -> None
    | Some perm ->
        let wires1 = permute_list perm wires1 in
        let combined =
          List.map2 (fun x y -> if x >= 0 then x else y) wires1 wires2
        in
        let is_perfect = List.for_all (fun x -> x >= 0) combined in
        let b1_k_1 = b1.(Array.length b1 - 2) in
        if perfectly && not is_perfect then None
        else if
          (* if the second last of b1 is not arithmetic, we cannot permute it *)
          not (is_identity_perm perm || CS.is_arithmetic_raw_constr b1_k_1)
        then None
        else
          let rename n =
            let selectors_assoc =
              List.combine
                (permute_list perm CS.next_constr_linear_selectors)
                CS.next_constr_linear_selectors
            in
            assert (List.compare_length_with selectors_assoc nb_wires_arch = 0) ;
            match List.find_opt (fun (s, _) -> s = n) selectors_assoc with
            | Some (_, s') -> s'
            | None -> n
          in
          let sels_prev = List.map (fun (n, q) -> (rename n, q)) b1_k_1.sels in
          let wires =
            (* TODO: Revisit [type raw_constraint] and how to represent unused
               wires. Should the conversion from -1 to 0 be done here? *)
            List.map (max 0) combined |> Array.of_list
          in
          let b2_0 = b2.(0) in
          let b2_rest = Array.sub b2 1 (Array.length b2 - 1) in
          assert (b1_k.precomputed_advice = []) ;
          b1.(Array.length b1 - 1) <-
            {
              wires;
              sels = b2_0.sels;
              precomputed_advice = b2_0.precomputed_advice;
              label = "hyb" :: b2_0.label;
            } ;
          b1.(Array.length b1 - 2) <- {b1_k_1 with sels = sels_prev} ;
          Some (Array.append b1 b2_rest)

let combine_blocks ~perfectly blocks =
  let blocks_with_empty_selectors, rest =
    let open CS in
    List.partition (fun b -> b.(Array.length b - 1).sels = []) blocks
  in
  combine_quadratic_efficient
    ~join:(place_over_block ~perfectly)
    blocks_with_empty_selectors
    rest

(** Takes {!type:trace_info} and returns a function that updates a given
    trace to make it compatible with the optimized constraints. *)
let trace_updater : trace_info -> scalar array -> scalar array =
 fun trace_info trace ->
  let sum ~trace =
    List.fold_left
      (fun s (coeff, i) -> Scalar.(add s @@ mul coeff trace.(i)))
      Scalar.zero
  in
  List.iter
    (fun (i, terms) -> trace.(i) <- sum ~trace terms)
    (* Iterate over the assignments reversed to ensure that variables
       are assigned in the correct order, in case they depend on each other. *)
    (List.rev trace_info.assignments) ;
  trace

let remove_boolean_gates gates =
  let gates, boolean_vars =
    List.fold_left
      (fun (non_boolean_gates, boolean_vars) gate ->
        if Array.length gate = 1 then
          match CS.boolean_raw_constr gate.(0) with
          | Some b -> (non_boolean_gates, b :: boolean_vars)
          | None -> (gate :: non_boolean_gates, boolean_vars)
        else (gate :: non_boolean_gates, boolean_vars))
      ([], [])
      gates
  in
  (List.rev gates, boolean_vars)

let add_boolean_checks ~boolean_vars gates =
  let bool_vars_map = ref @@ ISet.of_list boolean_vars in
  let shared_a constr =
    let a_wire = CS.(constr.wires.(0)) in
    if ISet.mem a_wire !bool_vars_map then (
      bool_vars_map := ISet.remove a_wire !bool_vars_map ;
      CS.{constr with sels = ("qbool", Scalar.one) :: constr.sels})
    else constr
  in
  let unused_a ~gate i constr =
    let wires = CS.wires_of_constr_i gate i in
    if List.hd wires < 0 && ISet.cardinal !bool_vars_map > 0 then (
      let a = ISet.choose !bool_vars_map in
      let wires = CS.(constr.wires) in
      wires.(0) <- a ;
      bool_vars_map := ISet.remove a !bool_vars_map ;
      CS.{constr with wires; sels = ("qbool", Scalar.one) :: constr.sels})
    else constr
  in
  (* First try to add them on existing constraints, through the qbool selector *)
  let gates = List.map (fun gate -> Array.map shared_a gate) gates in
  (* Then, try to use constraints that do not use the a wire *)
  let gates = List.map (fun gate -> Array.mapi (unused_a ~gate) gate) gates in
  (* Finally, add manual constraints for the remaining boolean variables *)
  let new_bool_gates =
    List.map CS.mk_bool_constr (ISet.to_seq !bool_vars_map |> List.of_seq)
    |> Array.of_list
  in
  List.rev (new_bool_gates :: gates)

(* This function takes care of renaming variables that are bound to be
   the same by an arithmetic constraint.
   For example, if there is a constraint of the form [x - y = 0], we will
   replace [x -> y] everywhere and remove the constraint.
   Alternatively, we could replace [y -> x].
   This is not possible if both [x] and [y] are inputs to the circuit. *)
let inline_renamings ~nb_inputs gates =
  let renaming_pairs =
    List.filter_map
      (fun gate ->
        if Array.length gate = 1 && CS.is_linear_raw_constr gate.(0) then
          match CS.linear_terms gate.(0) with
          | [(q1, w1); (q2, w2)]
            when w1 >= 0 && w2 >= 0 && Scalar.(is_zero @@ add q1 q2) ->
              Some (w1, w2)
          | _ -> None
        else None)
      gates
  in
  (* Create a map from wires to all the wires they are bound to be equal to *)
  let renaming_partitions =
    List.fold_left
      (fun partitions (i, j) ->
        let rec find_all_equal ~seen i =
          let i_set =
            Option.value ~default:ISet.empty @@ IMap.find_opt i partitions
          in
          ISet.fold
            (fun j (acc, seen) ->
              if ISet.mem j seen then (acc, seen)
              else
                let j_set = find_all_equal ~seen j in
                (ISet.union acc j_set, ISet.add j seen))
            i_set
            (i_set, ISet.add i seen)
          |> fst
        in
        let ij_partition =
          let ij = ISet.add i @@ ISet.singleton j in
          let i_set = find_all_equal ~seen:ij i in
          let j_set = find_all_equal ~seen:ij j in
          ISet.union i_set j_set |> ISet.union ij
        in
        ISet.fold
          (fun k partitions -> IMap.add k ij_partition partitions)
          ij_partition
          partitions)
      IMap.empty
      renaming_pairs
  in
  (* Each wire will be renamed (if it is not an input wire) to the
     minimal wire of their partition. This makes sure that all the wire
     in the same partition get renamed to the same wire (when renamed) *)
  let renaming =
    IMap.mapi
      (fun i i_set -> if i < nb_inputs then i else ISet.min_elt i_set)
      renaming_partitions
  in
  let free_wires =
    IMap.filter ( <> ) renaming |> IMap.bindings |> List.map fst
  in
  let rename i = Option.value ~default:i @@ IMap.find_opt i renaming in
  (List.map (CS.rename_wires ~rename) gates, free_wires)

(* We remove all constraints of the form [a * x - a * x = 0], which may
   be the result of having renamed some variables *)
let remove_trivial gates =
  List.filter
    (fun gate ->
      if Array.length gate = 1 && CS.is_linear_raw_constr gate.(0) then
        match CS.linear_terms gate.(0) with
        | [(q1, w1); (q2, w2)]
          when w1 >= 0 && w2 >= 0 && Scalar.(is_zero @@ add q1 q2) ->
            w1 <> w2
        | _ -> true
      else true)
    gates

(** Takes a list of raw_constraints and returns an equivalent constraint system
    with potentially fewer constraints.
    As a second output, it returns the necessary information to build a function
    that updates the trace to make it compatible with the new constraints *)
let optimize : nb_inputs:int -> CS.gate list -> CS.gate list * trace_info =
 fun ~nb_inputs gates ->
  let gates, free_wires_inlining = inline_renamings ~nb_inputs gates in
  let gates = remove_trivial gates in
  let linear_pseudo_blocks, non_linear, free_wires =
    inline_linear ~nb_inputs gates
  in
  let free_wires = List.sort Int.compare @@ free_wires_inlining @ free_wires in
  let non_linear, boolean_vars = remove_boolean_gates non_linear in
  let scope =
    if List.compare_length_with non_linear 10_000 > 0 then 10 else 100
  in
  let trace_info, linear_blocks =
    blocks_of_pseudo_blocks ~scope free_wires linear_pseudo_blocks
  in
  let cs =
    List.rev_append non_linear linear_blocks
    |> combine_blocks ~scope ~perfectly:true
    |> combine_blocks ~scope ~perfectly:false
    (* we shuffle the blocks to mix linear with non-linear ones and this
       way allow for more likely combinations; we use an arbitrary fixed seed
       for determinism *)
    |> shuffle_list ~seed:1031
    |> combine_blocks ~scope ~perfectly:false
    |> shuffle_list ~seed:42
    |> combine_blocks ~scope ~perfectly:false
  in
  let cs = add_boolean_checks ~boolean_vars cs in
  (cs, trace_info)
