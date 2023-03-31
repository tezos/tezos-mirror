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

let nb_wires_arch = 5

let wire_name i = "w_" ^ string_of_int i

let linear_selector_name = function
  | 0 -> "ql"
  | 1 -> "qr"
  | 2 -> "qo"
  | 3 -> "qd"
  | 4 -> "qe"
  | n -> "q_w" ^ string_of_int n

let add_next_wire_suffix s = s ^ "g"

module Scalar = struct
  include Bls12_381.Fr

  type scalar = t

  let mone = negate one

  let string_of_scalar x =
    if eq x (of_string "-1") then "-1"
    else if eq x (of_string "-2") then "-2"
    else
      let s = to_string x in
      if String.length s > 3 then "h" ^ string_of_int (Z.hash (to_z x)) else s

  let equal a b = Bytes.equal (to_bytes a) (to_bytes b)

  (* TODO https://gitlab.com/nomadic-labs/privacy-team/-/issues/183
     Duplicated in plonk/bls.ml *)
  let t : t Repr.t =
    Repr.(map (bytes_of (`Fixed size_in_bytes)) of_bytes_exn to_bytes)
end

(* If multiple tables are used, they all need to have the same number of wires,
   so any smaller one will be padded. *)
module Table : sig
  type t [@@deriving repr]

  val empty : t

  val size : t -> int

  type entry = Scalar.t array

  type partial_entry = Scalar.t option array

  val mem : entry -> t -> bool

  val find : partial_entry -> t -> entry option

  val to_list : t -> Scalar.t array list

  val of_list : Scalar.t array list -> t
end = struct
  (* Rows are variables, columns are entries in the table.
     If the table is full it would be |domain|^#variables e.g. 2^5=32
     Example OR gate:
     [
       [|0; 0; 1; 1|] ;
       [|0; 1; 0; 1|] ;
       [|0; 1; 1; 1|] ;
       [|0; 0; 0; 0|] ;
       [|0; 0; 0; 0|] ;
     ]
  *)
  type entry = Scalar.t array

  type partial_entry = Scalar.t option array

  type t = Scalar.t array array [@@deriving repr]

  let empty = [||]

  let size table = Array.length table.(0)

  (* Function returning the first table corresponding to the input partial entry.
     A partial entry is found on the table at row i if it coincides
     with the table values in all specified (i.e., not None) columns *)
  let find_entry_i : partial_entry -> t -> int -> entry option =
   fun pe table i ->
    let match_partial_entry o s =
      Option.(value ~default:true @@ map (Scalar.eq s) o)
    in
    if
      match_partial_entry pe.(0) table.(0).(i)
      && match_partial_entry pe.(1) table.(1).(i)
      && match_partial_entry pe.(2) table.(2).(i)
    then Some (Array.map (fun x -> x.(i)) table)
    else None

  let find pe table =
    (* TODO make it a binary search *)
    let sz = size table in
    let rec aux i =
      match i with
      | 0 -> find_entry_i pe table 0
      | _ ->
          let o = find_entry_i pe table i in
          if Option.is_some o then o else aux (i - 1)
    in
    aux (sz - 1)

  let mem : entry -> t -> bool =
   fun entry table ->
    match find (Array.map (fun x -> Some x) entry) table with
    | Some _ -> true
    | None -> false

  let to_list table =
    Format.printf "\n%i %i\n" (Array.length table) (Array.length table.(0)) ;
    Array.to_list table

  let of_list table = Array.of_list table
end

let table_or =
  Table.of_list
    Scalar.
      [
        [|zero; zero; one; one|];
        [|zero; one; zero; one|];
        [|zero; one; one; one|];
        [|zero; zero; zero; zero|];
        [|zero; zero; zero; zero|];
      ]

module Tables = Map.Make (String)

let table_registry = Tables.add "or" table_or Tables.empty

module CS = struct
  let q_list ?q_table ~qc ~linear ~linear_g ~qm ~qx2b ~qx5a ~qx5c ~qecc_ws_add
      ~qecc_ed_add ~qecc_ed_cond_add ~qbool ~qcond_swap ~q_anemoi ~q_plookup ()
      =
    let base =
      [
        ("qc", qc);
        ("qm", qm);
        ("qx2b", qx2b);
        ("qx5a", qx5a);
        ("qx5c", qx5c);
        ("qecc_ws_add", qecc_ws_add);
        ("qecc_ed_add", qecc_ed_add);
        ("qecc_ed_cond_add", qecc_ed_cond_add);
        ("qbool", qbool);
        ("qcond_swap", qcond_swap);
        ("q_anemoi", q_anemoi);
        ("q_plookup", q_plookup);
      ]
      @ List.map (fun (i, q) -> (linear_selector_name i, q)) linear
      @ List.map
          (fun (i, q) -> (linear_selector_name i |> add_next_wire_suffix, q))
          linear_g
    in
    Option.(map (fun q -> ("q_table", q)) q_table |> to_list) @ base

  type selector_tag =
    | Linear
    | Arithmetic
    | ThisConstr
    | NextConstr
    | Wire of int
  [@@deriving repr]

  let all_selectors =
    let linear =
      List.init nb_wires_arch (fun i ->
          (i, [ThisConstr; Linear; Arithmetic; Wire i]))
    in
    let linear_g =
      List.init nb_wires_arch (fun i ->
          (i, [NextConstr; Linear; Arithmetic; Wire i]))
    in
    q_list
      ~qc:[ThisConstr; Arithmetic]
      ~linear
      ~linear_g
      ~qm:[ThisConstr; Arithmetic; Wire 0; Wire 1]
      ~qx2b:[ThisConstr; Arithmetic; Wire 1]
      ~qx5a:[ThisConstr; Arithmetic; Wire 0]
      ~qx5c:[ThisConstr; Arithmetic; Wire 2]
      ~qecc_ws_add:[ThisConstr; NextConstr; Wire 0; Wire 1; Wire 2]
      ~qecc_ed_add:[ThisConstr; NextConstr; Wire 0; Wire 1; Wire 2]
      ~qecc_ed_cond_add:
        [ThisConstr; NextConstr; Wire 0; Wire 1; Wire 2; Wire 3; Wire 4]
      ~qbool:[ThisConstr; Wire 0]
      ~qcond_swap:[ThisConstr; Wire 0; Wire 1; Wire 2; Wire 3; Wire 4]
      ~q_anemoi:[ThisConstr; NextConstr; Wire 1; Wire 2; Wire 3; Wire 4]
      ~q_plookup:[ThisConstr; Wire 0; Wire 1; Wire 2; Wire 3; Wire 4]
      ~q_table:[ThisConstr; Wire 0; Wire 1; Wire 2; Wire 3; Wire 4]
      ()

  let selectors_with_tags tags =
    List.filter
      (fun (_, sel_tags) -> List.for_all (fun t -> List.mem t sel_tags) tags)
      all_selectors
    |> List.map fst

  let this_constr_selectors = selectors_with_tags [ThisConstr]

  let next_constr_selectors = selectors_with_tags [NextConstr]

  let this_constr_linear_selectors = selectors_with_tags [ThisConstr; Linear]

  let next_constr_linear_selectors = selectors_with_tags [NextConstr; Linear]

  let arithmetic_selectors = selectors_with_tags [Arithmetic]

  type raw_constraint = {
    wires : int array;
    sels : (string * Scalar.t) list;
    precomputed_advice : (string * Scalar.t) list;
    label : string list;
  }
  [@@deriving repr]

  type gate = raw_constraint array [@@deriving repr]

  type t = gate list [@@deriving repr]

  let new_constraint ~a ~b ~c ?(d = 0) ?(e = 0) ?qc ?(linear = [])
      ?(linear_g = []) ?qm ?qx2b ?qx5a ?qx5c ?qecc_ws_add ?qecc_ed_add
      ?qecc_ed_cond_add ?qbool ?qcond_swap ?q_anemoi ?q_plookup ?q_table
      ?(precomputed_advice = []) ?(labels = []) label =
    let sels =
      List.filter_map
        (fun (l, x) -> Option.bind x (fun c -> Some (l, c)))
        (q_list
           ~qc
           ~linear:(List.map (fun (i, x) -> (i, Some x)) linear)
           ~linear_g:(List.map (fun (i, x) -> (i, Some x)) linear_g)
           ~qm
           ~qx2b
           ~qx5a
           ~qx5c
           ~qecc_ws_add
           ~qecc_ed_add
           ~qecc_ed_cond_add
           ~qbool
           ~qcond_swap
           ~q_anemoi
           ~q_plookup
           ~q_table
           ())
    in
    let wires = [|a; b; c; d; e|] in
    {wires; sels; precomputed_advice; label = label :: labels}

  let get_sel sels s =
    match List.find_opt (fun (x, _) -> s = x) sels with
    | None -> Scalar.zero
    | Some (_, c) -> c

  let to_string_raw_constraint {wires; sels; precomputed_advice; label} : string
      =
    let pp_sel (s, c) = s ^ ":" ^ Scalar.string_of_scalar c in
    let selectors = String.concat " " (List.map pp_sel sels) in
    let precomputed_advice =
      String.concat " " (List.map pp_sel precomputed_advice)
    in
    let wires_str =
      Array.mapi (fun i w -> Format.sprintf "%s:%i" (wire_name i) w) wires
    in
    Format.sprintf
      "%s %s | %s [%s]"
      (String.concat " " @@ Array.to_list wires_str)
      selectors
      precomputed_advice
      (String.concat " ; " label)

  let to_string_gate g =
    String.concat "\n" @@ Array.to_list @@ Array.map to_string_raw_constraint g

  let to_string cs =
    List.fold_left (fun acc con -> acc ^ to_string_gate con ^ "\n\n") "" cs

  let is_linear_raw_constr constr =
    let linear_selectors =
      ("qc" :: this_constr_linear_selectors) @ next_constr_linear_selectors
    in
    let is_linear_sel (s, _q) = List.mem s linear_selectors in
    List.for_all is_linear_sel constr.sels

  let rename_wires_constr ~rename constr =
    {constr with wires = Array.map rename constr.wires}

  let rename_wires ~rename gate = Array.map (rename_wires_constr ~rename) gate

  let is_arithmetic_raw_constr constr =
    let is_arithmetic_sel (s, _q) = List.mem s arithmetic_selectors in
    List.for_all is_arithmetic_sel constr.sels

  let boolean_raw_constr constr =
    let module SMap = Map.Make (String) in
    if
      (* We do equality through maps as a way to sort the list *)
      SMap.equal
        Scalar.equal
        (SMap.of_seq @@ List.to_seq constr.sels)
        (SMap.of_seq @@ List.to_seq [("qm", Scalar.one); ("ql", Scalar.mone)])
      && constr.wires.(0) = constr.wires.(1)
    then Some constr.wires.(0)
    else None

  let used_selectors gate i =
    let this_sels = gate.(i).sels in
    let prev_sels = if i = 0 then [] else gate.(i - 1).sels in
    List.filter (fun (s, _) -> List.mem s this_constr_selectors) this_sels
    @ List.filter (fun (s, _) -> List.mem s next_constr_selectors) prev_sels

  let wires_of_constr_i gate i =
    let a_selectors = selectors_with_tags [Wire 0] in
    let b_selectors = selectors_with_tags [Wire 1] in
    let c_selectors = selectors_with_tags [Wire 2] in
    let d_selectors = selectors_with_tags [Wire 3] in
    let e_selectors = selectors_with_tags [Wire 4] in
    let intersect names = List.exists (fun (s, _q) -> List.mem s names) in
    let sels = used_selectors gate i in
    (* We treat qecc_ed_cond_add exceptionally until we have a better interface
       on unused wires *)
    let relax =
      List.map fst sels = ["qecc_ed_cond_add"] && gate.(i).sels = []
    in
    let a_selectors = if relax then [] else a_selectors in
    let b_selectors = if relax then [] else b_selectors in
    let c_selectors = if relax then [] else c_selectors in
    (* We treat q_anemoi exceptionally until we have a better interface
       on unused wires *)
    let relax = List.map fst sels = ["q_anemoi"] && gate.(i).sels = [] in
    let a_selectors = if relax then [] else a_selectors in
    let b_selectors = if relax then [] else b_selectors in
    let c_selectors = if relax then [] else c_selectors in
    List.map2
      (fun wsels w -> if intersect wsels sels then w else -1)
      [a_selectors; b_selectors; c_selectors; d_selectors; e_selectors]
      (gate.(i).wires |> Array.to_list)

  let gate_wires gate =
    List.init (Array.length gate) (wires_of_constr_i gate)
    |> List.concat |> List.sort_uniq Int.compare
    |> List.filter (fun x -> x >= 0)

  (* the relationship of this function wrt is_linear_raw_constr is a bit weird *)
  let linear_terms constr =
    if not @@ is_linear_raw_constr constr then
      raise @@ Invalid_argument "constraint is non-linear"
    else
      let module SMap = Map.Make (String) in
      let linear_terms_map =
        ("qc", -1)
        :: List.init nb_wires_arch (fun i ->
               (linear_selector_name i, constr.wires.(i)))
        |> List.to_seq |> SMap.of_seq
      in
      List.map
        (fun (sel_name, coeff) -> (coeff, SMap.find sel_name linear_terms_map))
        constr.sels
      |> List.filter (fun (q, _) -> not @@ Scalar.is_zero q)

  let mk_linear_constr (wires, sels) =
    {
      wires = Array.of_list wires;
      sels;
      precomputed_advice = [];
      label = ["linear"];
    }

  let mk_bool_constr wire =
    let wires = Array.init nb_wires_arch (Fun.const 0) in
    wires.(0) <- wire ;
    {
      wires;
      sels = [("qbool", Scalar.one)];
      precomputed_advice = [];
      label = ["bool"];
    }

  let raw_constraint_equal c1 c2 =
    Array.for_all2 ( = ) c1.wires c2.wires
    && c1.label = c2.label
    && List.for_all2
         (fun (name, coeff) (name', coeff') ->
           name = name' && Scalar.eq coeff coeff')
         c1.sels
         c2.sels
end
