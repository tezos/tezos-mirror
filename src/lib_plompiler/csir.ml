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

let nb_wires_arch = 6

let wire_prefix = "w"

let string_key_of_int ~nb_digits i =
  let s = string_of_int i in
  wire_prefix ^ String.make (nb_digits - String.length s) '0' ^ s

let wire_name i =
  if i < 0 || i >= nb_wires_arch then
    raise @@ Failure "wire_name: i must be in the range [0, nb_wires_arch)" ;
  string_key_of_int
    ~nb_digits:(String.length @@ string_of_int (nb_wires_arch - 1))
    i

let int_of_wire_name s =
  let n = String.length wire_prefix in
  try
    if String.sub s 0 n <> wire_prefix then
      failwith "int_of_wire_name : invalid wire name." ;
    int_of_string (String.sub s n (String.length s - n))
  with _ -> failwith "int_of_wire_name : invalid wire name."

let linear_selector_name i = "q_" ^ wire_name i

let add_next_wire_suffix s = s ^ "g"

module Scalar = struct
  include Bls12_381.Fr

  type scalar = t

  let mone = negate one

  let string_of_scalar s =
    if String.length (to_string s) < 10 then to_string s
    else if String.length (to_string (negate s)) < 10 then
      "-" ^ to_string (negate s)
    else "H" ^ (to_z s |> Z.hash |> string_of_int)

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
       ...
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

  let to_list table = Array.to_list table

  let of_list table = Array.of_list table
end

let table_or =
  Table.of_list
  @@ Scalar.
       [
         [|zero; zero; one; one|];
         [|zero; one; zero; one|];
         [|zero; one; one; one|];
       ]

let table_xor =
  Table.of_list
  @@ Scalar.
       [
         [|zero; zero; one; one|];
         [|zero; one; zero; one|];
         [|zero; one; one; zero|];
       ]

let table_band =
  Table.of_list
  @@ Scalar.
       [
         [|zero; zero; one; one|];
         [|zero; one; zero; one|];
         [|zero; zero; zero; one|];
       ]

(* There are three ways to define a lookup table for a unary operation
   when nb_wires_arch = 3. Let z := f x, then:
    1. x 0 z
    2. x x z
    3. x z 0
   In this module, we choose option 1. *)
let table_bnot =
  Table.of_list @@ Scalar.[[|zero; one|]; [|zero; zero|]; [|one; zero|]]

let generate_lookup_table_op1 ~nb_bits (f : int -> int) =
  let n = 1 lsl nb_bits in
  let x = Array.init n (fun i -> i) in
  let y = Array.init n (fun _i -> 0) in
  let z = Array.map f x in
  List.map (Array.map Scalar.of_int) [x; y; z]

let generate_lookup_table_op2 ~nb_bits (f : int -> int -> int) =
  let n = 1 lsl nb_bits in
  let x = List.init n (fun i -> Array.init n (fun _j -> i)) |> Array.concat in
  let y = List.init n (fun _i -> Array.init n (fun j -> j)) |> Array.concat in
  let z = Array.map2 f x y in
  List.map (Array.map Scalar.of_int) [x; y; z]

let table_bnot4 =
  let nb_bits = 4 in
  let mask4 = (1 lsl nb_bits) - 1 in
  Table.of_list
  @@ generate_lookup_table_op1 ~nb_bits (fun x -> Int.(logand (lognot x) mask4))

let table_xor4 =
  Table.of_list @@ generate_lookup_table_op2 ~nb_bits:4 Int.logxor

let table_band4 =
  Table.of_list @@ generate_lookup_table_op2 ~nb_bits:4 Int.logand

let rotate_right ~nb_bits x y b =
  let a = x + (y lsl nb_bits) in
  let r = Int.logor (a lsr b) (a lsl ((2 * nb_bits) - b)) in
  let mask = (1 lsl nb_bits) - 1 in
  Int.logand r mask

let table_rotate_right4_1 =
  (* x0x1x2x3 y0y1y2y3 -> x1x2x3y0 *)
  let nb_bits = 4 in
  Table.of_list
  @@ generate_lookup_table_op2 ~nb_bits (fun x y -> rotate_right ~nb_bits x y 1)

let table_rotate_right4_2 =
  (* x0x1x2x3 y0y1y2y3 -> x2x3y0y1 *)
  let nb_bits = 4 in
  Table.of_list
  @@ generate_lookup_table_op2 ~nb_bits (fun x y -> rotate_right ~nb_bits x y 2)

let table_rotate_right4_3 =
  (* x0x1x2x3 y0y1y2y3 -> x3y0y1y2 *)
  let nb_bits = 4 in
  Table.of_list
  @@ generate_lookup_table_op2 ~nb_bits (fun x y -> rotate_right ~nb_bits x y 3)

module Tables = Kzg.SMap

let table_registry =
  let t = Tables.add "or" table_or Tables.empty in
  let t = Tables.add "xor" table_xor t in
  let t = Tables.add "band" table_band t in
  let t = Tables.add "bnot" table_bnot t in
  let t = Tables.add "bnot4" table_bnot4 t in
  let t = Tables.add "xor4" table_xor4 t in
  let t = Tables.add "band4" table_band4 t in
  let t = Tables.add "rotate_right4_1" table_rotate_right4_1 t in
  let t = Tables.add "rotate_right4_2" table_rotate_right4_2 t in
  let t = Tables.add "rotate_right4_3" table_rotate_right4_3 t in
  t

module CS = struct
  let q_list ?q_table ~qc ~linear ~linear_g ~qm ~qx2b ~qx5a ~qx5c ~qecc_ws_add
      ~qecc_ed_add ~qecc_ed_cond_add ~qbool ~qcond_swap ~q_anemoi ~q_mod_add
      ~q_mod_mul ~q_plookup () =
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
      @ List.map (fun (label, q) -> ("q_mod_add_" ^ label, q)) q_mod_add
      @ List.map (fun (label, q) -> ("q_mod_mul_" ^ label, q)) q_mod_mul
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
      ~q_mod_add:
        (List.map
           (fun label ->
             (label, [ThisConstr; NextConstr] @ List.init 6 (fun i -> Wire i)))
             (* We list all the labels defined in the MOD_ARITH instantiations
                at the end of [lib_plompiler/gadget_mod_arith.ml] for which we
                want to enable addition. *)
           ["25519"; "64"])
      ~q_mod_mul:
        (List.map
           (fun label ->
             (label, [ThisConstr; NextConstr] @ List.init 6 (fun i -> Wire i)))
             (* We list all the labels defined in the MOD_ARITH instantiations
                at the end of [lib_plompiler/gadget_mod_arith.ml] for which we
                want to enable multiplication. *)
           ["25519"; "64"])
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

  let new_constraint ~wires ?qc ?(linear = []) ?(linear_g = []) ?qm ?qx2b ?qx5a
      ?qx5c ?qecc_ws_add ?qecc_ed_add ?qecc_ed_cond_add ?qbool ?qcond_swap
      ?q_anemoi ?(q_mod_add = []) ?(q_mod_mul = []) ?q_plookup ?q_table
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
           ~q_mod_add:(List.map (fun (i, x) -> (i, Some x)) q_mod_add)
           ~q_mod_mul:(List.map (fun (i, x) -> (i, Some x)) q_mod_mul)
           ~q_plookup
           ~q_table
           ())
    in
    let wires =
      let pad_length = nb_wires_arch - List.length wires in
      wires @ List.init pad_length (Fun.const 0) |> Array.of_list
    in
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
    let ql_name = linear_selector_name 0 in
    if
      (* We do equality through maps as a way to sort the list *)
      SMap.equal
        Scalar.equal
        (SMap.of_seq @@ List.to_seq constr.sels)
        (SMap.of_seq @@ List.to_seq [("qm", Scalar.one); (ql_name, Scalar.mone)])
      && constr.wires.(0) = constr.wires.(1)
    then Some constr.wires.(0)
    else None

  let used_selectors gate i =
    let this_sels = gate.(i).sels in
    let prev_sels = if i = 0 then [] else gate.(i - 1).sels in
    List.filter (fun (s, _) -> List.mem s this_constr_selectors) this_sels
    @ List.filter (fun (s, _) -> List.mem s next_constr_selectors) prev_sels

  let wires_of_constr_i gate i =
    let selectors =
      Array.init nb_wires_arch (fun i -> selectors_with_tags [Wire i])
    in
    let intersect names = List.exists (fun (s, _q) -> List.mem s names) in
    let sels = used_selectors gate i in
    (* We treat qecc_ed_cond_add exceptionally until we have a better interface
       on unused wires *)
    let relax =
      List.map fst sels = ["qecc_ed_cond_add"] && gate.(i).sels = []
    in
    if relax then (
      selectors.(0) <- [] ;
      selectors.(1) <- [] ;
      selectors.(2) <- []) ;
    (* We treat q_anemoi exceptionally until we have a better interface
       on unused wires *)
    let relax = List.map fst sels = ["q_anemoi"] && gate.(i).sels = [] in
    if relax then (
      selectors.(0) <- [] ;
      selectors.(1) <- [] ;
      selectors.(2) <- []) ;
    List.map2
      (fun wsels w -> if intersect wsels sels then w else -1)
      (Array.to_list selectors)
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
