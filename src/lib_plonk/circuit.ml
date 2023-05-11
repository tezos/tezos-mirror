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

open Plompiler.Csir

(* We assert here that all modules/selectors have been used.
   The "+2" is to take into account lookup selectors which are not defined in
   plonk/gates/custom_gates.ml
   The "-1" is to remove the Public_gate, which is not used *)
let () =
  assert (
    List.compare_length_with
      CS.all_selectors
      Custom_gates.(nb_custom_gates - 1 - nb_input_com + 2)
    = 0)

let gates_to_string m =
  SMap.fold
    (fun k v s ->
      s ^ k ^ " " ^ String.concat "," (List.map Scalar.to_string v) ^ "\n")
    m
    ""

module Circuit : sig
  type t = private {
    wires : int array array;
    gates : Scalar.t array SMap.t;
    tables : Scalar.t array list list;
    public_input_size : int;
    input_com_sizes : int list;
    circuit_size : int;
    table_size : int;
    nb_lookups : int;
    ultra : bool;
    range_checks : (int * int) list;
  }

  val make_gates :
    ?qc:Scalar.t list ->
    ?linear:(int * Scalar.t list) list ->
    ?linear_g:(int * Scalar.t list) list ->
    ?qm:Scalar.t list ->
    ?qx2b:Scalar.t list ->
    ?qx5a:Scalar.t list ->
    ?qx5c:Scalar.t list ->
    ?qecc_ws_add:Scalar.t list ->
    ?qecc_ed_add:Scalar.t list ->
    ?qecc_ed_cond_add:Scalar.t list ->
    ?qbool:Scalar.t list ->
    ?qcond_swap:Scalar.t list ->
    ?q_anemoi:Scalar.t list ->
    ?q_plookup:Scalar.t list ->
    ?q_table:Scalar.t list ->
    ?precomputed_advice:Scalar.t list SMap.t ->
    unit ->
    Scalar.t list SMap.t

  val make :
    wires:int list array ->
    gates:Scalar.t list SMap.t ->
    ?tables:Scalar.t array list list ->
    public_input_size:int ->
    ?input_com_sizes:int list ->
    ?range_checks:(int * int) list ->
    unit ->
    t

  val get_nb_of_constraints : t -> int

  (* /////////////////////////////////////////////////////////////////////// *)

  val get_selectors : t -> string list

  val sat : CS.t -> Table.t list -> Scalar.t array -> bool

  val to_plonk :
    public_input_size:int ->
    ?input_com_sizes:int list ->
    ?tables:Table.t list ->
    ?range_checks:(int * int) list ->
    CS.t ->
    t
end = struct
  type t = {
    wires : int array array;
    gates : Scalar.t array SMap.t;
    tables : Scalar.t array list list;
    public_input_size : int;
    input_com_sizes : int list;
    circuit_size : int;
    table_size : int;
    nb_lookups : int;
    ultra : bool;
    range_checks : (int * int) list;
  }

  let get_selectors circuit = SMap.keys circuit.gates

  let make_gates ?(qc = []) ?(linear = []) ?(linear_g = []) ?(qm = [])
      ?(qx2b = []) ?(qx5a = []) ?(qx5c = []) ?(qecc_ws_add = [])
      ?(qecc_ed_add = []) ?(qecc_ed_cond_add = []) ?(qbool = [])
      ?(qcond_swap = []) ?(q_anemoi = []) ?(q_plookup = []) ?(q_table = [])
      ?(precomputed_advice = SMap.empty) () =
    if q_anemoi <> [] && SMap.(is_empty precomputed_advice) then
      failwith "Make_gates : q_anemoi must come with advice selectors." ;
    (* Filtering and mapping selectors with labels. *)
    let gate_list =
      CS.q_list
        ~qc
        ~linear
        ~linear_g
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
        ()
    in
    let add_map map (label, q) =
      match q with
      | [] -> map
      | l -> if List.for_all Scalar.is_zero l then map else SMap.add label q map
    in
    let base =
      if q_table = [] then SMap.empty else SMap.singleton "q_table" q_table
    in
    List.fold_left add_map base (gate_list @ SMap.bindings precomputed_advice)

  (* If public_input_size is greater than 0, selector ql will be added if not
     already present.
     Wires and gates cannot be empty and must all have the same length.
  *)
  let make ~wires ~gates ?(tables = []) ~public_input_size
      ?(input_com_sizes = []) ?(range_checks = []) () =
    if Array.length wires = 0 then
      raise @@ Invalid_argument "Make Circuit: empty wires." ;
    if SMap.is_empty gates then
      raise @@ Invalid_argument "Make Circuit: empty gates." ;
    (* We infer the circuit size from the length of the first wire *)
    let circuit_size = List.length wires.(0) in
    if Int.equal circuit_size 0 then
      raise (Invalid_argument "Make Circuit: empty circuit.") ;
    let wires = Array.map Array.of_list wires in
    let nb_wires = Array.length wires in
    (* Check that all wires have same size *)
    Array.iter
      (fun l ->
        if Array.length l <> circuit_size then
          raise (Invalid_argument "Make Circuit: different length wires."))
      wires ;
    (* Add missing wires if omitted to have exactly [Csir.nb_wires_arch] *)
    let unused_wires =
      Array.init
        (Plompiler.Csir.nb_wires_arch - Array.length wires)
        (fun _ -> Array.init circuit_size (Fun.const 0))
    in
    let wires = Array.concat [wires; unused_wires] in
    (* Filter out null gates *)
    let gates =
      SMap.filter_map
        (fun label -> function
          | [] -> None
          | q ->
              if label = "q_table" then Some (Array.of_list q)
              else if List.exists (fun s -> not (Scalar.is_zero s)) q then
                Some (Array.of_list q)
              else None)
        gates
    in
    (* Check that all selectors have the same size, and that they are available. *)
    let () =
      SMap.iter
        (fun label q ->
          if Array.length q = circuit_size then ()
          else raise (Invalid_argument "Make Circuit: different length gates.") ;
          if
            List.mem label (List.map fst CS.all_selectors)
            || String.starts_with ~prefix:Custom_gates.qadv_label label
          then ()
          else raise (Invalid_argument "Make Circuit: unknown gates."))
        gates
    in
    (* Check all tables' columns have the same size. *)
    let () =
      List.iter
        (fun l ->
          let sub_table_size = Array.length (List.hd l) in
          if List.compare_length_with l nb_wires > 0 then
            raise
              (Invalid_argument "Make Circuit: table(s) with too many columns.") ;
          List.iter
            (fun t ->
              if Array.length t != sub_table_size then
                raise
                  (Invalid_argument
                     "Make Circuit: table(s) with columns of different length.")
              else ())
            l)
        tables
    in
    (* Check all range indexes are contained in the array *)
    let () =
      List.iter
        (fun (i, _) ->
          if
            List.exists
              (fun l -> i >= circuit_size + l + public_input_size)
              input_com_sizes
          then
            raise
              (Invalid_argument
                 "Make Circuit: inconsistent range checks indices."))
        range_checks
    in
    let table_size =
      if tables = [] then 0
      else List.fold_left (fun acc t -> acc + Array.length (List.hd t)) 0 tables
    in
    (* Determining if UltraPlonk or TurboPlonk needs to be used. *)
    let ultra = SMap.mem "q_plookup" gates in
    let nb_lookups =
      if not ultra then 0
      else
        let q_plookup = SMap.find "q_plookup" gates in
        Array.fold_left
          (fun acc qi -> if Scalar.is_zero qi then acc else acc + 1)
          0
          q_plookup
    in
    if ultra && not (SMap.mem "q_table" gates) then
      raise (Invalid_argument "Make Circuit: expected table selector.") ;
    if ultra && tables = [] then
      raise (Invalid_argument "Make Circuit: tables empty.") ;
    if (not ultra) && (tables != [] || SMap.mem "q_table" gates) then
      raise (Invalid_argument "Make Circuit: table(s) given with no lookups.") ;
    let gates =
      (* Define ql if undefined as it is the gate taking the public input in. *)
      let ql_name = Plompiler.Csir.linear_selector_name 0 in
      if
        List.fold_left ( + ) public_input_size input_com_sizes > 0
        && (not @@ SMap.mem ql_name gates)
      then
        SMap.add ql_name (Array.init circuit_size (fun _ -> Scalar.zero)) gates
      else gates
    in
    {
      circuit_size;
      wires;
      gates;
      tables;
      public_input_size;
      input_com_sizes;
      table_size;
      nb_lookups;
      ultra;
      range_checks;
    }

  let get_nb_of_constraints cs = Array.length cs.wires.(0)
  (* ////////////////////////////////////////////////////////// *)

  let sat_gate identities gate trace tables =
    let open CS in
    let nb_cs = Array.length gate in
    let identities =
      (* For each constraint *)
      List.init nb_cs (fun i ->
          (* Retrieving its values as well as the next constraint's values *)
          let j = (i + 1) mod nb_cs in
          let ci, cj = (gate.(i), gate.(j)) in
          let wires = Array.map (fun w -> trace.(w)) ci.wires in
          let wires_g = Array.map (fun w -> trace.(w)) cj.wires in
          (* Folding on selectors *)
          List.fold_left
            (fun id_map (s_name, q) ->
              match s_name with
              | "q_plookup" -> id_map
              | "q_table" ->
                  (* We assume there can be only one lookup per gate *)
                  let entry : Table.entry = wires in
                  let sub_table = List.nth tables (Scalar.to_z q |> Z.to_int) in
                  let b = Table.mem entry sub_table in
                  let id = [|(if b then Scalar.zero else Scalar.one)|] in
                  SMap.add "q_table" id id_map
              | _ ->
                  (* Retrieving the selector's identity name and equations *)
                  let s_id_name, _ = Custom_gates.get_ids s_name in
                  let s_ids = SMap.find s_id_name id_map in
                  let precomputed_advice = SMap.of_list ci.precomputed_advice in
                  (* Updating the identities with the equations' output *)
                  List.iteri
                    (fun i s -> s_ids.(i) <- Scalar.(s_ids.(i) + s))
                    ((Custom_gates.get_eqs s_name)
                       ~precomputed_advice
                       ~q
                       ~wires
                       ~wires_g
                       ()) ;
                  SMap.add s_id_name s_ids id_map)
            identities
            ci.sels)
    in
    (* Checking all identities are verified, i.e. the map contains only 0s *)
    List.for_all
      (SMap.for_all (fun _id_name id ->
           let b = Array.for_all Scalar.is_zero id in
           (* if Bool.not b then Printf.printf "\nIdentity '%s' not satisfied" id_name
              else () ; *)
           b))
      identities

  let sat cs tables trace =
    (* We initialise a map with all ids used in the circuit *)
    let identities =
      List.fold_left
        (fun map m ->
          let id, nb_ids = Custom_gates.get_ids m in
          if SMap.mem id map then map
          else SMap.add id (Array.init nb_ids (fun _ -> Scalar.zero)) map)
        (SMap.singleton "q_table" [|Scalar.zero|])
        Custom_gates.gates_list
    in
    let exception Constraint_not_satisfied of string in
    try
      (* We check in each gate, constraint by constraint, that all ids are satisfied *)
      List.iteri
        (fun i gate ->
          (* Printf.printf "\n\nGate %i: %s" i
                (Plompiler.Csir.CS.to_string_gate gate); *)
          let b = sat_gate identities gate trace tables in
          if b then ()
          else
            (* just to exit the iter *)
            raise
              (Constraint_not_satisfied
                 (Printf.sprintf "\nGate #%i not satisfied." i)))
        cs ;
      true
    with Constraint_not_satisfied _ -> false

  let to_plonk ~public_input_size ?(input_com_sizes = []) ?(tables = [])
      ?(range_checks = []) cs =
    let open CS in
    let cs = List.rev Array.(to_list @@ concat cs) in
    assert (cs <> []) ;
    let add_wires ws wires =
      if Array.length wires = 0 then Array.map (fun w -> [w]) ws
      else Array.map2 (fun w l -> w :: l) ws wires
    in
    let add_selectors sels map pad =
      (* Add to the map all new selectors with the coresponding padding
         (array of [pad] zeroes). *)
      let map =
        List.fold_left
          (fun map (k, _) ->
            if SMap.mem k map then map
            else
              let zeros = List.init pad (fun _ -> Scalar.zero) in
              SMap.add k zeros map)
          map
          sels
      in

      (* Extend every binding in the map by either add the coefficient
         or pad with a zero. *)
      SMap.fold
        (fun label qq map ->
          let q =
            match List.find_opt (fun (s, _) -> s = label) sels with
            | None -> Scalar.zero
            | Some (_, coeff) -> coeff
          in
          SMap.add label (q :: qq) map)
        map
        map
    in
    List.fold_left
      (fun (acc_wires, selectors_map, advice_map, pad)
           {wires; sels; precomputed_advice; label} ->
        ignore label ;
        let acc_wires = add_wires wires acc_wires in
        let selectors_map = add_selectors sels selectors_map pad in
        let advice_map = add_selectors precomputed_advice advice_map pad in
        (acc_wires, selectors_map, advice_map, pad + 1))
      SMap.([||], empty, empty, 0)
      cs
    |> fun (wires, selectors, advice, _) ->
    let gates = SMap.union_disjoint selectors advice in
    let tables = List.map Table.to_list tables in
    make
      ~wires
      ~gates
      ~range_checks
      ~public_input_size
      ~input_com_sizes
      ~tables
      ()
end

include Circuit
