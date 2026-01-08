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

(** PlonK circuit generating backend for Plompiler. *)

include Lang_core
module Tables = Csir.Tables
open Solver
module CS = Csir.CS

let tables = Csir.table_registry

let one = S.one

let mone = S.(negate one)

let wql, wqr, wqo = (0, 1, 2)

type scalar = X of S.t

(* Wire representation.
   Each atom is represented by an index in the trace. *)
type _ repr =
  | Unit : unit repr
  | Scalar : int -> scalar repr
  | Bool : int -> bool repr
  | Pair : 'a repr * 'b repr -> ('a * 'b) repr
  | List : 'a repr list -> 'a list repr

type input_kind = [`InputCom | `Public | `Private] [@@deriving show]

type trace_kind = [input_kind | `NoInput] [@@deriving show]

let compare_trace_kind x y =
  let to_int = function
    | `InputCom -> 0
    | `Public -> 1
    | `Private -> 2
    | `NoInput -> 3
  in
  Int.compare (to_int x) (to_int y)

module Scalar_map = Map.Make (S)

(* State of the interpreter. *)
type state = {
  nvars : int;
  (* Number of variables in the circuit. *)
  cs : CS.t;
  (* Constraint system. *)
  inputs : S.t array;
  (* Inputs declared for the circuit. *)
  input_com_sizes : int list;
  (* Sizes for input commitments. *)
  pi_size : int; (* Size of public inputs. *)
  input_flag : trace_kind;
  (* Flag indicating the type of inputs we are expecting. Inputs must
     come in order: (i) InputCom; (ii) Public; (iii) Private;
     (iv) NoInput (corresponding to intermediary variables).
     If we receive an input with earlier precedence than [input_flag], an
     error should be raised. *)
  check_wires : bool repr list;
  (* Boolean wires to be checked.
     If at the end of the circuit ([get_cs]) this is not empty, their
     conjunction will be asserted. *)
  delayed : state -> state * unit repr;
      (* Delayed computation, used to dump the implicit checks at the end.
         This is necessary because some implicit checks on the inputs might
         create intermediary variables, which would set to false the [input_flag]
         before some inputs are processed.
      *)
  tables : string list;
  solver : Solver.t;
  range_checks : Range_checks.t;
  (* label trace that creates a range-check and the size of the range-check *)
  range_checks_labels : (string list * int) list;
  (* label trace *)
  labels : string list;
  (* one and zero are used so often that it's worth reusing them across the
     whole circuit. Num.constant uses these two values as cache, which in turn
     is used by Bool.constant and Bytes.constant and leads to important
     reduction in circuit size. *)
  cache : scalar repr Scalar_map.t;
}

(* A Plompiler program is just a state monad. *)
type 'a t = state -> state * 'a

let ret : 'a -> 'a t = fun x s -> (s, x)

let ( let* ) : 'a t -> ('a -> 'b t) -> 'b t =
 fun m f s ->
  let s, o = m s in
  f o s

let unscalar (Scalar s) = s

(* Monadic bind that unwraps a scalar repr. *)
let ( let*& ) : scalar repr t -> (int -> 'b repr t) -> 'b repr t =
 fun m f ->
  let* m in
  f (unscalar m)

let ( >* ) m f =
  let* Unit = m in
  f

let fmap : ('a -> 'b) -> 'a t -> 'b t =
 fun f m ->
  let* m in
  ret (f m)

let ( <$> ) m f = fmap f m

let rec foldM f e l =
  match l with
  | [] -> ret e
  | x :: xs ->
      let* y = f e x in
      foldM f y xs

let rec mapM : ('a -> 'b t) -> 'a list -> 'b list t =
 fun f ls ->
  match ls with
  | [] -> ret @@ []
  | l :: ls ->
      let* o = f l in
      let* rest = mapM f ls in
      ret @@ (o :: rest)

let map2M f ls rs = mapM (fun (a, b) -> f a b) (List.combine ls rs)

let rec iterM f l =
  match l with
  | [] -> ret Unit
  | x :: xs ->
      let* _ = f x in
      iterM f xs

let iter2M f ls rs =
  let lrs =
    try List.combine ls rs
    with Invalid_argument _ ->
      failwith "iter2M: inputs are of different length"
  in
  iterM (fun (a, b) -> f a b) lrs

let with_bool_check : bool repr t -> unit repr t =
 fun c s ->
  let s, b = c s in
  ({s with check_wires = b :: s.check_wires}, Unit)

module Input = struct
  (* Checks to be performed on an input *)
  type 'a implicit_check = 'a repr -> unit repr t

  let default_check : 'a implicit_check = fun _ -> ret Unit

  (* Structured inputs *)
  type 'a t' =
    | U : unit t'
    | S : scalar -> scalar t'
    | B : bool -> bool t'
    | P : 'a t' * 'b t' -> ('a * 'b) t'
    | L : 'a t' list -> 'a list t'

  and 'a input = 'a t' * 'a implicit_check

  type 'a t = 'a input

  let with_implicit_bool_check bc (i, a) =
    let check x s =
      ({s with delayed = s.delayed >* with_bool_check (bc x)}, Unit)
    in
    (i, fun repr -> a repr >* check repr)

  let with_assertion na (i, a) =
    let delay_assertion x s = ({s with delayed = s.delayed >* na x}, Unit) in
    (i, fun repr -> a repr >* delay_assertion repr)

  let s x = (S (X x), default_check)

  let scalar x = s x

  let to_scalar (S (X x), _) = x

  let bool b = (B b, default_check)

  let to_bool (B b, _) = b

  let unit = (U, default_check)

  let pair : 'a t -> 'b t -> ('a * 'b) t =
   fun (a, check_a) (b, check_b) ->
    (P (a, b), fun (Pair (ar, br)) -> check_a ar >* check_b br)

  let to_pair (P (a, b), _) = ((a, default_check), (b, default_check))

  let list : 'a t list -> 'a list t =
   fun l ->
    ( L (List.map fst l),
      fun (List lr) ->
        let* _l =
          mapM (fun ((_, asssertion), r) -> asssertion r) (List.combine l lr)
        in
        ret Unit )

  let to_list (L l, _) = List.map (fun i -> (i, default_check)) l

  (* Traverse the input structure, replacing the scalars
     with increasing indices starting from [start]. *)
  let rec make_repr : type a. a t' -> int -> a repr * int =
   fun input start ->
    match input with
    | U -> (Unit, start)
    | S _ -> (Scalar start, start + 1)
    | B _ -> (Bool start, start + 1)
    | P (l, r) ->
        let l, m = make_repr l start in
        let r, e = make_repr r m in
        (Pair (l, r), e)
    | L l ->
        let l, e =
          List.fold_left
            (fun (l, i) x ->
              let r, i' = make_repr x i in
              (r :: l, i'))
            ([], start)
            l
        in
        (List (List.rev l), e)
end

(* Dummy inputs, useful for computing a circuit before knowning
   the actual inputs. *)
module Dummy = struct
  let scalar = Input.(S (X S.zero))

  let bool = Input.B false

  let list n a = Input.L (List.init n (fun _ -> a))
end

let rec encode : type a. a Input.t' -> S.t list =
 fun input ->
  match input with
  | U -> []
  | S (X s) -> [s]
  | B b -> if b then [S.one] else [S.zero]
  | P (l, r) -> encode l @ encode r
  | L l -> List.concat_map encode l

let serialize i = Array.of_list @@ encode i

(* Physical equality: the reprs have the same structure
   and use the same wires. *)
let rec eq : type a. a repr -> a repr -> bool =
 fun a b ->
  match (a, b) with
  | Scalar a, Scalar b | Bool a, Bool b -> a = b
  | Pair (al, ar), Pair (bl, br) -> eq al bl && eq ar br
  | List l1, List l2 -> List.for_all2 eq l1 l2
  | Unit, Unit -> true

let pair l r = Pair (l, r)

let of_pair (Pair (l, r)) = (l, r)

let to_list l = List l

let of_list (List l) = l

let with_label ~label m s =
  let s' = {s with labels = label :: s.labels} in
  let s'', a = m s' in
  ({s'' with labels = s.labels}, a)

let debug _ _ = ret Unit

let add_solver : solver:Solver.solver_desc -> unit repr t =
 fun ~solver s -> ({s with solver = Solver.append_solver solver s.solver}, Unit)

let default_solver ?(to_solve = W 2) g =
  let c = g.(0) in
  let get_sel = CS.(get_sel c.sels) in
  let linear =
    Array.init Csir.nb_wires_arch (fun i ->
        get_sel @@ Csir.linear_selector_name i)
  in
  Arith
    {
      wires = Array.map (fun i -> R i) c.wires;
      linear;
      qc = get_sel "qc";
      qm = get_sel "qm";
      qx5a = get_sel "qx5a";
      qx2b = get_sel "qx2b";
      to_solve;
    }

(* Add a gate to the constraint system *)
let append : CS.gate -> ?solver:Solver.solver_desc -> unit repr t =
 fun gate ?solver s ->
  let solver =
    match solver with
    | Some s -> s
    | None -> if Array.length gate = 1 then default_solver gate else Skip
  in
  let gate =
    Array.map (fun c -> Csir.CS.{c with label = c.label @ s.labels}) gate
  in
  let cs = gate :: s.cs in
  let solver = Solver.append_solver solver s.solver in
  ({s with cs; solver}, Unit)

(* Add a lookup to the CS *)
let append_lookup :
    wires:int tagged list -> table:string -> string -> unit repr t =
 fun ~wires ~table label s ->
  let rec find_index : 'a list -> int -> 'a -> int option =
   fun l i y ->
    match l with
    | [] -> None
    | x :: xs -> if x = y then Some i else find_index xs (i + 1) y
  in
  let use_table s id =
    match find_index s.tables 0 id with
    | Some i -> (s, i)
    | None ->
        let i = List.length s.tables in
        let tables = s.tables @ [id] in
        ({s with tables}, i)
  in
  let s, index = use_table s table in
  let wires = Array.of_list wires in
  let solver = Lookup {wires; table} in
  let wires = Array.map untag wires in
  let cstr =
    CS.new_constraint
      ~wires:(Array.to_list wires)
      ~q_plookup:S.one
      ~q_table:(S.of_z (Z.of_int index))
      ~labels:s.labels
      label
  in
  let cs = [|cstr|] :: s.cs in
  ({s with cs; solver = Solver.append_solver solver s.solver}, Unit)

(* Records inputs, for external use *)
let input : type a. ?kind:input_kind -> a Input.t -> a repr t =
 fun ?(kind = `Private) inp ->
  let rec aux : type a. kind:input_kind -> a Input.t' -> a repr t =
   fun ~kind inp s ->
    if compare_trace_kind (kind :> trace_kind) s.input_flag < 0 then
      raise
      @@ Invalid_argument
           (Format.sprintf
              "Input order not respected : input_kind : %s, s.input_flag : %s; \
               inputs must be declared in this order : `InputCom, `Public, \
               `Private"
              (show_input_kind kind)
              (show_trace_kind s.input_flag)) ;
    let serialized = serialize inp in
    let n = Array.length serialized in
    let inputs = Array.append s.inputs serialized in
    let input_flag = (kind :> trace_kind) in
    let input_com_sizes =
      match kind with
      | `InputCom -> (
          match s.input_com_sizes with
          | hd :: tl -> (hd + n) :: tl
          | _ ->
              raise
              @@ Failure
                   "initialize inputs commitments with new_input_commitment")
      | _ -> s.input_com_sizes
    in
    let pi_size = s.pi_size + match kind with `Public -> n | _ -> 0 in
    match inp with
    | Input.U -> assert false
    | Input.S _ ->
        let r, nvars = Input.make_repr inp s.nvars in
        ({s with nvars; inputs; input_com_sizes; pi_size; input_flag}, r)
    | Input.B _ ->
        let Bool o, nvars = Input.make_repr inp s.nvars in
        let s =
          {
            s with
            delayed =
              s.delayed
              >* append
                   [|
                     CS.new_constraint
                       ~wires:[o; o; 0]
                       ~linear:[(wql, mone)]
                       ~qm:one
                       "bool";
                   |]
                   ~solver:Skip;
            nvars;
            inputs;
            input_com_sizes;
            pi_size;
            input_flag;
          }
        in
        (s, Bool o)
    | Input.P (l, r) ->
        (let* l = aux ~kind l in
         let* r = aux ~kind r in
         ret @@ Pair (l, r))
          s
    | Input.L ls ->
        (let* l = mapM (aux ~kind) ls in
         ret @@ List l)
          s
  in
  let inp, implicit_check = inp in
  let* i = with_label ~label:"Core.input" @@ aux ~kind inp in
  implicit_check i >* ret i

let new_input_com : unit repr t =
 fun s -> ({s with input_com_sizes = 0 :: s.input_com_sizes}, Unit)

type 'b open_input_com = 'b t

let begin_input_com : 'b -> 'b open_input_com = fun b -> new_input_com >* ret b

let ( |: ) : type c d.
    (c repr -> d) open_input_com -> c Input.t -> d open_input_com =
 fun v i s ->
  let s, f = v s in
  let s, r = (input ~kind:`InputCom i) s in
  (s, f r)

let end_input_com : 'a open_input_com -> 'a t = Fun.id

(* Doesn't record inputs, for interal use *)
let fresh : type a. a Input.t' -> a repr t =
  let rec aux : type a. a Input.t' -> a repr t =
   fun input s ->
    let s = {s with input_flag = `NoInput} in
    match input with
    | Input.U | Input.S _ ->
        let r, nvars = Input.make_repr input s.nvars in
        ({s with nvars}, r)
    | Input.B _ ->
        let Bool o, nvars = Input.make_repr input s.nvars in
        let s, _ =
          append
            [|
              CS.new_constraint
                ~wires:[o; o; 0]
                ~linear:[(wql, mone)]
                ~qm:one
                "bool";
            |]
            ~solver:Skip
            {s with nvars}
        in
        (s, Bool o)
    | Input.P (l, r) ->
        (let* l = aux l in
         let* r = aux r in
         ret @@ Pair (l, r))
          s
    | Input.L ls ->
        (let* l = mapM aux ls in
         ret @@ List l)
          s
  in
  fun input -> with_label ~label:"Core.fresh" @@ aux input

let serialize (i, _) = serialize i

let deserialize : type a. S.t array -> a Input.t -> a Input.t =
  let rec aux : type a. S.t array -> a Input.t' -> int -> a Input.t' * int =
   fun a w i ->
    let open Input in
    match w with
    | U -> (U, i)
    | S _ ->
        let s = a.(i) in
        (S (X s), i + 1)
    | B _ ->
        let s = a.(i) in
        (B (S.is_one s), i + 1)
    | P (wl, wr) ->
        let l, i = aux a wl i in
        let r, i = aux a wr i in
        (P (l, r), i)
    | L ws ->
        let l, i =
          List.fold_left
            (fun (acc, i) w ->
              let x, i = aux a w i in
              (x :: acc, i))
            ([], i)
            ws
        in
        (L (List.rev l), i)
  in
  fun a (w, check) -> (fst @@ aux a w 0, check)

let scalar_of_bool (Bool b) = Scalar b

let unsafe_bool_of_scalar (Scalar b) = Bool b

let unit = Unit

module Num = struct
  type nonrec scalar = scalar

  type nonrec 'a repr = 'a repr

  type nonrec 'a t = 'a t

  (* checks that 0 <= (Scalar l) < 2^nb_bits *)
  let range_check ~nb_bits (Scalar l) s =
    assert (nb_bits > 0) ;
    let range_checks_labels = (s.labels, nb_bits) :: s.range_checks_labels in
    ( {
        s with
        range_checks = Range_checks.add ~nb_bits l s.range_checks;
        range_checks_labels;
      },
      Unit )

  (* l ≠ 0  <=>  ∃ r ≠ 0 : l * r - 1 = 0 *)
  let assert_nonzero (Scalar l) =
    let*& r = fresh Dummy.scalar in
    (* 0*l + 0*r + 0*0 + 1*l*r -1 = 0 *)
    let gate =
      [|CS.new_constraint ~wires:[l; r; 0] ~qc:mone ~qm:one "assert_nonzero"|]
    in
    let solver = default_solver gate ~to_solve:(W 1) in
    append gate ~solver

  let is_zero (Scalar l) =
    with_label ~label:"Num.is_zero"
    @@ let* (Bool bit) = fresh Dummy.bool in
       let* (Scalar r) = fresh Dummy.scalar in
       let gate =
         [|
           CS.new_constraint
             ~wires:[l; r; bit]
             ~qc:mone
             ~linear:[(wqo, one)]
             ~qm:one
             "is_zero";
         |]
       in
       let solver = IsZero [|l; r; bit|] in
       append gate ~solver >* assert_nonzero (Scalar r) >* ret @@ Bool bit

  let is_not_zero (Scalar l) =
    with_label ~label:"Num.is_not_zero"
    @@ let* (Bool bit) = fresh Dummy.bool in
       let* (Scalar r) = fresh Dummy.scalar in
       let gate =
         [|
           CS.new_constraint
             ~wires:[l; r; bit]
             ~linear:[(wqo, mone)]
             ~qm:one
             "is_not_zero";
         |]
       in
       let solver = IsNotZero [|l; r; bit|] in
       append gate ~solver >* assert_nonzero (Scalar r) >* ret @@ Bool bit

  let assert_bool (Scalar l) =
    with_label ~label:"Num.assert_bool"
    @@
    let gate = [|CS.new_constraint ~wires:[l] ~qbool:one "assert_bool"|] in
    let solver = Skip in
    append gate ~solver

  let custom ?(qc = S.zero) ?(ql = S.zero) ?(qr = S.zero) ?(qo = S.mone)
      ?(qm = S.zero) ?qx2b ?qx5a (Scalar l) (Scalar r) =
    let*& o = fresh Dummy.scalar in
    append
      [|
        CS.new_constraint
          ~wires:[l; r; o]
          ~qc
          ~linear:[(wql, ql); (wqr, qr); (wqo, qo)]
          ~qm
          ?qx2b
          ?qx5a
          "custom";
      |]
    >* ret @@ Scalar o

  let assert_custom ?(qc = S.zero) ?(ql = S.zero) ?(qr = S.zero) ?(qo = S.zero)
      ?(qm = S.zero) (Scalar l) (Scalar r) (Scalar o) =
    append
      [|
        CS.new_constraint
          ~wires:[l; r; o]
          ~qc
          ~linear:[(wql, ql); (wqr, qr); (wqo, qo)]
          ~qm
          "assert_custom";
      |]
      ~solver:Skip

  let add ?(qc = S.zero) ?(ql = S.one) ?(qr = S.one) (Scalar l) (Scalar r) =
    let*& o = fresh Dummy.scalar in
    append
      [|
        CS.new_constraint
          ~wires:[l; r; o]
          ~qc
          ~linear:[(wql, ql); (wqr, qr); (wqo, mone)]
          "add";
      |]
    >* ret @@ Scalar o

  let add_constant ?(ql = S.one) (k : S.t) (Scalar l) =
    let*& o = fresh Dummy.scalar in
    append
      [|
        CS.new_constraint
          ~wires:[l; 0; o]
          ~qc:k
          ~linear:[(wql, ql); (wqo, mone)]
          "add_constant";
      |]
    >* ret @@ Scalar o

  let sub (Scalar l) (Scalar r) =
    let*& o = fresh Dummy.scalar in
    append
      [|
        CS.new_constraint
          ~wires:[l; r; o]
          ~linear:[(wql, one); (wqr, mone); (wqo, mone)]
          "sub";
      |]
    >* ret @@ Scalar o

  let mul ?(qm = one) (Scalar l) (Scalar r) =
    let*& o = fresh Dummy.scalar in
    append
      [|CS.new_constraint ~wires:[l; r; o] ~qm ~linear:[(wqo, mone)] "mul"|]
    >* ret @@ Scalar o

  let div ?(den_coeff = one) (Scalar l) (Scalar r) =
    with_label ~label:"Num.div" @@ assert_nonzero (Scalar r)
    >* let*& o = fresh Dummy.scalar in
       let gate =
         [|
           CS.new_constraint
             ~wires:[r; o; l]
             ~qm:den_coeff
             ~linear:[(wqo, mone)]
             "div";
         |]
       in
       let solver = default_solver gate ~to_solve:(W 1) in
       (* r * o - l = 0  <=> o = l / r *)
       append gate ~solver >* ret @@ Scalar o

  let pow5 (Scalar l) =
    let*& o = fresh Dummy.scalar in
    let gate =
      [|
        CS.new_constraint
          ~wires:[l; 0; o]
          ~qx5a:one
          ~linear:[(wqo, mone)]
          "pow5";
      |]
    in
    let solver = Pow5 {a = l; c = o} in
    append gate ~solver >* ret @@ Scalar o

  let constant_aux s =
    let*& o = fresh Dummy.scalar in
    append
      [|
        CS.new_constraint
          ~wires:[0; 0; o]
          ~qc:s
          ~linear:[(wqo, mone)]
          "constant_scalar";
      |]
    >* ret (Scalar o)

  (* cache zero and one otherwise just add a fresh constraint *)
  let constant x : scalar repr t =
   fun st ->
    match Scalar_map.find_opt x st.cache with
    | None ->
        let st, o = constant_aux x st in
        let cache = Scalar_map.add x o st.cache in
        ({st with cache}, o)
    | Some o -> (st, o)

  let zero = constant S.zero

  let one = constant S.one
end

module Bool = struct
  type nonrec scalar = scalar

  type nonrec 'a repr = 'a repr

  type nonrec 'a t = 'a t

  let constant : bool -> bool repr t =
   fun b ->
    let s = if b then S.one else S.zero in
    let* (Scalar s) = Num.constant s in
    ret (Bool s)

  let assert_true (Bool bit) =
    append
      [|
        CS.new_constraint
          ~wires:[bit]
          ~qc:mone
          ~linear:[(wql, one)]
          "assert_true";
      |]
      ~solver:Skip

  let assert_false (Bool bit) =
    append
      [|CS.new_constraint ~wires:[bit] ~linear:[(wql, one)] "assert_false"|]
      ~solver:Skip

  let band : bool repr -> bool repr -> bool repr t =
   fun (Bool l) (Bool r) ->
    (* NB: Here [o] is declared as a fresh scalar to avoid adding the constraint
        asserting it's a bool. It's safe to do so because:
        - We can assume that [l] and [r] are booleans
        - This operation is closed in {0, 1}
       This has additionally been proven using Z3 (see z3 directory).
    *)
    let*& o = fresh Dummy.scalar in
    (* o - l*r = 0 *)
    append
      [|
        CS.new_constraint ~wires:[l; r; o] ~qm:mone ~linear:[(wqo, one)] "band";
      |]
    >* ret @@ Bool o

  let bnot (Bool b) =
    (* NB: Here [o] is declared as a fresh scalar to avoid adding the constraint
        asserting it's a bool. It's safe to do so because:
        - We can assume that [b] is a boolen.
        - This operation is closed in {0, 1}
       This has additionally been proven using Z3 (see z3 directory).
    *)
    let*& o = fresh Dummy.scalar in
    (* o - (1 - i) = 0 *)
    append
      [|
        CS.new_constraint
          ~wires:[b; 0; o]
          ~qc:mone
          ~linear:[(wql, one); (wqo, one)]
          "bnot";
      |]
    >* ret @@ Bool o

  let xor (Bool l) (Bool r) =
    (* NB: Here [o] is declared as a fresh scalar to avoid adding the constraint
        asserting it's a bool. It's safe to do so because:
        - We can assume that [l] and [r] are booleans
        - This operation is closed in {0, 1}
       This has additionally been proven using Z3 (see z3 directory).
    *)
    let*& o = fresh Dummy.scalar in
    let mtwo = S.of_string "-2" in
    append
      [|
        CS.new_constraint
          ~wires:[l; r; o]
          ~linear:[(wql, one); (wqr, one); (wqo, mone)]
          ~qm:mtwo
          "xor";
      |]
    >* ret @@ Bool o

  let bor (Bool l) (Bool r) =
    (* NB: Here [o] is declared as a fresh scalar to avoid adding the constraint
        asserting it's a bool. It's safe to do so because:
        - We can assume that [l] and [r] are booleans
        - This operation is closed in {0, 1}
       This has additionally been proven using Z3 (see z3 directory).
    *)
    let*& o = fresh Dummy.scalar in
    append
      [|
        CS.new_constraint
          ~wires:[l; r; o]
          ~linear:[(wql, one); (wqr, one); (wqo, mone)]
          ~qm:mone
          "nor";
      |]
    >* ret @@ Bool o

  let swap : type a. bool repr -> a repr -> a repr -> (a * a) repr t =
    let scalar_swap (Bool b) (Scalar x) (Scalar y) =
      let*& u = fresh Dummy.scalar in
      let*& v = fresh Dummy.scalar in
      let solver = Swap {b; x; y; u; v} in
      let gate =
        [|CS.new_constraint ~wires:[b; x; y; u; v] ~qcond_swap:one "swap"|]
      in
      append gate ~solver >* ret @@ pair (Scalar u) (Scalar v)
    in
    let rec aux : type a. bool repr -> a repr -> a repr -> (a * a) repr t =
     fun b x y ->
      match (x, y) with
      | Unit, Unit -> ret (pair Unit Unit)
      | Scalar _, Scalar _ -> scalar_swap b x y
      | Bool _, Bool _ ->
          let* res = scalar_swap b (scalar_of_bool x) (scalar_of_bool y) in
          let Scalar u, Scalar v = of_pair res in
          ret @@ pair (Bool u) (Bool v)
      | Pair (x1, y1), Pair (x2, y2) ->
          let* res_x = aux b x1 x2 in
          let* res_y = aux b y1 y2 in
          let (u1, v1), (u2, v2) = (of_pair res_x, of_pair res_y) in
          ret @@ pair (pair u1 u2) (pair v1 v2)
      | List ls, List rs ->
          let* l = map2M (fun l r -> aux b l r) ls rs in
          let l1, l2 = List.(map of_pair l |> split) in
          ret @@ pair (List l1) (List l2)
    in
    fun b x y -> with_label ~label:"Bool.swap" @@ aux b x y

  let ifthenelse : type a. bool repr -> a repr -> a repr -> a repr t =
    let aux b l r =
      let* swapped = swap b l r in
      let _, res = of_pair swapped in
      ret res
    in
    fun b l r -> with_label ~label:"Bool.ifthenelse" @@ aux b l r

  let is_eq_const l s =
    with_label ~label:"Bool.is_eq_const"
    @@ let* diff = Num.add_constant ~ql:S.mone s l in
       Num.is_zero diff

  let band_list l =
    with_label ~label:"Bool.band_list"
    @@
    match l with
    | [] -> constant true
    | hd :: tl ->
        let* sum =
          foldM Num.add (scalar_of_bool hd) (List.map scalar_of_bool tl)
        in
        is_eq_const sum (S.of_int @@ (List.length tl + 1))

  module Internal = struct
    let bor_lookup (Bool l) (Bool r) =
      let* (Bool o) = fresh Dummy.bool in
      append_lookup ~wires:[Input l; Input r; Output o] ~table:"or" "bor lookup"
      >* ret @@ Bool o

    let xor_lookup (Bool l) (Bool r) =
      let* (Bool o) = fresh Dummy.bool in
      append_lookup
        ~wires:[Input l; Input r; Output o]
        ~table:"xor"
        "xor lookup"
      >* ret @@ Bool o

    let band_lookup (Bool l) (Bool r) =
      let* (Bool o) = fresh Dummy.bool in
      append_lookup
        ~wires:[Input l; Input r; Output o]
        ~table:"band"
        "band lookup"
      >* ret @@ Bool o

    let bnot_lookup (Bool b) =
      let* (Bool o) = fresh Dummy.bool in
      let* (Scalar zero) = fresh Dummy.scalar in
      append_lookup
        ~wires:[Input b; Input zero; Output o]
        ~table:"bnot"
        "bnot lookup"
      >* ret @@ Bool o
  end
end

module Limb (N : sig
  val nb_bits : int
end) =
struct
  let nb_bits = N.nb_bits

  let xor_lookup (Scalar l) (Scalar r) =
    let* (Scalar o) = fresh Dummy.scalar in
    append_lookup
      ~wires:[Input l; Input r; Output o]
      ~table:("xor" ^ Int.to_string nb_bits)
      ("xor lookup" ^ Int.to_string nb_bits)
    >* ret @@ Scalar o

  let band_lookup (Scalar l) (Scalar r) =
    let* (Scalar o) = fresh Dummy.scalar in
    append_lookup
      ~wires:[Input l; Input r; Output o]
      ~table:("band" ^ Int.to_string nb_bits)
      ("band lookup" ^ Int.to_string nb_bits)
    >* ret @@ Scalar o

  let bnot_lookup (Scalar l) =
    let* (Scalar o) = fresh Dummy.scalar in
    let* (Scalar zero) = fresh Dummy.scalar in
    append_lookup
      ~wires:[Input l; Input zero; Output o]
      ~table:("bnot" ^ Int.to_string nb_bits)
      ("bnot lookup" ^ Int.to_string nb_bits)
    >* ret @@ Scalar o

  let rotate_right_lookup (Scalar l) (Scalar r) i =
    let* (Scalar o) = fresh Dummy.scalar in
    let nb_bits_i = Int.to_string nb_bits ^ "_" ^ Int.to_string i in
    append_lookup
      ~wires:[Input l; Input r; Output o]
      ~table:("rotate_right" ^ nb_bits_i)
      ("rotate_right lookup" ^ nb_bits_i)
    >* ret @@ Scalar o
end

let hd (List l) = match l with [] -> assert false | x :: _ -> ret x

let assert_equal : type a. a repr -> a repr -> unit repr t =
  let rec aux : type a. a repr -> a repr -> unit repr t =
   fun a b ->
    match (a, b) with
    | Unit, Unit -> ret Unit
    | Bool a, Bool b | Scalar a, Scalar b ->
        append
          [|
            CS.new_constraint
              ~wires:[0; b; a]
              ~linear:[(wqr, one); (wqo, mone)]
              "assert_equal";
          |]
          ~solver:Skip
    | Pair (la, ra), Pair (lb, rb) -> aux la lb >* aux ra rb
    | List ls, List rs -> iter2M aux ls rs
  in
  fun a b -> with_label ~label:"Core.assert_equal" @@ aux a b

let equal : type a. a repr -> a repr -> bool repr t =
  let rec aux : type a. a repr -> a repr -> bool repr t =
   fun a b ->
    let open Bool in
    let open Num in
    match (a, b) with
    | Unit, Unit -> Bool.constant true
    | Bool a, Bool b ->
        let* s = sub (Scalar a) (Scalar b) in
        is_zero s
    | Scalar _, Scalar _ ->
        let* s = sub a b in
        is_zero s
    | Pair (la, ra), Pair (lb, rb) ->
        let* le = aux la lb in
        let* re = aux ra rb in
        band le re
    | List ls, List rs ->
        let lrs = List.map2 pair ls rs in
        let* acc = Bool.constant true in
        foldM
          (fun acc (Pair (l, r)) ->
            let* e = aux l r in
            band acc e)
          acc
          lrs
  in
  fun a b -> with_label ~label:"Core.equal" @@ aux a b

let scalar_of_limbs ~nb_bits b =
  let sb = of_list b in
  let powers =
    let nb_limbs = List.length sb in
    let base = 1 lsl nb_bits |> Z.of_int in
    List.init nb_limbs (fun i -> S.of_z @@ Z.pow base i)
  in
  foldM
    (fun acc (qr, w) -> Num.add ~qr acc w)
    (List.hd sb)
    List.(tl @@ combine powers sb)

(* If [add_alpha], this function returns the binary decomposition of
   [l + Utils.alpha], instead of the binary decompostion of [l], where
   Utils.alpha is the difference between Scalar.order and its succeeding
   power of 2 *)
let bits_of_scalar ?(shift = Z.zero) ~nb_bits (Scalar l) =
  with_label ~label:"Core.bits_of_scalar"
  @@ let* bits = fresh @@ Dummy.list nb_bits Dummy.bool in
     add_solver
       ~solver:
         (BitsOfS
            {
              nb_bits;
              shift;
              l;
              bits = List.map (fun (Bool x) -> x) @@ of_list bits;
            })
     >* let* sum =
          let sbits = List.map scalar_of_bool (of_list bits) in
          scalar_of_limbs ~nb_bits:1 (to_list sbits)
        in
        let* l =
          if Z.(not @@ equal shift zero) then
            Num.add_constant (S.of_z shift) (Scalar l)
          else ret (Scalar l)
        in
        assert_equal l sum >* ret bits

let limbs_of_scalar ?(shift = Z.zero) ~total_nb_bits ~nb_bits (Scalar l) =
  with_label ~label:"Core.limbs_of_scalar"
  @@
  let nb_limbs = total_nb_bits / nb_bits in
  let* limbs = fresh @@ Dummy.list nb_limbs Dummy.scalar in
  add_solver
    ~solver:
      (LimbsOfS
         {
           total_nb_bits;
           nb_bits;
           shift;
           l;
           limbs = List.map (fun (Scalar x) -> x) @@ of_list limbs;
         })
  >* let* sum = scalar_of_limbs ~nb_bits limbs in
     let* l =
       if Z.(not @@ equal shift zero) then
         Num.add_constant (S.of_z shift) (Scalar l)
       else ret (Scalar l)
     in
     iterM (Num.range_check ~nb_bits) (of_list limbs)
     >* assert_equal l sum >* ret limbs

module Ecc = struct
  let weierstrass_add (Pair (Scalar x1, Scalar y1))
      (Pair (Scalar x2, Scalar y2)) =
    with_label ~label:"Ecc.weierstrass_add"
    @@ let*& x3 = fresh Dummy.scalar in
       let*& y3 = fresh Dummy.scalar in
       let gate =
         [|
           CS.new_constraint
             ~wires:[x1; x2; x3]
             ~qecc_ws_add:one
             "weierstrass-add.1";
           CS.new_constraint ~wires:[y1; y2; y3] "weierstrass-add.2";
         |]
       in
       let solver = Ecc_Ws {x1; x2; x3; y1; y2; y3} in
       append gate ~solver >* ret (Pair (Scalar x3, Scalar y3))

  let edwards_add (Pair (Scalar x1, Scalar y1)) (Pair (Scalar x2, Scalar y2)) =
    (* Improve Me: Functorize to pass curve in parameter. *)
    with_label ~label:"Ecc.edwards_add"
    @@
    let module W = Mec.Curve.Jubjub.AffineEdwards in
    let s_of_base s = S.of_z (W.Base.to_z s) in
    let a, d = (s_of_base W.a, s_of_base W.d) in
    let*& x3 = fresh Dummy.scalar in
    let*& y3 = fresh Dummy.scalar in
    let gate =
      [|
        CS.new_constraint ~wires:[x1; x2; x3] ~qecc_ed_add:one "edwards-add.1";
        CS.new_constraint ~wires:[y1; y2; y3] "edwards-add.2";
      |]
    in
    let solver = Ecc_Ed {x1; x2; x3; y1; y2; y3; a; d} in
    append gate ~solver >* ret (Pair (Scalar x3, Scalar y3))

  let edwards_cond_add (Pair (Scalar x1, Scalar y1))
      (Pair (Scalar x2, Scalar y2)) (Bool bit) =
    (* Improve Me: Functorize to pass curve in parameter. *)
    with_label ~label:"Ecc.edwards_cond_add"
    @@
    let module W = Mec.Curve.Jubjub.AffineEdwards in
    let s_of_base s = S.of_z (W.Base.to_z s) in
    let a, d = (s_of_base W.a, s_of_base W.d) in
    let*& x3 = fresh Dummy.scalar in
    let*& y3 = fresh Dummy.scalar in
    let gate =
      [|
        CS.new_constraint
          ~wires:[bit; x2; y2; x1; y1]
          ~qecc_ed_cond_add:one
          "edwards-cond-add.1";
        CS.new_constraint ~wires:[0; 0; 0; x3; y3] "edwards-cond-add.2";
      |]
    in
    let solver = Ecc_Cond_Ed {x1; x2; x3; y1; y2; y3; bit; a; d} in
    append gate ~solver >* ret (Pair (Scalar x3, Scalar y3))
end

module Mod_arith = struct
  (* Refer to [lib_plompiler/gadget_mod_arith.ml] for documentation on
     modular arithmetic and details about all parameters:
     [modulus], [nb_limbs], [base], [moduli], [qm_bound] and [ts_bounds] *)
  let add ?(subtraction = false) ~label ~modulus ~nb_limbs ~base ~moduli
      ~qm_bound ~ts_bounds (List xs) (List ys) =
    (* This is just a sanity check, inputs are assumed to be well-formed,
       in particular, their limb values are in the range [0, base) *)
    assert (List.compare_length_with xs nb_limbs = 0) ;
    assert (List.compare_length_with ys nb_limbs = 0) ;
    (* Assert that all bounds are compatible with our range-check protocol,
       which is designed to check membership in intervals of the form [0, 2^k) *)
    let qm_ubound = snd qm_bound in
    let ts_ubounds = List.map snd ts_bounds in
    assert (List.for_all Utils.is_power_of_2 (base :: qm_ubound :: ts_ubounds)) ;
    let label_suffix = if subtraction then "sub" else "add" in
    (* Create the corresponding constraints *)
    with_label ~label:("Mod_arith." ^ label_suffix)
    @@ let* zs = fresh @@ Dummy.list nb_limbs Dummy.scalar in
       let* qm = fresh Dummy.scalar in
       let* ts = fresh @@ Dummy.list (List.length moduli) Dummy.scalar in
       let inp1 = List.map unscalar xs in
       let inp2 = List.map unscalar ys in
       let out = List.map unscalar (of_list zs) in
       let scalar_qm = qm in
       let scalar_ts = of_list ts in
       let qm = unscalar qm in
       let ts = List.map unscalar scalar_ts in
       let gate =
         (* Substracions zs = xs - ys are modeled as additions xs = zs + ys.
            Thus, we swap inp1 (xs) and out (zs) when subtraction = true. *)
         let left_row1 = if subtraction then out else inp1 in
         let left_row2 = if subtraction then inp1 else out in
         [|
           CS.new_constraint
             ~wires:(left_row1 @ inp2)
             ~q_mod_add:[(label, one)]
             ("mod_arith-" ^ label_suffix ^ ".1");
           CS.new_constraint
             ~wires:(left_row2 @ [qm] @ ts)
             ("mod_arith-" ^ label_suffix ^ ".2");
         |]
       in
       let solver =
         Mod_Add
           {
             modulus;
             base;
             nb_limbs;
             moduli;
             qm_bound;
             ts_bounds;
             inp1;
             inp2;
             out;
             qm;
             ts;
             inverse = subtraction;
           }
       in
       (* The output is not assumed to be well-formed, we need to enforce this
          with constraints. In particular, we need to range-check every limb
          in the output in the range [0, base). *)
       iterM (Num.range_check ~nb_bits:(Z.log2 base)) (of_list zs)
       (* qm needs to be range-checked in the interval [0, qm_ubound) *)
       >* Num.range_check ~nb_bits:(Z.log2 qm_ubound) scalar_qm
          (* every tj needs to be range-checked in the interval [0, tj_ubound) *)
       >* iter2M
            (fun tj_ubound tj -> Num.range_check ~nb_bits:(Z.log2 tj_ubound) tj)
            ts_ubounds
            scalar_ts
       >* append gate ~solver >* ret zs

  (* This function is also used for division, since we implement division
     [z = x / y] as a multiplication [x = z * y]. However, one must be careful,
     as this does not prevent "division by 0", i.e., when [y = 0], constraint
     [x = z * y] is satisfiable for [x = 0]. Therefore, in the gadget for
     division we will need to explicitly assert that [y <> 0]. *)
  let mul ?(division = false) ~label ~modulus ~nb_limbs ~base ~moduli ~qm_bound
      ~ts_bounds (List xs) (List ys) =
    (* This is just a sanity check, inputs are assumed to be well-formed,
       in particular, their limb values are in the range [0, base) *)
    assert (List.compare_length_with xs nb_limbs = 0) ;
    assert (List.compare_length_with ys nb_limbs = 0) ;
    (* Assert that all bounds are compatible with our range-check protocol,
       which is designed to check membership in intervals of the form [0, 2^k) *)
    let qm_ubound = snd qm_bound in
    let ts_ubounds = List.map snd ts_bounds in
    assert (List.for_all Utils.is_power_of_2 (base :: qm_ubound :: ts_ubounds)) ;
    let label_suffix = if division then "div" else "mul" in
    (* Create the corresponding constraints *)
    with_label ~label:("Mod_arith." ^ label_suffix)
    @@ let* zs = fresh @@ Dummy.list nb_limbs Dummy.scalar in
       let* qm = fresh Dummy.scalar in
       let* ts = fresh @@ Dummy.list (List.length moduli) Dummy.scalar in
       let inp1 = List.map unscalar xs in
       let inp2 = List.map unscalar ys in
       let out = List.map unscalar (of_list zs) in
       let scalar_qm = qm in
       let scalar_ts = of_list ts in
       let qm = unscalar qm in
       let ts = List.map unscalar scalar_ts in
       let gate =
         (* Divisions zs = xs / ys are modeled as multiplications xs = zs * ys.
            Thus, we swap inp1 (xs) and out (zs) when division = true. *)
         let left_row1 = if division then out else inp1 in
         let left_row2 = if division then inp1 else out in
         [|
           CS.new_constraint
             ~wires:(left_row1 @ inp2)
             ~q_mod_mul:[(label, one)]
             ("mod_arith-" ^ label_suffix ^ ".1");
           CS.new_constraint
             ~wires:(left_row2 @ [qm] @ ts)
             ("mod_arith-" ^ label_suffix ^ ".2");
         |]
       in
       let solver =
         Mod_Mul
           {
             modulus;
             base;
             nb_limbs;
             moduli;
             qm_bound;
             ts_bounds;
             inp1;
             inp2;
             out;
             qm;
             ts;
             inverse = division;
           }
       in
       (* The output is not assumed to be well-formed, we need to enforce this
          with constraints. In particular, we need to range-check every limb
          in the output in the range [0, base). *)
       iterM (Num.range_check ~nb_bits:(Z.log2 base)) (of_list zs)
       (* qm needs to be range-checked in the interval [0, qm_ubound) *)
       >* Num.range_check ~nb_bits:(Z.log2 qm_ubound) scalar_qm
          (* every tj needs to be range-checked in the interval [0, tj_ubound) *)
       >* iter2M
            (fun tj_ubound tj -> Num.range_check ~nb_bits:(Z.log2 tj_ubound) tj)
            ts_ubounds
            scalar_ts
       >* append gate ~solver >* ret zs

  (* In order to show that [x] is non-zero, we exhibit a value [r] such that
     [x * r = 1]. This is a characterization of non-zero elements when
     [modulus] is prime. More generally, when modulus is a prime power
     [modulus = p^k], [x] is non-zero iff there exists [r] such that
     [x * r = p^(k-1)]. For other moduli, this algorithm could be generalized
     when the prime factorization of [modulus] is known. *)
  let assert_non_zero ~label ~modulus ~is_prime ~nb_limbs ~base ~moduli
      ~qm_bound ~ts_bounds xs =
    (* For now we focus on the case when the modulus is prime, this allows us
       to characterize non-zero elements as elements which have an inverse. *)
    if not is_prime then
      raise
      @@ Failure
           (Format.sprintf
              "assert_non_zero: this function does not support arbitrary \
               moduli yet; for now, the modulus is required to be prime; %s is \
               composite."
              (Z.to_string modulus)) ;
    let* o = Num.constant S.one in
    let* z = Num.constant S.zero in
    let one =
      o :: List.init (nb_limbs - 1) (Fun.const z) |> List.rev |> to_list
    in
    let* _ =
      mul
        ~division:true
        ~label
        ~modulus
        ~nb_limbs
        ~base
        ~moduli
        ~qm_bound
        ~ts_bounds
        one
        xs
    in
    ret unit

  let is_zero ~label ~modulus ~is_prime ~nb_limbs ~base ~moduli ~qm_bound
      ~ts_bounds (List xs) =
    let mul ?(division = false) =
      mul ~division ~label ~modulus ~nb_limbs ~base ~moduli ~qm_bound ~ts_bounds
    in
    let assert_non_zero =
      assert_non_zero
        ~label
        ~modulus
        ~is_prime
        ~nb_limbs
        ~base
        ~moduli
        ~qm_bound
        ~ts_bounds
    in
    with_label ~label:"Mod_arith.is_zero"
    @@
    (* b is the output of [is_zero]: b = 1 if x = 0 and b = 0 otherwise *)
    let* b = fresh Dummy.bool in
    let* rs = fresh @@ Dummy.list nb_limbs Dummy.scalar in
    let (Bool out) = b in
    let inp = List.map unscalar xs in
    let aux = List.map unscalar (of_list rs) in
    let solver = Mod_IsZero {modulus; base; nb_limbs; inp; aux; out} in
    let* (Bool not_b) = add_solver ~solver >* Bool.bnot b in
    let* z = Num.constant S.zero in
    (* [zero_or_one] represents the modular integer [0] if [x] is zero
       (b = 1) or the modular integer [1] if [x] is non-zero (b = 0) *)
    let zero_or_one =
      Scalar not_b :: List.init (nb_limbs - 1) (Fun.const z)
      |> List.rev |> to_list
    in
    (* We enforce the constraint [x * r = zero_or_one] for some [r <> 0].
       Note that if [x] is zero, the only way to satisfy the above equation
       is to set [zero_or_one] to be [zero], that is, set [b = 1].
       On the other hand, if [x <> 0], because we will enforce the constraint
       [r <> 0] and we are over an integral domain, the only way to satisfy
       the above constraint is to set [zero_or_one] to be [one], i.e. [b = 0],
       as desired. *)
    let* x_times_r = mul (List xs) rs in
    assert_non_zero rs >* assert_equal x_times_r zero_or_one >* ret b
end

module Poseidon = struct
  module VS = Linear_algebra.Make_VectorSpace (S)
  module Poly = Polynomial.MakeUnivariate (S)

  module Poly_Module = Linear_algebra.Make_Module (struct
    include Poly

    let eq = Poly.equal

    let negate p = Poly.(zero - p)

    let mul = Poly.( * )
  end)

  let poseidon128_full_round ~matrix ~k (Scalar x0, Scalar x1, Scalar x2) =
    let*& y0 = fresh Dummy.scalar in
    let*& y1 = fresh Dummy.scalar in
    let*& y2 = fresh Dummy.scalar in
    let solver = Poseidon128Full {x0; y0; x1; y1; x2; y2; k; matrix} in
    let minv = VS.inverse matrix in
    let k_vec = VS.(mul minv (transpose [|k|])) in

    (* We enforce the following constraints:

       [x0    y0]  with selectors {qc, qx5, qo, qrg, qog}
       [x1 y1 y2]  with selectors {qc, qx5, qr, qo, qrg}
       [x2 y0 y1]  with selectors {qc, qx5, qr, qo, qrg}
       [   y2   ]  with no selectors

       where the selector constants are given by the inverse of the MDS
       matrix. In particular:

          y = M * x^5 + k    iff    M^{-1} * y - x^5 - M^{-1} * k = 0

       (This allows us to have 1 power of 5 (instead of all 3) per constraint,
       since vector x^5 is not multiplied by M in the second representation.) *)
    append
      [|
        CS.new_constraint
          ~wires:[x0; 0; y0]
          ~qx5a:mone
          ~qc:(S.negate k_vec.(0).(0))
          ~linear:[(wqo, minv.(0).(0))]
          ~linear_g:[(wqr, minv.(0).(1)); (wqo, minv.(0).(2))]
          "pos128_full.1";
        CS.new_constraint
          ~wires:[x1; y1; y2]
          ~qx5a:mone
          ~qc:(S.negate k_vec.(1).(0))
          ~linear:[(wqr, minv.(1).(1)); (wqo, minv.(1).(2))]
          ~linear_g:[(wqr, minv.(1).(0))]
          "pos128_full.2";
        CS.new_constraint
          ~wires:[x2; y0; y1]
          ~qx5a:mone
          ~qc:(S.negate k_vec.(2).(0))
          ~linear:[(wqr, minv.(2).(0)); (wqo, minv.(2).(1))]
          ~linear_g:[(wqr, minv.(2).(2))]
          "pos128_full.3";
        CS.new_constraint ~wires:[0; y2; 0] "pos128_full.4";
      |]
      ~solver
    >* ret @@ to_list [Scalar y0; Scalar y1; Scalar y2]

  let poseidon128_four_partial_rounds ~matrix ~ks
      (Scalar x0, Scalar x1, Scalar x2) =
    let*& a = fresh Dummy.scalar in
    let*& a_5 = fresh Dummy.scalar in
    let*& b = fresh Dummy.scalar in
    let*& b_5 = fresh Dummy.scalar in
    let*& c = fresh Dummy.scalar in
    let*& c_5 = fresh Dummy.scalar in
    let*& y0 = fresh Dummy.scalar in
    let*& y1 = fresh Dummy.scalar in
    let*& y2 = fresh Dummy.scalar in
    let k_cols = Array.init 4 (fun i -> VS.filter_cols (Int.equal i) ks) in
    let solver =
      Poseidon128Partial
        {a; b; c; a_5; b_5; c_5; x0; y0; x1; y1; x2; y2; k_cols; matrix}
    in

    (* We represent variables x0, x1, x2_5, a, a_5, b, b_5, c, c_5, y0, y1, y2
       with monomials x, x^2, x^3, ..., x^12 respectively. *)
    let module SMap = Map.Make (String) in
    let vars =
      ["x0"; "x1"; "x2_5"; "a"; "a_5"; "b"; "b_5"; "c"; "c_5"; "y0"; "y1"; "y2"]
    in
    let varsMap =
      SMap.of_seq @@ List.(to_seq @@ mapi (fun i s -> (s, i + 1)) vars)
    in

    let var s = SMap.find s varsMap in
    let pvar s = Poly.of_coefficients [(S.one, var s)] in
    let state = [|[|pvar "x0"|]; [|pvar "x1"|]; [|pvar "x2_5"|]|] in

    let to_poly = Array.(map (map (fun c -> Poly.of_coefficients [(c, 0)]))) in
    let matrix = to_poly matrix in

    (* Apply partial round 0 *)
    let state = Poly_Module.(add (mul matrix state) @@ to_poly k_cols.(0)) in
    let eq1 = Poly.(state.(2).(0) - pvar "a") in
    state.(2) <- [|pvar "a_5"|] ;

    (* Apply partial round 1 *)
    let state = Poly_Module.(add (mul matrix state) @@ to_poly k_cols.(1)) in
    let eq2 = Poly.(state.(2).(0) - pvar "b") in
    state.(2) <- [|pvar "b_5"|] ;

    (* Apply partial round 2 *)
    let state = Poly_Module.(add (mul matrix state) @@ to_poly k_cols.(2)) in
    let eq3 = Poly.(state.(2).(0) - pvar "c") in
    state.(2) <- [|pvar "c_5"|] ;

    (* Apply partial round 3 *)
    let state = Poly_Module.(add (mul matrix state) @@ to_poly k_cols.(3)) in
    let eq4 = Poly.(state.(0).(0) - pvar "y0") in
    let eq5 = Poly.(state.(1).(0) - pvar "y1") in
    let eq6 = Poly.(state.(2).(0) - pvar "y2") in

    let eqs =
      let row_of_eq eq =
        (* This function gives coefficients in decending order of degree *)
        let coeffs = Poly.get_dense_polynomial_coefficients eq in
        List.(rev coeffs @ init (13 - List.length coeffs) (fun _ -> S.zero))
        |> Array.of_list
      in
      Array.map row_of_eq [|eq1; eq2; eq3; eq4; eq5; eq6|]
    in

    let cancel i j x =
      let x = var x in
      VS.row_add ~coeff:S.(negate @@ (eqs.(i).(x) / eqs.(j).(x))) i j eqs
    in

    (* Cancel x2_5 *)
    cancel 1 0 "x2_5" ;
    cancel 2 0 "x2_5" ;
    cancel 3 0 "x2_5" ;
    cancel 4 0 "x2_5" ;
    cancel 5 0 "x2_5" ;

    (* Cancel a_5 *)
    cancel 2 1 "a_5" ;
    cancel 3 1 "a_5" ;
    cancel 4 1 "a_5" ;
    cancel 5 1 "a_5" ;

    (* Cancel b_5 *)
    cancel 3 2 "b_5" ;
    cancel 4 2 "b_5" ;
    cancel 5 2 "b_5" ;

    (* Cancel c_5 *)
    cancel 4 3 "c_5" ;

    (* Cancel x0 in equation 5 (b_5 comes back) *)
    cancel 4 2 "x0" ;

    VS.row_swap 2 4 eqs ;

    (* We enforce the following constraints:

       [x2      ]  with selectors {qc, qx5, qlg, qrg, qog}
       [a  x0 x1]  with selectors {qc, qx5, ql, qr, qo, qlg}
       [b  y1 x1]  with selectors {qc, qx5, ql, qr, qo, qlg, qrg, qog}
       [c  y0  a]  with selectors {qc, qx5, ql, qr, qo, qlg, qrg, qog}
       [b  x0 x1]  with selectors {qc, qx5, ql, qr, qo, qlg, qrg}
       [c  a   b]  with selectors {qc, qx5, ql, qr, qo, qlg, qrg, qog}
       [y2 x0 x1]  with no selectors. *)
    append
      [|
        CS.new_constraint
          ~wires:[x2; 0; 0]
          ~qc:eqs.(0).(0)
          ~qx5a:eqs.(0).(var "x2_5")
          ~linear_g:
            [
              (wql, eqs.(0).(var "a"));
              (wqr, eqs.(0).(var "x0"));
              (wqo, eqs.(0).(var "x1"));
            ]
          "pos128_4partial.1";
        CS.new_constraint
          ~wires:[a; x0; x1]
          ~qc:eqs.(1).(0)
          ~qx5a:eqs.(1).(var "a_5")
          ~linear:
            [
              (wql, eqs.(1).(var "a"));
              (wqr, eqs.(1).(var "x0"));
              (wqo, eqs.(1).(var "x1"));
            ]
          ~linear_g:[(wql, eqs.(1).(var "b"))]
          "pos128_4partial.2";
        CS.new_constraint
          ~wires:[b; y1; x1]
          ~qc:eqs.(2).(0)
          ~qx5a:eqs.(2).(var "b_5")
          ~linear:
            [
              (wql, eqs.(2).(var "b"));
              (wqr, eqs.(2).(var "y1"));
              (wqo, eqs.(2).(var "x1"));
            ]
          ~linear_g:
            [
              (wql, eqs.(2).(var "c"));
              (wqr, eqs.(2).(var "y0"));
              (wqo, eqs.(2).(var "a"));
            ]
          "pos128_4partial.3";
        CS.new_constraint
          ~wires:[c; y0; a]
          ~qc:eqs.(3).(0)
          ~qx5a:eqs.(3).(var "c_5")
          ~linear:
            [
              (wql, eqs.(3).(var "c"));
              (wqr, eqs.(3).(var "y0"));
              (wqo, eqs.(3).(var "a"));
            ]
          ~linear_g:
            [
              (wql, eqs.(3).(var "b"));
              (wqr, eqs.(3).(var "x0"));
              (wqo, eqs.(3).(var "x1"));
            ]
          "pos128_4partial.4";
        CS.new_constraint
          ~wires:[b; x0; x1]
          ~qc:eqs.(4).(0)
          ~qx5a:eqs.(4).(var "b_5")
          ~linear:
            [
              (wql, eqs.(4).(var "b"));
              (wqr, eqs.(4).(var "x0"));
              (wqo, eqs.(4).(var "x1"));
            ]
          ~linear_g:[(wql, eqs.(4).(var "c")); (wqr, eqs.(4).(var "a"))]
          "pos128_4partial.5";
        CS.new_constraint
          ~wires:[c; a; b]
          ~qc:eqs.(5).(0)
          ~qx5a:eqs.(5).(var "c_5")
          ~linear:
            [
              (wql, eqs.(5).(var "c"));
              (wqr, eqs.(5).(var "a"));
              (wqo, eqs.(5).(var "b"));
            ]
          ~linear_g:
            [
              (wql, eqs.(5).(var "y2"));
              (wqr, eqs.(5).(var "x0"));
              (wqo, eqs.(5).(var "x1"));
            ]
          "pos128_4partial.6";
        CS.new_constraint ~wires:[y2; x0; x1] "pos128_4partial.7";
      |]
      ~solver
    >* ret @@ to_list [Scalar y0; Scalar y1; Scalar y2]
end

module Anemoi = struct
  module AnemoiPerm = Bls12_381_hash.Permutation.Anemoi

  let beta = AnemoiPerm.Parameters.beta

  let gamma = AnemoiPerm.Parameters.gamma

  let g = AnemoiPerm.Parameters.g

  let delta = AnemoiPerm.Parameters.delta

  (* Hash function described in https://eprint.iacr.org/2022/840.pdf *)

  let anemoi_round ~kx ~ky (Scalar x0, Scalar y0) =
    let*& w = fresh Dummy.scalar in
    let*& v = fresh Dummy.scalar in
    let*& x1 = fresh Dummy.scalar in
    let*& y1 = fresh Dummy.scalar in
    let solver = AnemoiRound {x0; y0; w; v; x1; y1; kx; ky} in

    (*
       The equations of a Anemoi round are as follows,
           a. x1 - u - g*v - (kx + g*ky) = 0
              Corresponds to Linear layer equation (after adding the round constant to x_i ), eq.4 page 24
           b. y1 - g u - (g^2+1)*v - (g*kx + (g^2+1)*ky) = 0
              Corresponds to Linear layer equation (after adding the round constant to y_i ), eq.5 page 24
           c. w^5 + beta y0^2 + gamma - x0 = 0
              Corresponds to the first equation of S-box, eq.2 page 24
           d. y0 - v - w = 0
               Corresponds to the second equation of S-box layer, eq.1 page 24
           e. w^5 + beta v^2 + delta - u = 0
              Corresponds to the third equation of S-box, eq.3 page 24

       We inlined $u$ from a. in e. and re-ordered the equations so that the variables fit in the wires.
       The result is as follows,
           1. w^5 + beta y0^2 + gamma - x0 = 0                           <- c
           2. y0 - v - w = 0                                             <- d
           3. w^5 + beta v^2 + g*v - x1 + (delta + kx + g*ky) = 0        <- e - a
           4. y1 - g x1 - v - ky = 0                                     <- b - g * a
      *)
    append
      [|
        CS.new_constraint
          ~wires:[y0; y0; w]
          ~qc:gamma
          ~qm:beta
          ~qx5c:one
          ~linear_g:[(wql, mone)]
          "anemoi.1";
        CS.new_constraint
          ~wires:[x0; y0; w]
          ~linear:[(wqr, one); (wqo, mone)]
          ~linear_g:[(wql, mone)]
          "anemoi.2";
        CS.new_constraint
          ~wires:[v; v; w]
          ~qc:S.(kx + delta + (g * ky))
          ~qm:beta
          ~qx5c:one
          ~linear:[(wql, g)]
          ~linear_g:[(wql, mone)]
          "anemoi.3";
        CS.new_constraint
          ~wires:[x1; y1; v]
          ~qc:S.(negate ky)
          ~linear:[(wql, S.(negate g)); (wqr, one); (wqo, mone)]
          "anemoi.4";
      |]
      ~solver
    >* ret @@ pair (Scalar x1) (Scalar y1)

  let anemoi_double_round ~kx1 ~ky1 ~kx2 ~ky2 (Scalar x0, Scalar y0) =
    let*& w0 = fresh Dummy.scalar in
    let*& w1 = fresh Dummy.scalar in
    let*& y1 = fresh Dummy.scalar in
    let*& x2 = fresh Dummy.scalar in
    let*& y2 = fresh Dummy.scalar in
    let solver =
      AnemoiDoubleRound {x0; y0; w0; w1; y1; x2; y2; kx1; kx2; ky1; ky2}
    in

    let two = S.(add one one) in
    let g2 = S.(g * g) in
    let g2_p_1 = S.(g2 + one) in
    let g_beta = S.(g * beta) in

    (*
                                  Equations                                  |     Wires              |     Selectors
      -------------------------------------------------------------------------------------------------------------
      a <- 2. -(beta*g)*w0^2 + (2*beta*g)*w0*y0 + (g^2+1)*w0 - g*x0 - (g^2+1)*y0 + y1 + (g*gamma - delta*g - g*kx1 - (g^2+1)*ky1)
      | a: y0, b: w0, c: x0
      | qr2=-(beta*g)   qm:(2*beta*g)   qr:(g^2+1)   qo:-g   ql:-(g^2+1)   qlg:1  qc =(g*gamma - delta*g - g*kx1 - (g^2+1)*ky1)

      b <- 4. -(g * beta)*w1^2 + (2 * g * beta)*w1*y1 - w0 + g^2*w1 + g*x2 + y0 - (g^2+1)*y1 + (g*gamma + ky1 - delta*g  - g*kx2 - g^2*ky2)
      | a: y1, b: w1, c: w0
      | qr2=-(g*beta)   qm:(2*g*beta)  qo:-1   qr:g^2   qrg:g   qlg:1   ql:-(g^2+1)   qc:(g*gamma + ky1 - delta*g  -g*kx2 - g^2*ky2)

      c <- 5. w1 - g*x2 - y1 + y2 - ky2
      | a: y0, b: x2, c: y2
      | qlg:1   qr:-g   qrg:-1   qo:1  qc:-ky2

      d <- 3. g*w1^5 + (g * beta)*y1^2 - w0 + y0 - y1 + (ky1 + g*gamma)
      | a: w1, b: y1, c: __
      | ql5=g    qr2=(g*beta)    qlg:-1   qrg:1    qr:-1    qc:(ky1 + g*gamma)

      e <- 1. w0^5 + beta*y0^2 - x0 + gamma
      | a: w0, b: y0, c: x0
      | ql5=1   qr2=beta   qo:-1   qc:gamma
      *)
    let qca = S.(sub (gamma * g) ((g * (kx1 + delta)) + (g2_p_1 * ky1))) in
    let qcb = S.(sub ((gamma * g) + ky1) ((g * (kx2 + delta)) + (g2 * ky2))) in
    let qcd = S.(ky1 + (g * gamma)) in
    append
      [|
        CS.new_constraint
          ~wires:[y0; w0; x0]
          ~qx2b:S.(negate g_beta)
          ~qm:S.(two * g_beta)
          ~linear:[(wql, S.(negate g2_p_1)); (wqr, g2_p_1); (wqo, S.(negate g))]
          ~linear_g:[(wql, S.one)]
          ~qc:qca
          "anemoi_double.a";
        CS.new_constraint
          ~wires:[y1; w1; w0]
          ~qx2b:S.(negate g_beta)
          ~qm:S.(two * g_beta)
          ~linear:[(wql, S.(negate g2_p_1)); (wqr, g2); (wqo, mone)]
          ~linear_g:[(wql, one); (wqr, g)]
          ~qc:qcb
          "anemoi_double.b";
        CS.new_constraint
          ~wires:[y0; x2; y2]
          ~linear:[(wqr, S.(negate g)); (wqo, one)]
          ~linear_g:[(wql, one); (wqr, mone)]
          ~qc:S.(negate ky2)
          "anemoi_double.c";
        CS.new_constraint
          ~wires:[w1; y1; y1]
          ~qx5a:g
          ~qx2b:g_beta
          ~linear:[(wqr, mone)]
          ~linear_g:[(wql, mone); (wqr, one)]
          ~qc:qcd
          "anemoi_double.d";
        CS.new_constraint
          ~wires:[w0; y0; x0]
          ~qx5a:one
          ~qx2b:beta
          ~linear:[(wqo, mone)]
          ~qc:gamma
          "anemoi_double.e";
      |]
      ~solver
    >* ret @@ pair (Scalar x2) (Scalar y2)

  let anemoi_custom ~kx1 ~ky1 ~kx2 ~ky2 (Scalar x0, Scalar y0) =
    with_label ~label:"custom_anemoi"
    @@ let*& x1 = fresh Dummy.scalar in
       let*& y1 = fresh Dummy.scalar in
       let*& x2 = fresh Dummy.scalar in
       let*& y2 = fresh Dummy.scalar in
       let precomputed_advice =
         [("qadv0", kx1); ("qadv1", ky1); ("qadv2", kx2); ("qadv3", ky2)]
       in
       let gate =
         [|
           CS.new_constraint
             ~wires:[0; x1; y1; x0; y0]
             ~q_anemoi:one
             ~precomputed_advice
             "custom_anemoi.1";
           CS.new_constraint ~wires:[0; 0; 0; x2; y2] "custom_anemoi.2";
         |]
       in
       let solver = AnemoiCustom {x0; y0; x1; y1; x2; y2; kx1; kx2; ky1; ky2} in
       append gate ~solver >* ret @@ pair (Scalar x2) (Scalar y2)
end

(* Forces the delayed checks, and computes the conjunction of
   check wires. *)
let get_checks_wire s =
  let s, Unit = s.delayed s in
  let s, w = Bool.band_list s.check_wires s in
  ({s with check_wires = []; delayed = ret Unit}, w)

(* Run the monad *)
let get f =
  let s, res =
    f
      {
        nvars = 0;
        cs = [];
        inputs = Array.init 0 (fun _ -> S.zero);
        input_com_sizes = [];
        pi_size = 0;
        input_flag = `InputCom;
        tables = [];
        solver = Solver.empty_solver;
        delayed = ret Unit;
        check_wires = [];
        range_checks = Range_checks.empty;
        range_checks_labels = [];
        labels = [];
        cache = Scalar_map.empty;
      }
  in
  let s, Unit = s.delayed s in
  let s, res =
    match s.check_wires with
    | [] -> (s, res)
    | ws ->
        let s, w = Bool.band_list ws s in
        let s, Unit = Bool.assert_true w s in
        (s, res)
  in
  let solver =
    {s.solver with final_size = s.nvars; initial_size = Array.length s.inputs}
  in
  let s = {s with solver} in
  (s, res)

let get_inputs f =
  let s, _ = get f in
  (s.inputs, s.pi_size)

type cs_result = {
  nvars : int;
  free_wires : int list;
  cs : Csir.CS.t;
  public_input_size : int;
  input_com_sizes : int list;
  tables : Csir.Table.t list;
  (* wire index * (plonk index * bound) *)
  range_checks : (int * (int * int) list) list;
  range_checks_labels : (string list * int) list;
  solver : Solver.t;
}
[@@deriving repr]

let cs_ti_t = Repr.pair Csir.CS.t Optimizer.trace_info_t

(* Converting range-checks (given as a list of (plompiler index * bound)) to
    something that PlonK can understand : (wire name * (wire index * bound)).
    We go through each wire & each constraint, we take the plompiler index of
    this wire at this constraint, and look for it in the plompiler
    range-checks ; if this index is range-checked, it will be formatted as
   (wire name * (plonk gate index * bound))
*)
let to_plonk_range_checks plomp_range_checks cs =
  let range_checks =
    let cs = Array.concat cs in
    (* This function puts all the range-checks that can be written with the
       given [wire] in their PlonK form in [all_found_rc], and removes them
       from [pending_rc] *)
    let rec format_rc (pending_rc, all_found_rc) wire =
      (* We went through all wires *)
      if wire = Csir.nb_wires_arch then
        (* We assert that all pending range-checks have been processed *)
        if not (Range_checks.is_empty pending_rc) then
          (* Probably not all inputs are constrained, i.e., they are unused *)
          failwith
            "to_plonk_range_checks : not all range-checks can be converted \
             into Plonk representation !"
        else all_found_rc
      else
        let pending_rc, found_rc =
          (* Go through all constraints of the CS *)
          Array.fold_left
            (fun (pending_rc, found_rc) (i, (constr : CS.raw_constraint)) ->
              (* idx is the plompiler index corresponding to [wire] in the [i]-th
                 constraint [constr] *)
              let idx = constr.wires.(wire) in
              match Range_checks.find_opt idx pending_rc with
              | None ->
                  (* [idx] is not range-checked, everything is returned unchanged *)
                  (pending_rc, found_rc)
              | Some bound ->
                  (* [idx] is range-checked, it’s removed from [pending_rc]
                     & ([wire] * (index of the gate [idx] * [bound]) is added to
                     [found_rc] *)
                  let pending_rc = Range_checks.remove idx pending_rc in
                  let found_rc = (i, bound) :: found_rc in
                  (pending_rc, found_rc))
            (pending_rc, [])
            (Array.mapi (fun i c -> (i, c)) cs)
        in
        format_rc (pending_rc, (wire, found_rc) :: all_found_rc) (wire + 1)
    in
    format_rc (plomp_range_checks, []) 0
  in
  (* Remove empty lists from range checks, in order to avoid useless iterations
     in PlonK & make the number of wires range-checked more easily computable *)
  List.filter (fun (_, r) -> r <> []) range_checks

let get_cs ?(optimize = false) f : cs_result =
  let s, _ = get f in
  let ts = List.map (fun t_id -> Tables.find t_id tables) s.tables in
  let cs, solver, free_wires =
    if optimize then
      let path = Utils.circuit_path (Utils.get_circuit_id s.cs) in
      let cs, ti =
        if Sys.file_exists path then (
          (* If defined, load it up *)
          let inc = open_in path in
          let size = in_channel_length inc in
          let buffer = Bytes.create size in
          really_input inc buffer 0 (in_channel_length inc) ;
          close_in inc ;
          Utils.of_bytes cs_ti_t buffer)
        else
          let nb_inputs = Array.length s.inputs in
          let o =
            Optimizer.optimize ~nb_inputs ~range_checks:s.range_checks s.cs
          in
          let serialized = Utils.to_bytes cs_ti_t o in
          let outc = open_out_bin path in
          output_bytes outc serialized ;
          close_out outc ;
          o
      in
      (cs, Solver.append_solver (Updater ti) s.solver, ti.free_wires)
    else (s.cs, s.solver, [])
  in
  let range_checks = to_plonk_range_checks s.range_checks cs in
  let range_checks_labels =
    List.map (fun (label, nb) -> (List.rev label, nb)) s.range_checks_labels
  in
  {
    nvars = s.nvars;
    free_wires;
    cs;
    tables = ts;
    public_input_size = s.pi_size;
    input_com_sizes = List.rev s.input_com_sizes;
    solver;
    range_checks;
    range_checks_labels;
  }
