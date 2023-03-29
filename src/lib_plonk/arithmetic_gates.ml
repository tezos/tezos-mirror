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

open Bls
open Identities
module L = Plompiler.LibCircuit
open Gates_common

module type Params = sig
  val wire : string

  val selector : string

  val is_next : bool

  val cs :
    q:L.scalar L.repr ->
    a:L.scalar L.repr ->
    b:L.scalar L.repr ->
    c:L.scalar L.repr ->
    d:L.scalar L.repr ->
    e:L.scalar L.repr ->
    ag:L.scalar L.repr ->
    bg:L.scalar L.repr ->
    cg:L.scalar L.repr ->
    dg:L.scalar L.repr ->
    eg:L.scalar L.repr ->
    ?precomputed_advice:L.scalar L.repr SMap.t ->
    unit ->
    L.scalar L.repr list L.t
end

(* General functor to create Artih monomial gate that add wire *)
module AddWire (Params : Params) : Base_sig = struct
  let q_label = Params.selector

  let identity = (arith, 1)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 1

  let gx_composition = Params.is_next

  let equations ~q ~wires ~wires_g ?precomputed_advice:_ () =
    let i =
      match Params.wire with
      | s when s = left -> 0
      | s when s = right -> 1
      | s when s = output -> 2
      | s when s = top -> 3
      | s when s = bottom -> 4
      | _ -> assert false
    in
    let ws = if Params.is_next then wires_g else wires in
    Scalar.[q * ws.(i)]

  let blinds =
    let array = if Params.is_next then [|0; 1|] else [|1; 0|] in
    SMap.singleton Params.wire array

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain :
      prover_identities =
   fun evaluations ->
    let tmps, _ = get_buffers ~nb_buffers ~nb_ids:0 in
    let poly_names = [prefix_common q_label; prefix Params.wire] in
    let composition_gx =
      if Params.is_next then ([0; 1], Domain.length domain) else ([0; 0], 1)
    in

    let res =
      Evaluations.mul ~res:tmps.(0) ~evaluations ~poly_names ~composition_gx ()
    in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let q = get_answer answers X @@ prefix_common q_label in
    let w =
      let p = if Params.is_next then GX else X in
      get_answer answers p @@ prefix Params.wire
    in
    let res = Scalar.mul q w in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let polynomials_degree = SMap.of_list [(Params.wire, 2); (q_label, 2)]

  let cs = Params.cs
end

(* Add next output gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·c
*)
module AddOutput = AddWire (struct
  let wire = output

  let selector = "qo"

  let is_next = false

  let cs ~q:qo ~a:_ ~b:_ ~c ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qo c)
end)

(* Add next left gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·a
*)
module AddLeft = AddWire (struct
  let wire = left

  let selector = "ql"

  let is_next = false

  let cs ~q:ql ~a ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul ql a)
end)

(* Add next right gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·b
*)
module AddRight = AddWire (struct
  let wire = right

  let selector = "qr"

  let is_next = false

  let cs ~q:qr ~a:_ ~b ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qr b)
end)

(* Add top gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·d
*)
module AddTop = AddWire (struct
  let wire = top

  let selector = "qd"

  let is_next = false

  let cs ~q:qd ~a:_ ~b:_ ~c:_ ~d ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qd d)
end)

(* Add bottom gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·e
*)
module AddBottom = AddWire (struct
  let wire = bottom

  let selector = "qe"

  let is_next = false

  let cs ~q:qe ~a:_ ~b:_ ~c:_ ~d:_ ~e ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qe e)
end)

(* Add next output gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·cg
*)
module AddNextOutput = AddWire (struct
  let wire = output

  let selector = "qog"

  let is_next = true

  let cs ~q:qog ~a:_ ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qog cg)
end)

(* Add next left gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·ag
*)
module AddNextLeft = AddWire (struct
  let wire = left

  let selector = "qlg"

  let is_next = true

  let cs ~q:qlg ~a:_ ~b:_ ~c:_ ~d:_ ~e:_ ~ag ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qlg ag)
end)

(* Add next right gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·bg
*)
module AddNextRight = AddWire (struct
  let wire = right

  let selector = "qrg"

  let is_next = true

  let cs ~q:qrg ~a:_ ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qrg bg)
end)

(* Add next top gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·dg
*)
module AddNextTop = AddWire (struct
  let wire = top

  let selector = "qdg"

  let is_next = true

  let cs ~q:qdg ~a:_ ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg ~eg:_
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qdg dg)
end)

(* Add next bottom gate
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·eg
*)
module AddNextBottom = AddWire (struct
  let wire = bottom

  let selector = "qeg"

  let is_next = true

  let cs ~q:qeg ~a:_ ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg
      ?precomputed_advice:_ () =
    map_singleton (L.Num.mul qeg eg)
end)

(* Add constant
   Arith monomial
   degree : n
   advice selectors : None
   equations : + q
*)
module Constant : Base_sig = struct
  let q_label = "qc"

  let identity = (arith, 1)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 1

  let gx_composition = false

  let equations ~q ~wires:_ ~wires_g:_ ?precomputed_advice:_ () = [q]

  let blinds = SMap.empty

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain:_ :
      prover_identities =
   fun evaluations ->
    let tmps, _ = get_buffers ~nb_buffers ~nb_ids:0 in

    (* This is copied because in sum_prover_queries it could
       be overwritten by the inplace addition. *)
    let res =
      Evaluations.copy
        ~res:tmps.(0)
        (SMap.find (prefix_common q_label) evaluations)
    in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let res = get_answer answers X @@ prefix_common q_label in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let polynomials_degree = SMap.empty

  let cs ~q:qc ~a:_ ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    L.ret [qc]
end

(* Add multiplication
   Arith monomial
   degree : 3n
   advice selectors : None
   equations : + q·a·b
*)
module Multiplication : Base_sig = struct
  let q_label = "qm"

  let identity = (arith, 1)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 1

  let gx_composition = false

  let equations ~q ~wires ~wires_g:_ ?precomputed_advice:_ () =
    let a = wires.(0) in
    let b = wires.(1) in
    Scalar.[q * a * b]

  let blinds = SMap.of_list [(right, [|1; 0|]); (left, [|1; 0|])]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain:_ :
      prover_identities =
   fun evaluations ->
    let tmps, _ = get_buffers ~nb_buffers ~nb_ids:0 in
    let ({q; a; b; _} : witness) =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in

    let res = Evaluations.mul_c ~res:tmps.(0) ~evaluations:[q; a; b] () in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let ({q; a; b; _} : answers) =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let res = Scalar.(q * a * b) in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let polynomials_degree = SMap.of_list [(left, 3); (right, 3); (q_label, 3)]

  let cs ~q:qm ~a ~b ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    let open L in
    map_singleton
      (let* tmp = Num.mul qm a in
       Num.mul tmp b)
end

(* Add right²
   Arith monomial
   degree : 6n
   advice selectors : None
   equations : + q·b²
*)
module X2B : Base_sig = struct
  let q_label = "qx2b"

  let identity = (arith, 1)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 1

  let gx_composition = false

  let equations ~q ~wires ~wires_g:_ ?precomputed_advice:_ () =
    let b = wires.(1) in
    Scalar.[q * square b]

  let blinds = SMap.singleton right [|1; 0|]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain:_ :
      prover_identities =
   fun evaluations ->
    let tmps, _ = get_buffers ~nb_buffers ~nb_ids:0 in
    let ({q; b; _} : witness) =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in

    let res =
      Evaluations.mul_c ~res:tmps.(0) ~evaluations:[q; b] ~powers:[1; 2] ()
    in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let ({q; b; _} : answers) =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let res = Scalar.(q * square b) in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let polynomials_degree = SMap.of_list [(right, 3); (q_label, 3)]

  let cs ~q:qx2b ~a:_ ~b ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    let open L in
    map_singleton
      (let* b2 = Num.square b in
       Num.mul qx2b b2)
end

(* Add left⁵
   Arith monomial
   degree : 6n
   advice selectors : None
   equations : + q·a⁵
*)
module X5A : Base_sig = struct
  let q_label = "qx5a"

  let identity = (arith, 1)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 1

  let gx_composition = false

  let equations ~q ~wires ~wires_g:_ ?precomputed_advice:_ () =
    let a = wires.(0) in
    Scalar.[q * pow a (Z.of_int 5)]

  let blinds = SMap.singleton left [|1; 0|]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain:_ :
      prover_identities =
   fun evaluations ->
    let tmps, _ = get_buffers ~nb_buffers ~nb_ids:0 in
    let ({q; a; _} : witness) =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in

    let res =
      Evaluations.mul_c ~res:tmps.(0) ~evaluations:[q; a] ~powers:[1; 5] ()
    in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let ({q; a; _} : answers) =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let a2 = Scalar.mul a a in
    let a4 = Scalar.mul a2 a2 in
    let a5 = Scalar.mul a4 a in
    let res = Scalar.mul q a5 in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let polynomials_degree = SMap.of_list [(left, 6); (q_label, 6)]

  let cs ~q:qx5 ~a ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    let open L in
    map_singleton
      (let* a5 = Num.pow5 a in
       Num.mul qx5 a5)
end

(* Add output⁵
   Arith monomial
   degree : 6n
   advice selectors : None
   equations : + q·c⁵
*)
module X5C : Base_sig = struct
  let q_label = "qx5c"

  let identity = (arith, 1)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 1

  let gx_composition = false

  let equations ~q ~wires ~wires_g:_ ?precomputed_advice:_ () =
    let c = wires.(2) in
    Scalar.[q * pow c (Z.of_int 5)]

  let blinds = SMap.singleton output [|1; 0|]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain:_ :
      prover_identities =
   fun evaluations ->
    let tmps, _ = get_buffers ~nb_buffers ~nb_ids:0 in
    let ({q; c; _} : witness) =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in

    let res =
      Evaluations.mul_c ~res:tmps.(0) ~evaluations:[q; c] ~powers:[1; 5] ()
    in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let ({q; c; _} : answers) =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let c2 = Scalar.mul c c in
    let c4 = Scalar.mul c2 c2 in
    let c5 = Scalar.mul c4 c in
    let res = Scalar.mul q c5 in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let polynomials_degree = SMap.of_list [(output, 6); (q_label, 6)]

  let cs ~q:qx5c ~a:_ ~b:_ ~c ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    let open L in
    map_singleton
      (let* c5 = Num.pow5 c in
       Num.mul qx5c c5)
end

(* Add public input polynomial
   Arith monomial
   degree : n
   advice selectors : None
   equations : + q·a·b
*)
module Public : Base_sig = struct
  let q_label = "qpub"

  let blinds = SMap.empty

  let identity = (arith, 1)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 0

  let gx_composition = false

  let equations ~q:_ ~wires:_ ~wires_g:_ ?precomputed_advice:_ () =
    Scalar.[zero]

  let compute_PI ~start public_inputs domain evaluations =
    let size_domain = Domain.length domain in
    if size_domain = 0 then Evaluations.zero
    else
      let l = Array.length public_inputs in
      let scalars =
        Array.(
          concat
            [
              init start (fun _ -> Scalar.zero);
              public_inputs;
              init (size_domain - l - start) (fun _ -> Scalar.zero);
            ])
      in
      let pi =
        Poly.(opposite (Evaluations.interpolation_fft2 domain scalars))
      in
      let domain = Evaluations.get_domain evaluations in
      Evaluations.evaluation_fft domain pi

  let prover_identities ~prefix_common:_ ~prefix ~public ~domain :
      prover_identities =
   fun evaluations ->
    let res =
      compute_PI
        ~start:public.input_coms_size
        public.public_inputs
        domain
        evaluations
    in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let verifier_identities ~prefix_common:_ ~prefix ~public ~generator
      ~size_domain : verifier_identities =
   fun x _ ->
    let res =
      if size_domain = 0 then Scalar.zero
      else
        let g = Scalar.inverse_exn generator in
        let f (acc, gix) wi =
          let den = Scalar.(sub gix one) in
          Scalar.(acc + (wi / den), g * gix)
        in
        let res, _ =
          let shift = public.input_coms_size in
          let gx_init = Scalar.(pow generator Z.(neg (of_int shift)) * x) in
          Array.fold_left f Scalar.(zero, gx_init) public.public_inputs
        in
        let n = size_domain in
        let xn = Scalar.pow x (Z.of_int n) in
        let xn_min_one_div_n = Scalar.(sub xn one / of_int n) in
        Scalar.(negate (xn_min_one_div_n * res))
    in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let polynomials_degree = SMap.empty

  (* this function will not be used *)
  let cs ~q:_ ~a:_ ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    let open L in
    ret []
end

(* Add idx-th input com polynomial
   Arith monomial
   degree : 2n
   advice selectors : None
   equations : + q·com_idx
*)
module InputCom (Com : sig
  val idx : int
end) : Base_sig = struct
  let q_label = "qcom" ^ string_of_int Com.idx

  let com_label = com_label ^ string_of_int Com.idx

  let blinds = SMap.empty

  let identity = (arith, 1)

  let index_com = Some Com.idx

  let nb_advs = 0

  let nb_buffers = 0

  let gx_composition = false

  let equations ~q:_ ~wires:_ ~wires_g:_ ?precomputed_advice:_ () =
    Scalar.[zero]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain:_ :
      prover_identities =
   fun evaluations ->
    let _tmps, ids = get_buffers ~nb_buffers ~nb_ids:(snd identity) in
    let {q; _} =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in
    let com = Evaluations.find_evaluation evaluations (prefix com_label) in

    let res = Evaluations.mul_c ~res:ids.(0) ~evaluations:[q; com] () in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let ({q; _} : answers) =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let com = get_answer answers X @@ prefix com_label in
    let res = Scalar.(q * com) in
    SMap.singleton (prefix @@ arith ^ ".0") res

  let polynomials_degree = SMap.of_list [(com_label, 2); (q_label, 2)]

  (* TODO: implement *)
  let cs ~q:_ ~a:_ ~b:_ ~c:_ ~d:_ ~e:_ ~ag:_ ~bg:_ ~cg:_ ~dg:_ ~eg:_ =
    failwith "input commitments in aPlonK proofs are not supported yet"
end
