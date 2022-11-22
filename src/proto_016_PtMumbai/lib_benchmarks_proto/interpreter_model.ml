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

let ns = Namespace.make Registration_helpers.ns "interpreter"

let fv s = Free_variable.of_namespace (ns s)

(* ------------------------------------------------------------------------- *)

let trace_error expected given =
  let open Interpreter_workload in
  let exp = string_of_instr_or_cont expected in
  let given = string_of_instr_or_cont given in
  let msg =
    Format.asprintf
      "Interpreter_model: trace error, expected %s, given %s"
      exp
      given
  in
  Stdlib.failwith msg

let arity_error instr expected given =
  let open Interpreter_workload in
  let s = string_of_instr_or_cont instr in
  let msg =
    Format.asprintf
      "Interpreter_model: arity error (%s), expected %d, given %a"
      s
      expected
      Interpreter_workload.pp_args
      given
  in
  Stdlib.failwith msg

(* ------------------------------------------------------------------------- *)

let model_0 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | {name; args = []} -> if name = instr then () else trace_error instr name
      | {args; _} -> arity_error instr 0 args)
    ~model

let model_1 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | {name; args = [{name = _; arg}]} ->
          if name = instr then (arg, ()) else trace_error instr name
      | {args; _} -> arity_error instr 1 args)
    ~model

let model_2 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | {name; args = [{name = _; arg = x}; {name = _; arg = y}]} ->
          if name = instr then (x, (y, ())) else trace_error instr name
      | {args; _} -> arity_error instr 2 args)
    ~model

let model_3 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | {
          name;
          args = [{name = _; arg = x}; {name = _; arg = y}; {name = _; arg = z}];
        } ->
          if name = instr then (x, (y, (z, ()))) else trace_error instr name
      | {args; _} -> arity_error instr 3 args)
    ~model

let model_4 instr model =
  let open Interpreter_workload in
  Model.make
    ~conv:(function
      | {
          name;
          args =
            [
              {name = _; arg = w};
              {name = _; arg = x};
              {name = _; arg = y};
              {name = _; arg = z};
            ];
        } ->
          if name = instr then (w, (x, (y, (z, ()))))
          else trace_error instr name
      | {args; _} -> arity_error instr 4 args)
    ~model

let sf = Format.asprintf

let division_cost name =
  let const = fv (sf "%s_const" name) in
  let coeff = fv (sf "%s_coeff" name) in
  let module M = struct
    type arg_type = int * (int * unit)

    let name = ns name

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = Model.arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        (* Note that [q] is guaranteed to be non-negative because we use
           saturated subtraction. When [size1 < size2], the model evaluates to
           [const] as expected. *)
        let_ ~name:"q" (sat_sub size1 size2) @@ fun q ->
        (free ~name:coeff * q * size2) + free ~name:const
    end
  end in
  (module M : Model.Model_impl with type arg_type = int * (int * unit))

let addlogadd name =
  let const = fv (sf "%s_const" name) in
  let coeff = fv (sf "%s_coeff" name) in
  let module M = struct
    type arg_type = int * (int * unit)

    let name = ns name

    module Def (X : Costlang.S) = struct
      open X

      type model_type = size -> size -> size

      let arity = Model.arity_2

      let model =
        lam ~name:"size1" @@ fun size1 ->
        lam ~name:"size2" @@ fun size2 ->
        let_ ~name:"a" (size1 + size2) @@ fun a ->
        (free ~name:coeff * (a * log2 (int 1 + a))) + free ~name:const
    end
  end in
  (module M : Model.Model_impl with type arg_type = int * (int * unit))

(* Some instructions are oveloaded (eg COMPARE). In order to generate distinct
   models at different types, we must specialize these models. The [specialization]
   parameter acts as a mangling scheme to produce distinct models. *)
let name_of_instr_or_cont ?specialization instr_or_cont =
  let spec = Option.fold ~none:"" ~some:(fun s -> "_" ^ s) specialization in
  Interpreter_workload.string_of_instr_or_cont instr_or_cont ^ spec

module Models = struct
  let const1_model name =
    (* For constant-time instructions *)
    Model.unknown_const1 ~name:(ns name) ~const:(fv (sf "%s_const" name))

  let affine_model name =
    (* For instructions with cost function
       [\lambda size. const + coeff * size] *)
    Model.affine
      ~name:(ns name)
      ~intercept:(fv (sf "%s_const" name))
      ~coeff:(fv (sf "%s_coeff" name))

  let break_model name break =
    Model.breakdown
      ~name:(ns name)
      ~coeff1:(fv (sf "%s_coeff1" name))
      ~coeff2:(fv (sf "%s_coeff2" name))
      ~break

  let break_model_2 name break1 break2 =
    Model.breakdown2
      ~name:(ns name)
      ~coeff1:(fv (sf "%s_coeff1" name))
      ~coeff2:(fv (sf "%s_coeff2" name))
      ~coeff3:(fv (sf "%s_coeff3" name))
      ~break1
      ~break2

  let break_model_2_const name break1 break2 =
    Model.breakdown2_const
      ~name:(ns name)
      ~coeff1:(fv (sf "%s_coeff1" name))
      ~coeff2:(fv (sf "%s_coeff2" name))
      ~coeff3:(fv (sf "%s_coeff3" name))
      ~const:(fv (sf "%s_const" name))
      ~break1
      ~break2

  let nlogm_model name =
    (* For instructions with cost function
       [\lambda size1. \lambda size2. const + coeff * size1 log2(size2)] *)
    Model.nlogm
      ~name:(ns name)
      ~intercept:(fv (sf "%s_const" name))
      ~coeff:(fv (sf "%s_coeff" name))

  let concat_model name =
    Model.bilinear_affine
      ~name:(ns name)
      ~intercept:(fv (sf "%s_const" name))
      ~coeff1:(fv (sf "%s_total_bytes" name))
      ~coeff2:(fv (sf "%s_list_length" name))

  let concat_pair_model name =
    Model.linear_sum
      ~name:(ns name)
      ~intercept:(fv (sf "%s_const" name))
      ~coeff:(fv (sf "%s_coeff" name))

  let linear_max_model name =
    (* For instructions with cost function
       [\lambda size1. \lambda size2. const + coeff * max(size1,size2)] *)
    Model.linear_max
      ~name:(ns name)
      ~intercept:(fv (sf "%s_const" name))
      ~coeff:(fv (sf "%s_coeff" name))

  let linear_min_model name =
    (* For instructions with cost function
       [\lambda size1. \lambda size2. const + coeff * min(size1,size2)] *)
    Model.linear_min
      ~name:(ns name)
      ~intercept:(fv (sf "%s_const" name))
      ~coeff:(fv (sf "%s_coeff" name))

  let pack_model name =
    Model.trilinear
      ~name:(ns name)
      ~coeff1:(fv (sf "%s_micheline_nodes" name))
      ~coeff2:(fv (sf "%s_micheline_int_bytes" name))
      ~coeff3:(fv (sf "%s_micheline_string_bytes" name))

  let open_chest_model name =
    let module M = struct
      type arg_type = int * (int * unit)

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size -> size

        let arity = Model.arity_2

        let model =
          lam ~name:"size1" @@ fun size1 ->
          lam ~name:"size2" @@ fun size2 ->
          free ~name:(fv (sf "%s_const" name))
          + (free ~name:(fv (sf "%s_log_time_coeff" name)) * size1)
          + (free ~name:(fv (sf "%s_plaintext_coeff" name)) * size2)
      end

      let name = ns name
    end in
    (module M : Model.Model_impl with type arg_type = int * (int * unit))

  let verify_update_model name =
    Model.bilinear_affine
      ~name:(ns name)
      ~intercept:(fv (sf "%s_const" name))
      ~coeff1:(fv (sf "%s_inputs" name))
      ~coeff2:(fv (sf "%s_ouputs" name))

  let list_enter_body_model name =
    let module M = struct
      type arg_type = int * (int * unit)

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size -> size

        let arity = Model.arity_2

        let model =
          lam ~name:"size_xs" @@ fun size_xs ->
          lam ~name:"size_ys" @@ fun size_ys ->
          if_
            (eq size_xs (int 0))
            (free ~name:(fv (sf "%s_const" name))
            + (free ~name:(fv (sf "%s_coeff" name)) * size_ys))
            (free ~name:(fv (sf "%s_iter" name)))
      end

      let name = ns name
    end in
    (module M : Model.Model_impl with type arg_type = int * (int * unit))

  let branching_model ~case_0 ~case_1 name =
    let module M = struct
      type arg_type = int * unit

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size

        let arity = Model.arity_1

        let model =
          lam ~name:"size" @@ fun size ->
          if_
            (eq size (int 0))
            (free ~name:(fv (sf "%s_%s" name case_0)))
            (free ~name:(fv (sf "%s_%s" name case_1)))
      end

      let name = ns name
    end in
    (module M : Model.Model_impl with type arg_type = int * unit)

  let empty_branch_model name =
    branching_model ~case_0:"empty" ~case_1:"nonempty" name

  let apply_model name = branching_model ~case_0:"lam" ~case_1:"lamrec" name

  let join_tickets_model name =
    let module M = struct
      type arg_type = int * (int * (int * (int * unit)))

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size -> size -> size -> size

        let arity = Model.Succ_arity Model.arity_3

        let model =
          lam ~name:"content_size_x" @@ fun content_size_x ->
          lam ~name:"content_size_y" @@ fun content_size_y ->
          lam ~name:"amount_size_x" @@ fun amount_size_x ->
          lam ~name:"amount_size_y" @@ fun amount_size_y ->
          free ~name:(fv (sf "%s_const" name))
          + free ~name:(fv (sf "%s_compare_coeff" name))
            * min content_size_x content_size_y
          + free ~name:(fv (sf "%s_add_coeff" name))
            * max amount_size_x amount_size_y
      end

      let name = ns name
    end in
    (module M : Model.Model_impl
      with type arg_type = int * (int * (int * (int * unit))))

  let lsl_bytes_model name =
    Model.bilinear_affine
      ~name:(ns name)
      ~intercept:(fv (sf "%s_const" name))
      ~coeff1:(fv (sf "%s_bytes" name))
      ~coeff2:(fv (sf "%s_shift" name))

  let lsr_bytes_model name =
    let const = fv (sf "%s_const" name) in
    let coeff = fv (sf "%s_coeff" name) in
    let module M = struct
      type arg_type = int * (int * unit)

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size -> size

        let arity = Model.arity_2

        let model =
          lam ~name:"size1" @@ fun size1 ->
          lam ~name:"size2" @@ fun size2 ->
          (* Note that [q] is guaranteed to be non-negative because we use
             saturated subtraction. When [size1 < size2], the model evaluates to
             [const] as expected. *)
          let_ ~name:"q" (sat_sub size1 (size2 * float 0.125)) @@ fun q ->
          free ~name:const + (free ~name:coeff * q)
      end

      let name = ns name
    end in
    (module M : Model.Model_impl with type arg_type = int * (int * unit))
end

let ir_model ?specialization instr_or_cont =
  let open Interpreter_workload in
  let open Models in
  let name = name_of_instr_or_cont ?specialization instr_or_cont in
  match instr_or_cont with
  | Instr_name instr -> (
      match instr with
      | N_IDrop | N_IDup | N_ISwap | N_IConst | N_ICons_pair | N_ICar | N_ICdr
      | N_ICons_some | N_ICons_none | N_IIf_none | N_IOpt_map | N_ILeft
      | N_IRight | N_IIf_left | N_ICons_list | N_INil | N_IIf_cons
      | N_IEmpty_set | N_IEmpty_map | N_IEmpty_big_map | N_IOr | N_IAnd | N_IXor
      | N_INot | N_IIf | N_ILoop | N_ILoop_left | N_IDip | N_IExec | N_IView
      | N_ILambda | N_IFailwith | N_IAddress | N_ICreate_contract
      | N_ISet_delegate | N_INow | N_IMin_block_time | N_IBalance | N_IHash_key
      | N_IUnpack | N_ISource | N_ISender | N_ISelf | N_IAmount | N_IChainId
      | N_ILevel | N_ISelf_address | N_INever | N_IUnpair | N_IVoting_power
      | N_ITotal_voting_power | N_IList_size | N_ISet_size | N_IMap_size
      | N_ISapling_empty_state ->
          model_0 instr_or_cont (const1_model name)
      | N_ISet_mem | N_ISet_update | N_IMap_mem | N_IMap_get | N_IMap_update
      | N_IBig_map_mem | N_IBig_map_get | N_IBig_map_update
      | N_IMap_get_and_update | N_IBig_map_get_and_update ->
          model_2 instr_or_cont (nlogm_model name)
      | N_IConcat_string -> model_2 instr_or_cont (concat_model name)
      | N_IConcat_string_pair -> model_2 instr_or_cont (concat_pair_model name)
      | N_ISlice_string -> model_1 instr_or_cont (affine_model name)
      | N_IString_size -> model_0 instr_or_cont (const1_model name)
      | N_IConcat_bytes -> model_2 instr_or_cont (concat_model name)
      | N_IConcat_bytes_pair -> model_2 instr_or_cont (concat_pair_model name)
      | N_ISlice_bytes -> model_1 instr_or_cont (affine_model name)
      | N_IBytes_size -> model_0 instr_or_cont (const1_model name)
      | N_IOr_bytes -> model_2 instr_or_cont (linear_max_model name)
      | N_IAnd_bytes -> model_2 instr_or_cont (linear_min_model name)
      | N_IXor_bytes -> model_2 instr_or_cont (linear_max_model name)
      | N_INot_bytes -> model_1 instr_or_cont (affine_model name)
      | N_ILsl_bytes -> model_2 instr_or_cont (lsl_bytes_model name)
      | N_ILsr_bytes -> model_2 instr_or_cont (lsr_bytes_model name)
      | N_IBytes_nat -> model_1 instr_or_cont (affine_model name)
      | N_INat_bytes -> model_1 instr_or_cont (affine_model name)
      | N_IBytes_int -> model_1 instr_or_cont (affine_model name)
      | N_IInt_bytes -> model_1 instr_or_cont (affine_model name)
      | N_IAdd_seconds_to_timestamp | N_IAdd_timestamp_to_seconds
      | N_ISub_timestamp_seconds | N_IDiff_timestamps ->
          model_2 instr_or_cont (linear_max_model name)
      | N_IAdd_tez | N_ISub_tez | N_ISub_tez_legacy | N_IEdiv_tez ->
          model_0 instr_or_cont (const1_model name)
      | N_IMul_teznat | N_IMul_nattez ->
          model_1 instr_or_cont (affine_model name)
      | N_IEdiv_teznat -> model_2 instr_or_cont (division_cost name)
      | N_IIs_nat -> model_0 instr_or_cont (const1_model name)
      | N_INeg -> model_1 instr_or_cont (affine_model name)
      | N_IAbs_int -> model_1 instr_or_cont (affine_model name)
      | N_IInt_nat -> model_0 instr_or_cont (const1_model name)
      | N_IAdd_int -> model_2 instr_or_cont (linear_max_model name)
      | N_IAdd_nat -> model_2 instr_or_cont (linear_max_model name)
      | N_ISub_int -> model_2 instr_or_cont (linear_max_model name)
      | N_IMul_int -> model_2 instr_or_cont (addlogadd name)
      | N_IMul_nat -> model_2 instr_or_cont (addlogadd name)
      | N_IEdiv_int -> model_2 instr_or_cont (division_cost name)
      | N_IEdiv_nat -> model_2 instr_or_cont (division_cost name)
      | N_ILsl_nat -> model_1 instr_or_cont (affine_model name)
      | N_ILsr_nat -> model_1 instr_or_cont (affine_model name)
      | N_IOr_nat -> model_2 instr_or_cont (linear_max_model name)
      | N_IAnd_nat -> model_2 instr_or_cont (linear_min_model name)
      | N_IAnd_int_nat -> model_2 instr_or_cont (linear_min_model name)
      | N_IXor_nat -> model_2 instr_or_cont (linear_max_model name)
      | N_INot_int -> model_1 instr_or_cont (affine_model name)
      | N_ICompare -> model_2 instr_or_cont (linear_min_model name)
      | N_IEq | N_INeq | N_ILt | N_IGt | N_ILe | N_IGe ->
          model_0 instr_or_cont (const1_model name)
      | N_IPack -> model_3 instr_or_cont (pack_model name)
      | N_IBlake2b | N_ISha256 | N_ISha512 | N_IKeccak | N_ISha3 ->
          model_1 instr_or_cont (affine_model name)
      | N_ICheck_signature_ed25519 | N_ICheck_signature_secp256k1
      | N_ICheck_signature_p256 | N_ICheck_signature_bls ->
          model_1 instr_or_cont (affine_model name)
      | N_IContract | N_ITransfer_tokens | N_IImplicit_account ->
          model_0 instr_or_cont (const1_model name)
      (* The following two instructions are expected to have an affine model. However,
         we observe 3 affine parts, on [0;300], [300;400] and [400;\inf[. *)
      | N_IDupN -> model_1 instr_or_cont (break_model_2 name 300 400)
      | N_IDropN -> model_1 instr_or_cont (break_model_2_const name 300 400)
      | N_IDig | N_IDug | N_IDipN -> model_1 instr_or_cont (affine_model name)
      | N_IAdd_bls12_381_g1 | N_IAdd_bls12_381_g2 | N_IAdd_bls12_381_fr
      | N_IMul_bls12_381_g1 | N_IMul_bls12_381_g2 | N_IMul_bls12_381_fr
      | N_INeg_bls12_381_g1 | N_INeg_bls12_381_g2 | N_INeg_bls12_381_fr
      | N_IInt_bls12_381_z_fr ->
          model_0 instr_or_cont (const1_model name)
      | N_IMul_bls12_381_fr_z | N_IMul_bls12_381_z_fr
      | N_IPairing_check_bls12_381 ->
          model_1 instr_or_cont (affine_model name)
      | N_IComb_get | N_IComb | N_IComb_set | N_IUncomb ->
          model_1 instr_or_cont (affine_model name)
      | N_ITicket | N_IRead_ticket -> model_0 instr_or_cont (const1_model name)
      | N_ISplit_ticket -> model_2 instr_or_cont (linear_max_model name)
      | N_IJoin_tickets -> model_4 instr_or_cont (join_tickets_model name)
      | N_ISapling_verify_update ->
          model_2 instr_or_cont (verify_update_model name)
      | N_IList_map -> model_0 instr_or_cont (const1_model name)
      | N_IList_iter -> model_0 instr_or_cont (const1_model name)
      | N_IIter -> model_0 instr_or_cont (const1_model name)
      | N_IMap_map -> model_1 instr_or_cont (affine_model name)
      | N_IMap_iter -> model_1 instr_or_cont (affine_model name)
      | N_ISet_iter -> model_1 instr_or_cont (affine_model name)
      | N_IHalt -> model_0 instr_or_cont (const1_model name)
      | N_IApply -> model_1 instr_or_cont (apply_model name)
      | N_ILog -> model_0 instr_or_cont (const1_model name)
      | N_IOpen_chest -> model_2 instr_or_cont (open_chest_model name)
      | N_IEmit -> model_0 instr_or_cont (const1_model name))
  | Cont_name cont -> (
      match cont with
      | N_KNil -> model_0 instr_or_cont (const1_model name)
      | N_KCons -> model_0 instr_or_cont (const1_model name)
      | N_KReturn -> model_0 instr_or_cont (const1_model name)
      | N_KView_exit -> model_0 instr_or_cont (const1_model name)
      | N_KMap_head -> model_0 instr_or_cont (const1_model name)
      | N_KUndip -> model_0 instr_or_cont (const1_model name)
      | N_KLoop_in -> model_0 instr_or_cont (const1_model name)
      | N_KLoop_in_left -> model_0 instr_or_cont (const1_model name)
      | N_KIter -> model_1 instr_or_cont (empty_branch_model name)
      | N_KList_enter_body -> model_2 instr_or_cont (list_enter_body_model name)
      | N_KList_exit_body -> model_0 instr_or_cont (const1_model name)
      | N_KMap_enter_body -> model_1 instr_or_cont (empty_branch_model name)
      | N_KMap_exit_body -> model_2 instr_or_cont (nlogm_model name)
      | N_KLog -> model_0 instr_or_cont (const1_model name))

let amplification_loop_iteration = fv "amplification_loop_iteration"

let amplification_loop_model =
  Model.make
    ~conv:(fun iterations -> (iterations, ()))
    ~model:(Model.linear ~name:(ns "amp") ~coeff:amplification_loop_iteration)

(* The following model stitches together the per-instruction models and
   adds a term corresponding to the latency induced by the timer itself. *)
let interpreter_model ?amplification sub_model =
  Model.make_aggregated
    ~model:(fun trace ->
      let module Def (X : Costlang.S) = struct
        type t = X.size X.repr

        let applied =
          let initial =
            match amplification with
            | None -> X.int 0
            | Some amplification_factor ->
                let (module Amplification_applied) =
                  Model.apply amplification_loop_model amplification_factor
                in
                let module Amplification_result = Amplification_applied (X) in
                Amplification_result.applied
          in
          List.fold_left
            (fun (acc : X.size X.repr) instr_trace ->
              let (module Applied_instr) =
                Model.apply
                  (ir_model instr_trace.Interpreter_workload.name)
                  instr_trace
              in
              let module R = Applied_instr (X) in
              X.(acc + R.applied))
            initial
            trace
      end in
      ((module Def) : Model.applied))
    ~sub_models:[sub_model]

let make_model ?amplification instr_name =
  (* When generating code, we don't want to consider the terms specific to
     Lwt and to the timer latency. Also, we restrict to single instructions. *)
  let ir_model =
    match ir_model instr_name with
    | Aggregate _ -> assert false
    | Abstract {model; _} -> Model.Model model
  in
  [("interpreter", interpreter_model ?amplification ir_model)]
