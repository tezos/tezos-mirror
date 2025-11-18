(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol
open Alpha_context
open Generator_descriptors

(** {2 Building the Setup} *)

(** Setup for generating valid operation of several kind of
    operations. It gathers the following information to setup
    {! Generator_descriptor.state} into which valid operations
    can be generated:
    - [nb_cycles] the total number of cycles to bake,
    - [nb_blocks] the number of blocks to bake in the last cycle,
    - [params] the constants required, and
    - [prelude] that associates to each cycle to bake a list of
    {! Generator_descriptors.descriptor} prelude functions. *)
type setup = {
  prelude :
    (int * (state -> (packed_operation list * state) tzresult Lwt.t) list) list;
  nb_cycles : int;
  nb_blocks : int;
  params : Parameters.t;
}

(** Select the prelude actions of a specific cycle in a setup prelude. *)
let prelude_on_cycle (c : int)
    (actions :
      (int * (state -> (packed_operation list * state) tzresult Lwt.t) list)
      list) : (state -> (packed_operation list * state) tzresult Lwt.t) list =
  match List.filter (fun (c1, _actions) -> c = c1) actions with
  | (c1, actions) :: _ ->
      assert (c = c1) ;
      actions
  | [] -> []

(** Knowing the total number of required cycles, normalize a prelude
   on the list of the pair of a cycle and prelude actions. *)
let normalize_preludes nb_cycles (descr : descriptor) =
  let normalize prelude =
    match prelude with
    | On n, actions -> [(nb_cycles - n, actions)]
    | From n, actions ->
        List.fold_left
          (fun acc i -> acc @ [(nb_cycles - n + i, actions)])
          []
          (1 -- n)
  in
  let prim = normalize descr.prelude in
  match descr.opt_prelude with
  | Some prelude -> normalize prelude @ prim
  | None -> prim

(** Insert a normalized prelude in a prelude of a setup.*)
let rec insert_normalize_preludes
    ((n, action) :
      int * (state -> (packed_operation list * state) tzresult Lwt.t))
    (preludes :
      (int * (state -> (packed_operation list * state) tzresult Lwt.t) list)
      list) =
  match preludes with
  | [] -> [(n, [action])]
  | (m, actions) :: rest ->
      if m = n then (m, actions @ [action]) :: rest
      else (m, actions) :: insert_normalize_preludes (n, action) rest

(** Produce a setup prelude from a list of descriptor and a nb of
   cycles*)
let compose_preludes nb_cycles descrs =
  let normalized_preludes = List.map (normalize_preludes nb_cycles) descrs in
  List.fold_left
    (fun acc pre ->
      List.fold_left (fun acc pr -> insert_normalize_preludes pr acc) acc pre)
    []
    normalized_preludes

(** Agregate the parameters of several {! Generator_descriptors.descriptor}.*)
let initiated_params descrs nb_accounts =
  let consensus_committee_size = nb_accounts in
  let initial_params =
    Tezos_protocol_024_PtTALLiN_parameters.Default_parameters
    .parameters_of_constants
      {
        Context.default_test_constants with
        consensus_threshold_size = 0;
        consensus_committee_size;
        dal =
          {
            Context.default_test_constants.dal with
            cryptobox_parameters =
              {
                Context.default_test_constants.dal.cryptobox_parameters with
                number_of_shards = consensus_committee_size;
              };
          };
      }
  in
  let descrs_params = List.map (fun descr -> descr.parameters) descrs in
  List.fold_left (fun acc f -> f acc) initial_params descrs_params

(** Make a [setup] from a list of {! Generator_descriptors.descriptor}. The required number of
    cycles and number of blocks in the last cycle are the maximum of
    required cycle and number of block in the descriptors list. The
    prelude is the composition of the composition of the descriptors
    preludes list -- see [compose_preludes]. The parameters are the agregation of the
    descriptors parameters -- see [initiated_params]. *)
let setup_of descrs nb_accounts =
  let params = initiated_params descrs nb_accounts in
  let max_list l = List.fold_left max 0 l in
  let required_cycle_list l =
    List.map (fun descr -> descr.required_cycle params) l
  in
  let required_block_list l =
    List.map (fun descr -> descr.required_block params) l
  in
  let sorted_descrs =
    List.sort
      (fun pre1 pre2 ->
        Int.compare (pre1.required_cycle params) (pre2.required_cycle params))
      descrs
  in
  let nb_cycles = max_list (required_cycle_list descrs) in
  let nb_blocks = max_list (required_block_list descrs) in
  let prelude = compose_preludes nb_cycles sorted_descrs in
  {prelude; nb_cycles; nb_blocks; params}

(** From a number of accounts and a list of descriptors set up the
    prelude state.

    Thanks to the setup computing for the list of descriptors -- see [setup_of] --,
    initiates a context with the setup parameters, and the number of
    accounts. Initiate a state that will be fulfilled during the
    preludes. During the required number of cycles of the setup, bakes
    each cycle with the setup prelude by selecting the actions to
    perform on it. On the last cycle, bake the required number of
    blocks of the setup. Finally, adds the delegates at the end of
    the prelude in the state. *)
let init nb_accounts descrs =
  let open Lwt_result_syntax in
  let setup = setup_of descrs nb_accounts in
  let* initial_block, bootstraps =
    Context.init_with_parameters_n setup.params nb_accounts
  in
  let my_bake selected_preludes_for_cycle state =
    let* state, operations =
      List.fold_left_es
        (fun (state, ops) prelude ->
          let+ ops', state = prelude state in
          let ops = ops' @ ops in
          (state, ops))
        (state, [])
        selected_preludes_for_cycle
    in
    let b = state.block in
    let operations =
      List.sort (fun op1 op2 -> Operation.compare_by_passes op2 op1) operations
    in
    let+ block = Block.bake ~operations b in
    {state with block; pred = Some b}
  in
  let my_bake_n cycle n state =
    List.fold_left_es
      (fun state _ ->
        let selected_preludes = prelude_on_cycle cycle setup.prelude in
        my_bake selected_preludes state)
      state
      (1 -- n)
  in
  let my_bake_until_cycle_end cycle state =
    let current_level = state.block.Block.header.shell.level in
    let current_level =
      Int32.rem current_level setup.params.constants.blocks_per_cycle
    in
    let delta =
      Int32.sub setup.params.constants.blocks_per_cycle current_level
    in
    my_bake_n cycle (Int32.to_int delta) state
  in
  let* state =
    List.fold_left_es
      (fun state cycle -> my_bake_until_cycle_end cycle state)
      (init_state initial_block ~bootstraps)
      (Stdlib.List.init setup.nb_cycles Fun.id)
  in
  let my_bake_n_default n state =
    List.fold_left_es
      (fun state _ ->
        let pred = state.block in
        let+ block = Block.bake state.block in
        {state with block; pred = Some pred})
      state
      (1 -- n)
  in
  let* state =
    if setup.nb_blocks >= 1 then my_bake_n_default setup.nb_blocks state
    else return state
  in
  return state

(** In a state, generates all the valid operations of a list of kinds. *)
let candidates state kinds nb_bootstrap max_batch_size =
  let open Lwt_result_syntax in
  let* candidates =
    List.fold_left_es
      (fun acc k ->
        let* candidates =
          (descriptor_of k ~nb_bootstrap ~max_batch_size).candidates_generator
            state
        in
        let acc = acc @ candidates in
        return acc)
      []
      kinds
  in
  return candidates

(** From a list of kind of operations generates all the valid
    operations of this kind and the generation state. *)
let covalid ks ~nb_bootstrap ~max_batch_size =
  let open Lwt_result_syntax in
  let* state =
    init nb_bootstrap (descriptors_of ~nb_bootstrap ~max_batch_size ks)
  in
  let* candidates = candidates state ks nb_bootstrap max_batch_size in
  return (state, candidates)
