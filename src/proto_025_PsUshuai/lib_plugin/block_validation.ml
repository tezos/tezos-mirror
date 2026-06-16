(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

(* Shell-side state threaded through block validation. We only need the level of
   the block being validated to bound DAL page-proof levels (see below). *)
type block_validation_state = {current_level : Raw_level.t}

let init_block_validation_state validation_state : block_validation_state =
  let ctxt = Validate.get_initial_ctxt validation_state in
  {current_level = (Level.current ctxt).level}

let shell_fail err : 'a Environment.Error_monad.shell_tzresult Lwt.t =
  Lwt.return_error [Environment.wrap_tzerror err]

(* A smart-rollup refutation [Proof] move whose DAL page proof references a
   [published_level] greater than the level at which the operation is processed
   cannot correspond to a published slot (a legitimate proof references a slot
   published in the past, so [published_level <= current level]), and it makes
   the protocol's page-validity check compute an out-of-range level
   ([Raw_level_repr.add] overflowing into a negative [int32] and tripping an
   assertion in the frozen [lib_protocol], aborting block application).

   Since [lib_protocol] is frozen, this filtering lives in the plugin (used both
   by block validation, [check_block_operation], and by the mempool,
   [Mempool.pre_filter]). The error is [`Temporary] rather than [`Permanent]:
   the operation is not invalid forever -- it is rejected only because its
   [published_level] is greater than the current level, and as the head
   advances that level may be reached, after which the operation could
   become valid. [`Temporary] is the category whose generic classification
   (see [prevalidation.ml]) is [`Branch_delayed], the classification
   [Mempool.pre_filter] assigns here. *)
type Environment.Error_monad.error +=
  | Sc_rollup_refute_dal_proof_future_published_level of {
      published_level : Raw_level.t;
      level : Raw_level.t;
    }

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Temporary
    ~id:
      "block_validation_plugin.sc_rollup_refute_dal_proof_future_published_level"
    ~title:"Sc_rollup refutation with a future DAL published level"
    ~description:
      "A smart-rollup refutation proof references a DAL page whose published \
       level is greater than the current level. Applying it would overflow an \
       internal level computation, so it is rejected by the plugin."
    ~pp:(fun ppf (published_level, level) ->
      Format.fprintf
        ppf
        "Smart-rollup refutation rejected: its DAL page proof has \
         published_level %a, which is greater than the current level %a."
        Raw_level.pp
        published_level
        Raw_level.pp
        level)
    Data_encoding.(
      obj2
        (req "published_level" Raw_level.encoding)
        (req "level" Raw_level.encoding))
    (function
      | Sc_rollup_refute_dal_proof_future_published_level
          {published_level; level} ->
          Some (published_level, level)
      | _ -> None)
    (fun (published_level, level) ->
      Sc_rollup_refute_dal_proof_future_published_level {published_level; level})

(* The DAL page [published_level] targeted by a refutation [Proof] move, if the
   move carries a DAL page proof. *)
let dal_page_published_level (refutation : Sc_rollup.Game.refutation) =
  match refutation with
  | Sc_rollup.Game.Move
      {
        step =
          Sc_rollup.Game.Proof
            {
              input_proof =
                Some
                  (Sc_rollup.Proof.Reveal_proof
                     (Sc_rollup.Proof.Dal_page_proof {page_id; _}));
              _;
            };
        _;
      } ->
      Some page_id.slot_id.published_level
  | _ -> None

let manager_op_future_dal : type kind.
    level:Raw_level.t -> kind manager_operation -> Raw_level.t option =
 fun ~level operation ->
  match operation with
  | Sc_rollup_refute {refutation; _} -> (
      match dal_page_published_level refutation with
      | Some published_level when Raw_level.(published_level > level) ->
          Some published_level
      | _ -> None)
  | _ -> None

(* [find_future_dal_refute ~level op] returns [Some published_level] when [op]
   contains a [Sc_rollup_refute] whose DAL page proof targets a [published_level]
   greater than [level]. [level] is the level at which the operation is
   processed: the block's own level for block validation, and [head + 1] (the
   block the operation would be baked into) for the mempool. *)
let find_future_dal_refute ~level
    ({protocol_data = Operation_data {contents; _}; _} : packed_operation) =
  List.find_map
    (function
      | Contents (Manager_operation {operation; _}) ->
          manager_op_future_dal ~level operation
      | Contents _ -> None)
    (Operation.to_list (Contents_list contents))

let check_block_operation ({current_level} as state) (op : packed_operation) :
    block_validation_state Environment.Error_monad.shell_tzresult Lwt.t =
  match find_future_dal_refute ~level:current_level op with
  | Some published_level ->
      shell_fail
        (Sc_rollup_refute_dal_proof_future_published_level
           {published_level; level = current_level})
  | None -> Lwt_result_syntax.return state
