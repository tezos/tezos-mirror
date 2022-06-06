(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

module V2_0_0 = struct
  open Sc_rollup_repr
  module PS = Sc_rollup_PVM_sem

  module type P = sig
    module Tree :
      Context.TREE with type key = string list and type value = bytes

    type tree = Tree.tree

    type proof

    val proof_encoding : proof Data_encoding.t

    val proof_before : proof -> State_hash.t

    val proof_after : proof -> State_hash.t

    val verify_proof :
      proof -> (tree -> (tree * 'a) Lwt.t) -> (tree * 'a) option Lwt.t

    val produce_proof :
      Tree.t -> tree -> (tree -> (tree * 'a) Lwt.t) -> (proof * 'a) option Lwt.t
  end

  module type S = sig
    include Sc_rollup_PVM_sem.S

    val name : string

    val parse_boot_sector : string -> string option

    val pp_boot_sector : Format.formatter -> string -> unit

    (** [get_tick state] gets the total tick counter for the given PVM state. *)
    val get_tick : state -> Sc_rollup_tick_repr.t Lwt.t

    (** PVM status *)
    type status = Computing | WaitingForInputMessage

    (** [get_status state] gives you the current execution status for the PVM. *)
    val get_status : state -> status Lwt.t
  end

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3091

     The tree proof contains enough information to derive given and requested.
     Get rid of the duplication by writing the projection functions and
     removing the [given] and [requested] fields.
  *)
  type 'a proof = {
    tree_proof : 'a;
    given : PS.input option;
    requested : PS.input_request;
  }

  let proof_encoding e =
    let open Data_encoding in
    conv
      (fun {tree_proof; given; requested} -> (tree_proof, given, requested))
      (fun (tree_proof, given, requested) -> {tree_proof; given; requested})
      (obj3
         (req "tree_proof" e)
         (req "given" (option PS.input_encoding))
         (req "requested" PS.input_request_encoding))

  module Make (Context : P) :
    S
      with type context = Context.Tree.t
       and type state = Context.tree
       and type proof = Context.proof proof = struct
    module Tree = Context.Tree

    type context = Context.Tree.t

    type hash = State_hash.t

    type nonrec proof = Context.proof proof

    let proof_input_given p = p.given

    let proof_input_requested p = p.requested

    let proof_encoding = proof_encoding Context.proof_encoding

    let proof_start_state p = Context.proof_before p.tree_proof

    let proof_stop_state p =
      match (p.given, p.requested) with
      | None, PS.No_input_required -> Some (Context.proof_after p.tree_proof)
      | None, _ -> None
      | _ -> Some (Context.proof_after p.tree_proof)

    let name = "wasm_2_0_0"

    let parse_boot_sector s = Some s

    let pp_boot_sector fmt s = Format.fprintf fmt "%s" s

    type tree = Tree.tree

    type status = Computing | WaitingForInputMessage

    module State = struct
      type state = tree

      module Monad : sig
        type 'a t

        val run : 'a t -> state -> (state * 'a option) Lwt.t

        val return : 'a -> 'a t

        module Syntax : sig
          val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
        end

        val get : tree t

        val set : tree -> unit t

        val lift : 'a Lwt.t -> 'a t

        val find_value : Tree.key -> 'a Data_encoding.t -> 'a option t

        val set_value : Tree.key -> 'a Data_encoding.t -> 'a -> unit t
      end = struct
        type 'a t = state -> (state * 'a option) Lwt.t

        let return x state = Lwt.return (state, Some x)

        let bind m f state =
          let open Lwt_syntax in
          let* state, res = m state in
          match res with
          | None -> return (state, None)
          | Some res -> f res state

        module Syntax = struct
          let ( let* ) = bind
        end

        let run m state = m state

        let internal_error_key = ["internal_error"]

        let internal_error msg tree =
          let open Lwt_syntax in
          let* tree = Tree.add tree internal_error_key (Bytes.of_string msg) in
          return (tree, None)

        let decode encoding bytes state =
          let open Lwt_syntax in
          match Data_encoding.Binary.of_bytes_opt encoding bytes with
          | None -> internal_error "Error during decoding" state
          | Some v -> return (state, Some v)

        let get s = Lwt.return (s, Some s)

        let set s _ = Lwt.return (s, Some ())

        let lift m s = Lwt.map (fun r -> (s, Some r)) m

        let find_value key encoding state =
          let open Lwt_syntax in
          let* obytes = Tree.find state key in
          match obytes with
          | None -> return (state, Some None)
          | Some bytes ->
              let* state, value = decode encoding bytes state in
              return (state, Some value)

        let set_value key encoding value tree =
          let open Lwt_syntax in
          Data_encoding.Binary.to_bytes_opt encoding value |> function
          | None -> internal_error "Internal_Error during encoding" tree
          | Some bytes ->
              let* tree = Tree.add tree key bytes in
              return (tree, Some ())
      end

      open Monad

      module MakeVar (P : sig
        type t

        val name : string

        val initial : t

        val encoding : t Data_encoding.t
      end) =
      struct
        let key = [P.name]

        let get =
          let open Monad.Syntax in
          let* v = find_value key P.encoding in
          match v with None -> return P.initial | Some v -> return v

        let set = set_value key P.encoding
      end

      module CurrentTick = MakeVar (struct
        include Sc_rollup_tick_repr

        let name = "tick"
      end)

      module Boot_sector = MakeVar (struct
        type t = string

        let name = "boot_sector"

        let initial = ""

        let encoding = Data_encoding.string

        let _pp fmt s = Format.fprintf fmt "%s" s
      end)

      module Status = MakeVar (struct
        type t = status

        let initial = Computing

        let encoding =
          Data_encoding.string_enum
            [
              ("Computing", Computing);
              ("WaitingForInputMessage", WaitingForInputMessage);
            ]

        let name = "status"

        let string_of_status = function
          | Computing -> "Computing"
          | WaitingForInputMessage -> "WaitingForInputMessage"

        let _pp fmt status = Format.fprintf fmt "%s" (string_of_status status)
      end)

      module CurrentLevel = MakeVar (struct
        type t = Raw_level_repr.t

        let initial = Raw_level_repr.root

        let encoding = Raw_level_repr.encoding

        let name = "current_level"

        let _pp = Raw_level_repr.pp
      end)

      module MessageCounter = MakeVar (struct
        type t = Z.t option

        let initial = None

        let encoding = Data_encoding.option Data_encoding.n

        let name = "message_counter"

        let _pp fmt = function
          | None -> Format.fprintf fmt "None"
          | Some c -> Format.fprintf fmt "Some %a" Z.pp_print c
      end)

      module NextMessage = MakeVar (struct
        type t = string option

        let initial = None

        let encoding = Data_encoding.(option string)

        let name = "next_message"

        let _pp fmt = function
          | None -> Format.fprintf fmt "None"
          | Some s -> Format.fprintf fmt "Some %s" s
      end)
    end

    module WASM_machine = Wasm_2_0_0.Make (Tree)
    open State

    type state = State.state

    open Monad

    let initial_state ctxt _boot_sector =
      let state = Tree.empty ctxt in
      Lwt.return state

    let state_hash state =
      let m =
        let open Monad.Syntax in
        let* status = Status.get in
        match status with
        | _ ->
            Context_hash.to_bytes @@ Tree.hash state |> fun h ->
            return @@ State_hash.hash_bytes [h]
      in
      let open Lwt_syntax in
      let* state = Monad.run m state in
      match state with
      | _, Some hash -> return hash
      | _ -> (* Hash computation always succeeds. *) assert false

    let result_of ~default m state =
      let open Lwt_syntax in
      let* _, v = run m state in
      match v with None -> return default | Some v -> return v

    let state_of m state =
      let open Lwt_syntax in
      let* s, _ = run m state in
      return s

    let get_tick =
      result_of ~default:Sc_rollup_tick_repr.initial CurrentTick.get

    let is_input_state_monadic =
      let open Monad.Syntax in
      let* status = Status.get in
      match status with
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/3092

         Implement handling of input logic.
      *)
      | WaitingForInputMessage -> (
          let* level = CurrentLevel.get in
          let* counter = MessageCounter.get in
          match counter with
          | Some n -> return (PS.First_after (level, n))
          | None -> return PS.Initial)
      | _ -> return PS.No_input_required

    let is_input_state =
      result_of ~default:PS.No_input_required @@ is_input_state_monadic

    let get_status = result_of ~default:Computing @@ Status.get

    let set_input_monadic input =
      let open PS in
      let {inbox_level; message_counter; payload} = input in
      let open Monad.Syntax in
      let* boot_sector = Boot_sector.get in
      let msg = boot_sector ^ payload in
      let* () = CurrentLevel.set inbox_level in
      let* () = MessageCounter.set (Some message_counter) in
      let* () = NextMessage.set (Some msg) in
      return ()

    let set_input input = state_of @@ set_input_monadic input

    let eval_step =
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/3090

         Call into tickified parsing/evaluation exposed in lib_webassembly.
      *)
      let open Monad.Syntax in
      let* s = get in
      let* s = lift (WASM_machine.step s) in
      set s

    let ticked m =
      let open Monad.Syntax in
      let* tick = CurrentTick.get in
      let* () = CurrentTick.set (Sc_rollup_tick_repr.next tick) in
      m

    let eval state = state_of (ticked eval_step) state

    let step_transition input_given state =
      let open Lwt_syntax in
      let* request = is_input_state state in
      let* state =
        match request with
        | PS.No_input_required -> eval state
        | _ -> (
            match input_given with
            | Some input -> set_input input state
            | None -> return state)
      in
      return (state, request)

    let verify_proof proof =
      let open Lwt_syntax in
      let* result =
        Context.verify_proof proof.tree_proof (step_transition proof.given)
      in
      match result with
      | None -> return false
      | Some (_, request) ->
          return (PS.input_request_equal request proof.requested)

    let produce_proof context input_given state =
      let open Lwt_syntax in
      let* result =
        Context.produce_proof context state (step_transition input_given)
      in
      match result with
      | Some (tree_proof, requested) ->
          return (Result.ok {tree_proof; given = input_given; requested})
      | None -> return (Result.error "Context.produce_proof returned None")

    type output_proof = {
      output_proof_state : hash;
      output_proof_output : PS.output;
    }

    (* FIXME: #3176
       The WASM PVM must provide an implementation for these
       proofs. These are likely to be similar to the proofs about the
       PVM execution steps. *)
    let output_of_output_proof s = s.output_proof_output

    let state_of_output_proof s = s.output_proof_state

    let verify_output_proof _proof = Lwt.return true

    let produce_output_proof _context state output_proof_output =
      let open Lwt_result_syntax in
      let*! output_proof_state = state_hash state in
      return {output_proof_state; output_proof_output}
  end

  module ProtocolImplementation = Make (struct
    module Tree = struct
      include Context.Tree

      type tree = Context.tree

      type t = Context.t

      type key = string list

      type value = bytes
    end

    type tree = Context.tree

    type proof = Context.Proof.tree Context.Proof.t

    let verify_proof p f =
      Lwt.map Result.to_option (Context.verify_tree_proof p f)

    let produce_proof _context _state _f =
      (* Can't produce proof without full context*)
      Lwt.return None

    let kinded_hash_to_state_hash = function
      | `Value hash | `Node hash ->
          State_hash.hash_bytes [Context_hash.to_bytes hash]

    let proof_before proof =
      kinded_hash_to_state_hash proof.Context.Proof.before

    let proof_after proof = kinded_hash_to_state_hash proof.Context.Proof.after

    let proof_encoding = Context.Proof_encoding.V1.Tree32.tree_proof_encoding
  end)
end
