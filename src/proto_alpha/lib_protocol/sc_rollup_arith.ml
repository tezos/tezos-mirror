(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context
open Sc_rollup

module type P = sig
  module Tree : Context.TREE with type key = string list and type value = bytes

  type tree = Tree.tree

  type proof

  val proof_encoding : proof Data_encoding.t

  val proof_start_state : proof -> State_hash.t

  val proof_stop_state : proof -> State_hash.t

  val verify_proof :
    proof ->
    (tree -> (tree * 'a) Lwt.t) ->
    ( tree * 'a,
      [ `Proof_mismatch of string
      | `Stream_too_long of string
      | `Stream_too_short of string ] )
    result
    Lwt.t
end

module type S = sig
  include Sc_rollup_PVM_sem.S

  val name : string

  val parse_boot_sector : string -> string option

  val pp_boot_sector : Format.formatter -> string -> unit

  val pp : state -> (Format.formatter -> unit -> unit) Lwt.t

  val get_tick : state -> Sc_rollup.Tick.t Lwt.t

  type status = Halted | WaitingForInputMessage | Parsing | Evaluating

  val get_status : state -> status Lwt.t

  type instruction = IPush : int -> instruction | IAdd : instruction

  val equal_instruction : instruction -> instruction -> bool

  val pp_instruction : Format.formatter -> instruction -> unit

  val get_parsing_result : state -> bool option Lwt.t

  val get_code : state -> instruction list Lwt.t

  val get_stack : state -> int list Lwt.t

  val get_evaluation_result : state -> bool option Lwt.t

  val get_is_stuck : state -> string option Lwt.t
end

module Make (Context : P) : S with type context = Context.Tree.t = struct
  module Tree = Context.Tree

  type context = Context.Tree.t

  type hash = State_hash.t

  type proof = Context.proof

  let proof_encoding = Context.proof_encoding

  let proof_start_state = Context.proof_start_state

  let proof_stop_state = Context.proof_stop_state

  let name = "arith"

  let parse_boot_sector s = Some s

  let pp_boot_sector fmt s = Format.fprintf fmt "%s" s

  type tree = Tree.tree

  type status = Halted | WaitingForInputMessage | Parsing | Evaluating

  type instruction = IPush : int -> instruction | IAdd : instruction

  let equal_instruction i1 i2 =
    match (i1, i2) with
    | (IPush x, IPush y) -> Compare.Int.(x = y)
    | (IAdd, IAdd) -> true
    | (_, _) -> false

  let pp_instruction fmt = function
    | IPush x -> Format.fprintf fmt "push(%d)" x
    | IAdd -> Format.fprintf fmt "add"

  (*

     The machine state is represented using a Merkle tree.

     Here is the data model of this state represented in the tree:

     - tick : Tick.t
       The current tick counter of the machine.
     - status : status
       The current status of the machine.
     - stack : int deque
       The stack of integers.
     - next_message : string option
       The current input message to be processed.
     - code : instruction deque
       The instructions parsed from the input message.
     - lexer_state : int * int
       The internal state of the lexer.
     - parsing_state : parsing_state
       The internal state of the parser.
     - parsing_result : bool option
       The outcome of parsing.
     - evaluation_result : bool option
       The outcome of evaluation.

  *)
  module State = struct
    type state = tree

    module Monad : sig
      type 'a t

      val run : 'a t -> state -> (state * 'a option) Lwt.t

      val is_stuck : string option t

      val internal_error : string -> 'a t

      val return : 'a -> 'a t

      module Syntax : sig
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
      end

      val remove : Tree.key -> unit t

      val find_value : Tree.key -> 'a Data_encoding.t -> 'a option t

      val get_value : default:'a -> Tree.key -> 'a Data_encoding.t -> 'a t

      val set_value : Tree.key -> 'a Data_encoding.t -> 'a -> unit t
    end = struct
      type 'a t = state -> (state * 'a option) Lwt.t

      let return x state = Lwt.return (state, Some x)

      let bind m f state =
        let open Lwt_syntax in
        let* (state, res) = m state in
        match res with None -> return (state, None) | Some res -> f res state

      module Syntax = struct
        let ( let* ) = bind
      end

      let run m state = m state

      let internal_error_key = ["internal_error"]

      let internal_error msg tree =
        let open Lwt_syntax in
        let* tree = Tree.add tree internal_error_key (Bytes.of_string msg) in
        return (tree, None)

      let is_stuck tree =
        let open Lwt_syntax in
        let* v = Tree.find tree internal_error_key in
        return (tree, Some (Option.map Bytes.to_string v))

      let remove key tree =
        let open Lwt_syntax in
        let* tree = Tree.remove tree key in
        return (tree, Some ())

      let find_value key encoding state =
        let open Lwt_syntax in
        let* obytes = Tree.find state key in
        match obytes with
        | None -> return (state, Some None)
        | Some bytes -> (
            match Data_encoding.Binary.of_bytes_opt encoding bytes with
            | None -> internal_error "Internal_Error during decoding" state
            | Some v -> return (state, Some (Some v)))

      let get_value ~default key encoding =
        let open Syntax in
        let* ov = find_value key encoding in
        match ov with None -> return default | Some x -> return x

      let set_value key encoding value tree =
        let open Lwt_syntax in
        Data_encoding.Binary.to_bytes_opt encoding value |> function
        | None -> internal_error "Internal_Error during encoding" tree
        | Some bytes ->
            let* tree = Tree.add tree key bytes in
            return (tree, Some ())
    end

    open Monad
    open Monad.Syntax

    module MakeVar (P : sig
      type t

      val name : string

      val initial : t

      val pp : Format.formatter -> t -> unit

      val encoding : t Data_encoding.t
    end) =
    struct
      let key = [P.name]

      let create = set_value key P.encoding P.initial

      let get =
        let* v = find_value key P.encoding in
        match v with
        | None ->
            (* This case should not happen if [create] is properly called. *)
            return P.initial
        | Some v -> return v

      let set = set_value key P.encoding

      let pp =
        let open Monad.Syntax in
        let* v = get in
        return @@ fun fmt () -> Format.fprintf fmt "@[%s : %a@]" P.name P.pp v
    end

    module MakeDeque (P : sig
      type t

      val name : string

      val encoding : t Data_encoding.t
    end) =
    struct
      (*

         A stateful deque.

         [[head; end[] is the index range for the elements of the deque.

         The length of the deque is therefore [end - head].

      *)

      let head_key = [P.name; "head"]

      let end_key = [P.name; "end"]

      let get_head = get_value ~default:Z.zero head_key Data_encoding.z

      let set_head = set_value head_key Data_encoding.z

      let get_end = get_value ~default:(Z.of_int 0) end_key Data_encoding.z

      let set_end = set_value end_key Data_encoding.z

      let idx_key idx = [P.name; Z.to_string idx]

      let push x =
        let open Monad.Syntax in
        let* head_idx = get_head in
        let head_idx' = Z.pred head_idx in
        let* () = set_head head_idx' in
        set_value (idx_key head_idx') P.encoding x

      let pop =
        let open Monad.Syntax in
        let* head_idx = get_head in
        let* end_idx = get_end in
        if Z.(leq end_idx head_idx) then return None
        else
          let* v = find_value (idx_key head_idx) P.encoding in
          match v with
          | None -> (* By invariants of the Deque. *) assert false
          | Some x ->
              let* () = remove (idx_key head_idx) in
              let head_idx = Z.succ head_idx in
              let* () = set_head head_idx in
              return (Some x)

      let inject x =
        let open Monad.Syntax in
        let* end_idx = get_end in
        let end_idx' = Z.succ end_idx in
        let* () = set_end end_idx' in
        set_value (idx_key end_idx) P.encoding x

      let to_list =
        let open Monad.Syntax in
        let* head_idx = get_head in
        let* end_idx = get_end in
        let rec aux l idx =
          if Z.(lt idx head_idx) then return l
          else
            let* v = find_value (idx_key idx) P.encoding in
            match v with
            | None -> (* By invariants of deque *) assert false
            | Some v -> aux (v :: l) (Z.pred idx)
        in
        aux [] (Z.pred end_idx)

      let clear = remove [P.name]
    end

    module CurrentTick = MakeVar (struct
      include Tick

      let name = "tick"
    end)

    module Stack = MakeDeque (struct
      type t = int

      let name = "stack"

      let encoding = Data_encoding.int31
    end)

    module Code = MakeDeque (struct
      type t = instruction

      let name = "code"

      let encoding =
        Data_encoding.(
          union
            [
              case
                ~title:"push"
                (Tag 0)
                Data_encoding.int31
                (function IPush x -> Some x | _ -> None)
                (fun x -> IPush x);
              case
                ~title:"add"
                (Tag 1)
                Data_encoding.unit
                (function IAdd -> Some () | _ -> None)
                (fun () -> IAdd);
            ])
    end)

    module Boot_sector = MakeVar (struct
      type t = string

      let name = "boot_sector"

      let initial = ""

      let encoding = Data_encoding.string

      let pp fmt s = Format.fprintf fmt "%s" s
    end)

    module Status = MakeVar (struct
      type t = status

      let initial = Halted

      let encoding =
        Data_encoding.string_enum
          [
            ("Halted", Halted);
            ("WaitingForInput", WaitingForInputMessage);
            ("Parsing", Parsing);
            ("Evaluating", Evaluating);
          ]

      let name = "status"

      let string_of_status = function
        | Halted -> "Halted"
        | WaitingForInputMessage -> "WaitingForInputMessage"
        | Parsing -> "Parsing"
        | Evaluating -> "Evaluating"

      let pp fmt status = Format.fprintf fmt "%s" (string_of_status status)
    end)

    module CurrentLevel = MakeVar (struct
      type t = Raw_level.t

      let initial = Raw_level.root

      let encoding = Raw_level.encoding

      let name = "current_level"

      let pp = Raw_level.pp
    end)

    module MessageCounter = MakeVar (struct
      type t = Z.t

      let initial = Z.(pred zero)

      let encoding = Data_encoding.n

      let name = "message_counter"

      let pp = Z.pp_print
    end)

    module NextMessage = MakeVar (struct
      type t = string option

      let initial = None

      let encoding = Data_encoding.(option string)

      let name = "next_message"

      let pp fmt = function
        | None -> Format.fprintf fmt "None"
        | Some s -> Format.fprintf fmt "Some %s" s
    end)

    type parser_state = ParseInt | SkipLayout

    module LexerState = MakeVar (struct
      type t = int * int

      let name = "lexer_buffer"

      let initial = (-1, -1)

      let encoding = Data_encoding.(tup2 int31 int31)

      let pp fmt (start, len) =
        Format.fprintf fmt "lexer.(start = %d, len = %d)" start len
    end)

    module ParserState = MakeVar (struct
      type t = parser_state

      let name = "parser_state"

      let initial = SkipLayout

      let encoding =
        Data_encoding.string_enum
          [("ParseInt", ParseInt); ("SkipLayout", SkipLayout)]

      let pp fmt = function
        | ParseInt -> Format.fprintf fmt "Parsing int"
        | SkipLayout -> Format.fprintf fmt "Skipping layout"
    end)

    module ParsingResult = MakeVar (struct
      type t = bool option

      let name = "parsing_result"

      let initial = None

      let encoding = Data_encoding.(option bool)

      let pp fmt = function
        | None -> Format.fprintf fmt "n/a"
        | Some true -> Format.fprintf fmt "parsing succeeds"
        | Some false -> Format.fprintf fmt "parsing fails"
    end)

    module EvaluationResult = MakeVar (struct
      type t = bool option

      let name = "evaluation_result"

      let initial = None

      let encoding = Data_encoding.(option bool)

      let pp fmt = function
        | None -> Format.fprintf fmt "n/a"
        | Some true -> Format.fprintf fmt "evaluation succeeds"
        | Some false -> Format.fprintf fmt "evaluation fails"
    end)

    let pp =
      let open Monad.Syntax in
      let* status_pp = Status.pp in
      let* message_counter_pp = MessageCounter.pp in
      let* next_message_pp = NextMessage.pp in
      let* parsing_result_pp = ParsingResult.pp in
      let* parser_state_pp = ParserState.pp in
      let* lexer_state_pp = LexerState.pp in
      let* evaluation_result_pp = EvaluationResult.pp in
      return @@ fun fmt () ->
      Format.fprintf
        fmt
        "@[<v 0 >@;%a@;%a@;%a@;%a@;%a@;%a@;%a@]"
        status_pp
        ()
        message_counter_pp
        ()
        next_message_pp
        ()
        parsing_result_pp
        ()
        parser_state_pp
        ()
        lexer_state_pp
        ()
        evaluation_result_pp
        ()
  end

  open State

  type state = State.state

  let pp state =
    let open Lwt_syntax in
    let* (_, pp) = Monad.run pp state in
    match pp with
    | None -> return @@ fun fmt _ -> Format.fprintf fmt "<opaque>"
    | Some pp -> return pp

  open Monad

  let initial_state ctxt boot_sector =
    let state = Tree.empty ctxt in
    let m =
      let open Monad.Syntax in
      let* () = Boot_sector.set boot_sector in
      let* () = Status.set Halted in
      return ()
    in
    let open Lwt_syntax in
    let* (state, _) = run m state in
    return state

  let state_hash state =
    let m =
      let open Monad.Syntax in
      let* status = Status.get in
      match status with
      | Halted -> return State_hash.zero
      | _ ->
          Context_hash.to_bytes @@ Tree.hash state |> fun h ->
          return @@ State_hash.hash_bytes [h]
    in
    let open Lwt_syntax in
    let* state = Monad.run m state in
    match state with
    | (_, Some hash) -> return hash
    | _ -> (* Hash computation always succeeds. *) assert false

  let boot =
    let open Monad.Syntax in
    let* () = Status.create in
    let* () = NextMessage.create in
    let* () = Status.set WaitingForInputMessage in
    return ()

  let result_of ~default m state =
    let open Lwt_syntax in
    let* (_, v) = run m state in
    match v with None -> return default | Some v -> return v

  let state_of m state =
    let open Lwt_syntax in
    let* (s, _) = run m state in
    return s

  let get_tick = result_of ~default:Tick.initial CurrentTick.get

  let is_input_state_monadic =
    let open Monad.Syntax in
    let* status = Status.get in
    match status with
    | WaitingForInputMessage ->
        let* level = CurrentLevel.get in
        let* counter = MessageCounter.get in
        return (Some (level, counter))
    | _ -> return None

  let is_input_state = result_of ~default:None @@ is_input_state_monadic

  let get_status = result_of ~default:WaitingForInputMessage @@ Status.get

  let get_code = result_of ~default:[] @@ Code.to_list

  let get_parsing_result = result_of ~default:None @@ ParsingResult.get

  let get_stack = result_of ~default:[] @@ Stack.to_list

  let get_evaluation_result = result_of ~default:None @@ EvaluationResult.get

  let get_is_stuck = result_of ~default:None @@ is_stuck

  type input = {
    inbox_level : Raw_level.t;
    message_counter : Z.t;
    payload : string;
  }

  let set_input_monadic {inbox_level; message_counter; payload} =
    let open Monad.Syntax in
    let* boot_sector = Boot_sector.get in
    let msg = boot_sector ^ payload in
    let* last_level = CurrentLevel.get in
    let* last_counter = MessageCounter.get in
    let update =
      let* () = CurrentLevel.set inbox_level in
      let* () = MessageCounter.set message_counter in
      let* () = NextMessage.set (Some msg) in
      return ()
    in
    let does_not_follow =
      internal_error "The input message does not follow the previous one."
    in
    if Raw_level.(equal last_level inbox_level) then
      if Z.(equal message_counter (succ last_counter)) then update
      else does_not_follow
    else if Raw_level.(last_level < inbox_level) then
      if Z.(equal message_counter Z.zero) then update else does_not_follow
    else does_not_follow

  let set_input input = state_of @@ set_input_monadic input

  let next_char =
    let open Monad.Syntax in
    LexerState.(
      let* (start, len) = get in
      set (start, len + 1))

  let no_message_to_lex () =
    internal_error "lexer: There is no input message to lex"

  let current_char =
    let open Monad.Syntax in
    let* (start, len) = LexerState.get in
    let* msg = NextMessage.get in
    match msg with
    | None -> no_message_to_lex ()
    | Some s ->
        if Compare.Int.(start + len < String.length s) then
          return (Some s.[start + len])
        else return None

  let lexeme =
    let open Monad.Syntax in
    let* (start, len) = LexerState.get in
    let* msg = NextMessage.get in
    match msg with
    | None -> no_message_to_lex ()
    | Some s ->
        let* () = LexerState.set (start + len, 0) in
        return (String.sub s start len)

  let push_int_literal =
    let open Monad.Syntax in
    let* s = lexeme in
    match int_of_string_opt s with
    | Some x -> Code.inject (IPush x)
    | None -> (* By validity of int parsing. *) assert false

  let start_parsing : unit t =
    let open Monad.Syntax in
    let* () = Status.set Parsing in
    let* () = ParsingResult.set None in
    let* () = ParserState.set SkipLayout in
    let* () = LexerState.set (0, 0) in
    let* () = Status.set Parsing in
    let* () = Code.clear in
    return ()

  let start_evaluating : unit t =
    let open Monad.Syntax in
    let* () = EvaluationResult.set None in
    let* () = Stack.clear in
    let* () = Status.set Evaluating in
    return ()

  let stop_parsing outcome =
    let open Monad.Syntax in
    let* () = ParsingResult.set (Some outcome) in
    start_evaluating

  let stop_evaluating outcome =
    let open Monad.Syntax in
    let* () = EvaluationResult.set (Some outcome) in
    Status.set WaitingForInputMessage

  let parse : unit t =
    let open Monad.Syntax in
    let produce_add =
      let* _ = lexeme in
      let* () = next_char in
      let* () = Code.inject IAdd in
      return ()
    in
    let produce_int =
      let* () = push_int_literal in
      let* () = ParserState.set SkipLayout in
      return ()
    in
    let is_digit d = Compare.Char.(d >= '0' && d <= '9') in
    let* parser_state = ParserState.get in
    match parser_state with
    | ParseInt -> (
        let* char = current_char in
        match char with
        | Some d when is_digit d -> next_char
        | Some '+' ->
            let* () = produce_int in
            let* () = produce_add in
            return ()
        | Some ' ' ->
            let* () = produce_int in
            let* () = next_char in
            return ()
        | None ->
            let* () = push_int_literal in
            stop_parsing true
        | _ -> stop_parsing false)
    | SkipLayout -> (
        let* char = current_char in
        match char with
        | Some ' ' -> next_char
        | Some '+' -> produce_add
        | Some d when is_digit d ->
            let* _ = lexeme in
            let* () = next_char in
            let* () = ParserState.set ParseInt in
            return ()
        | None -> stop_parsing true
        | _ -> stop_parsing false)

  let evaluate =
    let open Monad.Syntax in
    let* i = Code.pop in
    match i with
    | None -> stop_evaluating true
    | Some (IPush x) -> Stack.push x
    | Some IAdd -> (
        let* v = Stack.pop in
        match v with
        | None -> stop_evaluating false
        | Some x -> (
            let* v = Stack.pop in
            match v with
            | None -> stop_evaluating false
            | Some y -> Stack.push (x + y)))

  let reboot =
    let open Monad.Syntax in
    let* () = Status.set WaitingForInputMessage in
    let* () = Stack.clear in
    let* () = Code.clear in
    return ()

  let eval_step =
    let open Monad.Syntax in
    let* x = is_stuck in
    match x with
    | Some _ -> reboot
    | None -> (
        let* status = Status.get in
        match status with
        | Halted -> boot
        | WaitingForInputMessage -> (
            let* msg = NextMessage.get in
            match msg with
            | None ->
                internal_error
                  "An input state was not provided an input message."
            | Some _ -> start_parsing)
        | Parsing -> parse
        | Evaluating -> evaluate)

  let ticked m =
    let open Monad.Syntax in
    let* tick = CurrentTick.get in
    let* () = CurrentTick.set (Tick.next tick) in
    m

  let eval state = state_of (ticked eval_step) state

  let verify_proof ~input proof =
    let open Lwt_syntax in
    let transition state =
      let* state =
        match input with
        | None -> eval state
        | Some input -> state_of (ticked (set_input_monadic input)) state
      in
      return (state, ())
    in
    let* x = Context.verify_proof proof transition in
    match x with Ok _ -> return_true | Error _ -> return_false
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

  let verify_proof = Context.verify_tree_proof

  let kinded_hash_to_state_hash = function
    | `Value hash | `Node hash ->
        State_hash.hash_bytes [Context_hash.to_bytes hash]

  let proof_start_state proof =
    kinded_hash_to_state_hash proof.Context.Proof.before

  let proof_stop_state proof =
    kinded_hash_to_state_hash proof.Context.Proof.after

  let proof_encoding = Context.Proof_encoding.V2.Tree32.tree_proof_encoding
end)
