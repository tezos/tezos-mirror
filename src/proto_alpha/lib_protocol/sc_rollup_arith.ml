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

open Sc_rollup_repr
module PS = Sc_rollup_PVM_sig

(*
  This is the state hash of reference that both the prover of the node
  and the verifier of the protocol {!Protocol_implementation} have to
  agree on (if they do, it means they are using the same tree
  structure).

  We have to hard-code this value because the Arith PVM uses Irmin as
  its Merkle proof verification backend, and the economic protocol
  cannot create an empty Irmin context. Such a context is required to
  create an empty tree, itself required to create the initial state of
  the Arith PVM.

  Utlimately, the value of this constant is decided by the prover of
  reference (the only need is for it to be compatible with
  {!Protocol_implementation}.)

  Its value is the result of the following snippet

  {|
  let*! state = Prover.initial_state context in
  Prover.state_hash state
  |}
*)
let reference_initial_state_hash =
  State_hash.of_b58check_exn
    "scs11cXwQJJ5dkpEQGq3x2MJm3cM73cbEkHJqo5eDSoRpHUPyEQLB4"

type error +=
  | Arith_proof_production_failed
  | Arith_output_proof_production_failed
  | Arith_invalid_claim_about_outbox

let () =
  let open Data_encoding in
  let msg = "Invalid claim about outbox" in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_arith_invalid_claim_about_outbox"
    ~title:msg
    ~pp:(fun fmt () -> Format.pp_print_string fmt msg)
    ~description:msg
    unit
    (function Arith_invalid_claim_about_outbox -> Some () | _ -> None)
    (fun () -> Arith_invalid_claim_about_outbox) ;
  let msg = "Output proof production failed" in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_arith_output_proof_production_failed"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Arith_output_proof_production_failed -> Some () | _ -> None)
    (fun () -> Arith_output_proof_production_failed) ;
  let msg = "Proof production failed" in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_arith_proof_production_failed"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Arith_proof_production_failed -> Some () | _ -> None)
    (fun () -> Arith_proof_production_failed)

module type P = sig
  module Tree : Context.TREE with type key = string list and type value = bytes

  type tree = Tree.tree

  val hash_tree : tree -> State_hash.t

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
  include PS.S

  val name : string

  val parse_boot_sector : string -> string option

  val pp_boot_sector : Format.formatter -> string -> unit

  val pp : state -> (Format.formatter -> unit -> unit) Lwt.t

  val get_tick : state -> Sc_rollup_tick_repr.t Lwt.t

  type status =
    | Halted
    | Waiting_for_input_message
    | Waiting_for_reveal
    | Parsing
    | Evaluating

  val get_status : state -> status Lwt.t

  val get_outbox : state -> Sc_rollup_PVM_sig.output list Lwt.t

  type instruction =
    | IPush : int -> instruction
    | IAdd : instruction
    | IStore : string -> instruction

  val equal_instruction : instruction -> instruction -> bool

  val pp_instruction : Format.formatter -> instruction -> unit

  val get_parsing_result : state -> bool option Lwt.t

  val get_code : state -> instruction list Lwt.t

  val get_stack : state -> int list Lwt.t

  val get_var : state -> string -> int option Lwt.t

  val get_evaluation_result : state -> bool option Lwt.t

  val get_is_stuck : state -> string option Lwt.t
end

module Make (Context : P) :
  S
    with type context = Context.Tree.t
     and type state = Context.tree
     and type proof = Context.proof = struct
  module Tree = Context.Tree

  type context = Context.Tree.t

  type hash = State_hash.t

  type proof = Context.proof

  let proof_encoding = Context.proof_encoding

  let proof_start_state proof = Context.proof_before proof

  let proof_stop_state proof = Context.proof_after proof

  let name = "arith"

  let parse_boot_sector s = Some s

  let pp_boot_sector fmt s = Format.fprintf fmt "%s" s

  type tree = Tree.tree

  type status =
    | Halted
    | Waiting_for_input_message
    | Waiting_for_reveal
    | Parsing
    | Evaluating

  type instruction =
    | IPush : int -> instruction
    | IAdd : instruction
    | IStore : string -> instruction

  let equal_instruction i1 i2 =
    match (i1, i2) with
    | IPush x, IPush y -> Compare.Int.(x = y)
    | IAdd, IAdd -> true
    | IStore x, IStore y -> Compare.String.(x = y)
    | _, _ -> false

  let pp_instruction fmt = function
    | IPush x -> Format.fprintf fmt "push(%d)" x
    | IAdd -> Format.fprintf fmt "add"
    | IStore x -> Format.fprintf fmt "store(%s)" x

  (*

     The machine state is represented using a Merkle tree.

     Here is the data model of this state represented in the tree:

     - tick : Sc_rollup_tick_repr.t
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

      val children : Tree.key -> 'a Data_encoding.t -> (string * 'a) list t

      val get_value : default:'a -> Tree.key -> 'a Data_encoding.t -> 'a t

      val set_value : Tree.key -> 'a Data_encoding.t -> 'a -> unit t
    end = struct
      type 'a t = state -> (state * 'a option) Lwt.t

      let return x state = Lwt.return (state, Some x)

      let bind m f state =
        let open Lwt_syntax in
        let* state, res = m state in
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

      let decode encoding bytes state =
        let open Lwt_syntax in
        match Data_encoding.Binary.of_bytes_opt encoding bytes with
        | None -> internal_error "Error during decoding" state
        | Some v -> return (state, Some v)

      let find_value key encoding state =
        let open Lwt_syntax in
        let* obytes = Tree.find state key in
        match obytes with
        | None -> return (state, Some None)
        | Some bytes ->
            let* state, value = decode encoding bytes state in
            return (state, Some value)

      let children key encoding state =
        let open Lwt_syntax in
        let* children = Tree.list state key in
        let rec aux = function
          | [] -> return (state, Some [])
          | (key, tree) :: children -> (
              let* obytes = Tree.to_value tree in
              match obytes with
              | None -> internal_error "Invalid children" state
              | Some bytes -> (
                  let* state, v = decode encoding bytes state in
                  match v with
                  | None -> return (state, None)
                  | Some v -> (
                      let* state, l = aux children in
                      match l with
                      | None -> return (state, None)
                      | Some l -> return (state, Some ((key, v) :: l)))))
        in
        aux children

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

    module Make_var (P : sig
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
        let open Monad.Syntax in
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

    module Make_dict (P : sig
      type t

      val name : string

      val pp : Format.formatter -> t -> unit

      val encoding : t Data_encoding.t
    end) =
    struct
      let key k = [P.name; k]

      let get k = find_value (key k) P.encoding

      let set k v = set_value (key k) P.encoding v

      let entries = children [P.name] P.encoding

      let mapped_to k v state =
        let open Lwt_syntax in
        let* state', _ = Monad.(run (set k v) state) in
        let* t = Tree.find_tree state (key k)
        and* t' = Tree.find_tree state' (key k) in
        Lwt.return (Option.equal Tree.equal t t')

      let pp =
        let open Monad.Syntax in
        let* l = entries in
        let pp_elem fmt (key, value) =
          Format.fprintf fmt "@[%s : %a@]" key P.pp value
        in
        return @@ fun fmt () -> Format.pp_print_list pp_elem fmt l
    end

    module Make_deque (P : sig
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

      let top =
        let open Monad.Syntax in
        let* head_idx = get_head in
        let* end_idx = get_end in
        let* v = find_value (idx_key head_idx) P.encoding in
        if Z.(leq end_idx head_idx) then return None
        else
          match v with
          | None -> (* By invariants of the Deque. *) assert false
          | Some x -> return (Some x)

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

    module Current_tick = Make_var (struct
      include Sc_rollup_tick_repr

      let name = "tick"
    end)

    module Vars = Make_dict (struct
      type t = int

      let name = "vars"

      let encoding = Data_encoding.int31

      let pp fmt x = Format.fprintf fmt "%d" x
    end)

    module Stack = Make_deque (struct
      type t = int

      let name = "stack"

      let encoding = Data_encoding.int31
    end)

    module Code = Make_deque (struct
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
              case
                ~title:"store"
                (Tag 2)
                Data_encoding.string
                (function IStore x -> Some x | _ -> None)
                (fun x -> IStore x);
            ])
    end)

    module Boot_sector = Make_var (struct
      type t = string

      let name = "boot_sector"

      let initial = ""

      let encoding = Data_encoding.string

      let pp fmt s = Format.fprintf fmt "%s" s
    end)

    module Status = Make_var (struct
      type t = status

      let initial = Halted

      let encoding =
        Data_encoding.string_enum
          [
            ("Halted", Halted);
            ("Waiting_for_input_message", Waiting_for_input_message);
            ("Waiting_for_reveal", Waiting_for_reveal);
            ("Parsing", Parsing);
            ("Evaluating", Evaluating);
          ]

      let name = "status"

      let string_of_status = function
        | Halted -> "Halted"
        | Waiting_for_input_message -> "Waiting for input message"
        | Waiting_for_reveal -> "Waiting for reveal"
        | Parsing -> "Parsing"
        | Evaluating -> "Evaluating"

      let pp fmt status = Format.fprintf fmt "%s" (string_of_status status)
    end)

    module Required_reveal = Make_var (struct
      type t = PS.Input_hash.t option

      let initial = None

      let encoding = Data_encoding.option PS.Input_hash.encoding

      let name = "required_pre_image_hash"

      let pp fmt v =
        match v with
        | None -> Format.fprintf fmt "<none>"
        | Some h -> PS.Input_hash.pp fmt h
    end)

    module Current_level = Make_var (struct
      type t = Raw_level_repr.t

      let initial = Raw_level_repr.root

      let encoding = Raw_level_repr.encoding

      let name = "current_level"

      let pp = Raw_level_repr.pp
    end)

    module Message_counter = Make_var (struct
      type t = Z.t option

      let initial = None

      let encoding = Data_encoding.option Data_encoding.n

      let name = "message_counter"

      let pp fmt = function
        | None -> Format.fprintf fmt "None"
        | Some c -> Format.fprintf fmt "Some %a" Z.pp_print c
    end)

    module Next_message = Make_var (struct
      type t = string option

      let initial = None

      let encoding = Data_encoding.(option string)

      let name = "next_message"

      let pp fmt = function
        | None -> Format.fprintf fmt "None"
        | Some s -> Format.fprintf fmt "Some %s" s
    end)

    type parser_state = ParseInt | ParseVar | SkipLayout

    module Lexer_state = Make_var (struct
      type t = int * int

      let name = "lexer_buffer"

      let initial = (-1, -1)

      let encoding = Data_encoding.(tup2 int31 int31)

      let pp fmt (start, len) =
        Format.fprintf fmt "lexer.(start = %d, len = %d)" start len
    end)

    module Parser_state = Make_var (struct
      type t = parser_state

      let name = "parser_state"

      let initial = SkipLayout

      let encoding =
        Data_encoding.string_enum
          [
            ("ParseInt", ParseInt);
            ("ParseVar", ParseVar);
            ("SkipLayout", SkipLayout);
          ]

      let pp fmt = function
        | ParseInt -> Format.fprintf fmt "Parsing int"
        | ParseVar -> Format.fprintf fmt "Parsing var"
        | SkipLayout -> Format.fprintf fmt "Skipping layout"
    end)

    module Parsing_result = Make_var (struct
      type t = bool option

      let name = "parsing_result"

      let initial = None

      let encoding = Data_encoding.(option bool)

      let pp fmt = function
        | None -> Format.fprintf fmt "n/a"
        | Some true -> Format.fprintf fmt "parsing succeeds"
        | Some false -> Format.fprintf fmt "parsing fails"
    end)

    module Evaluation_result = Make_var (struct
      type t = bool option

      let name = "evaluation_result"

      let initial = None

      let encoding = Data_encoding.(option bool)

      let pp fmt = function
        | None -> Format.fprintf fmt "n/a"
        | Some true -> Format.fprintf fmt "evaluation succeeds"
        | Some false -> Format.fprintf fmt "evaluation fails"
    end)

    module Output_counter = Make_var (struct
      type t = Z.t

      let initial = Z.zero

      let name = "output_counter"

      let encoding = Data_encoding.n

      let pp = Z.pp_print
    end)

    module Output = Make_dict (struct
      type t = Sc_rollup_PVM_sig.output

      let name = "output"

      let encoding = Sc_rollup_PVM_sig.output_encoding

      let pp = Sc_rollup_PVM_sig.pp_output
    end)

    let pp =
      let open Monad.Syntax in
      let* status_pp = Status.pp in
      let* message_counter_pp = Message_counter.pp in
      let* next_message_pp = Next_message.pp in
      let* parsing_result_pp = Parsing_result.pp in
      let* parser_state_pp = Parser_state.pp in
      let* lexer_state_pp = Lexer_state.pp in
      let* evaluation_result_pp = Evaluation_result.pp in
      let* vars_pp = Vars.pp in
      let* output_pp = Output.pp in
      let* stack = Stack.to_list in
      let* current_tick_pp = Current_tick.pp in
      return @@ fun fmt () ->
      Format.fprintf
        fmt
        "@[<v 0 >@;\
         %a@;\
         %a@;\
         %a@;\
         %a@;\
         %a@;\
         %a@;\
         %a@;\
         tick : %a@;\
         vars : %a@;\
         output :%a@;\
         stack : %a@;\
         @]"
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
        current_tick_pp
        ()
        vars_pp
        ()
        output_pp
        ()
        Format.(pp_print_list pp_print_int)
        stack
  end

  open State

  type state = State.state

  open Monad

  let initial_state ctxt =
    let state = Tree.empty ctxt in
    let m =
      let open Monad.Syntax in
      let* () = Status.set Halted in
      return ()
    in
    let open Lwt_syntax in
    let* state, _ = run m state in
    return state

  let install_boot_sector state boot_sector =
    let m =
      let open Monad.Syntax in
      let* () = Boot_sector.set boot_sector in
      return ()
    in
    let open Lwt_syntax in
    let* state, _ = run m state in
    return state

  let state_hash state =
    let context_hash = Tree.hash state in
    Lwt.return @@ State_hash.context_hash_to_state_hash context_hash

  let pp state =
    let open Lwt_syntax in
    let* _, pp = Monad.run pp state in
    match pp with
    | None -> return @@ fun fmt _ -> Format.fprintf fmt "<opaque>"
    | Some pp ->
        let* state_hash = state_hash state in
        return (fun fmt () ->
            Format.fprintf fmt "@[%a: %a@]" State_hash.pp state_hash pp ())

  let boot =
    let open Monad.Syntax in
    let* () = Status.create in
    let* () = Next_message.create in
    let* () = Status.set Waiting_for_input_message in
    return ()

  let result_of ~default m state =
    let open Lwt_syntax in
    let* _, v = run m state in
    match v with None -> return default | Some v -> return v

  let state_of m state =
    let open Lwt_syntax in
    let* s, _ = run m state in
    return s

  let get_tick = result_of ~default:Sc_rollup_tick_repr.initial Current_tick.get

  let is_input_state_monadic =
    let open Monad.Syntax in
    let* status = Status.get in
    match status with
    | Waiting_for_input_message -> (
        let* level = Current_level.get in
        let* counter = Message_counter.get in
        match counter with
        | Some n -> return (PS.First_after (level, n))
        | None -> return PS.Initial)
    | Waiting_for_reveal -> (
        let* h = Required_reveal.get in
        match h with
        | None -> internal_error "Internal error: Reveal invariant broken"
        | Some h -> return (PS.Needs_reveal (Reveal_raw_data h)))
    | _ -> return PS.No_input_required

  let is_input_state =
    result_of ~default:PS.No_input_required @@ is_input_state_monadic

  let get_status = result_of ~default:Waiting_for_input_message @@ Status.get

  let get_outbox state =
    let open Lwt_syntax in
    let+ entries = result_of ~default:[] Output.entries state in
    List.map snd entries

  let get_code = result_of ~default:[] @@ Code.to_list

  let get_parsing_result = result_of ~default:None @@ Parsing_result.get

  let get_stack = result_of ~default:[] @@ Stack.to_list

  let get_var state k = (result_of ~default:None @@ Vars.get k) state

  let get_evaluation_result = result_of ~default:None @@ Evaluation_result.get

  let get_is_stuck = result_of ~default:None @@ is_stuck

  let start_parsing : unit t =
    let open Monad.Syntax in
    let* () = Status.set Parsing in
    let* () = Parsing_result.set None in
    let* () = Parser_state.set SkipLayout in
    let* () = Lexer_state.set (0, 0) in
    let* () = Code.clear in
    return ()

  let set_inbox_message_monadic {PS.inbox_level; message_counter; payload} =
    let open Monad.Syntax in
    let payload =
      match Sc_rollup_inbox_message_repr.deserialize payload with
      | Error _ -> None
      | Ok (External payload) -> Some payload
      | Ok (Internal {payload; _}) -> (
          match Micheline.root payload with
          | String (_, payload) -> Some payload
          | _ -> None)
    in
    match payload with
    | Some payload ->
        let* boot_sector = Boot_sector.get in
        let msg = boot_sector ^ payload in
        let* () = Current_level.set inbox_level in
        let* () = Message_counter.set (Some message_counter) in
        let* () = Next_message.set (Some msg) in
        let* () = start_parsing in
        return ()
    | None ->
        let* () = Current_level.set inbox_level in
        let* () = Message_counter.set (Some message_counter) in
        let* () = Status.set Waiting_for_input_message in
        return ()

  let reveal_monadic (PS.Raw_data data) =
    (*

       The inbox cursor is unchanged as the message comes from the
       outer world.

       We don't have to check that the data hash is the one we
       expected as we decided to trust the initial witness.

       It is the responsibility of the rollup node to check it if it
       does not want to publish a wrong commitment.

       Notice that a multi-page transmission is possible by embedding
       a continuation encoded as an optional hash in [data].

    *)
    let open Monad.Syntax in
    let* () = Next_message.set (Some data) in
    let* () = start_parsing in
    return ()

  let ticked m =
    let open Monad.Syntax in
    let* tick = Current_tick.get in
    let* () = Current_tick.set (Sc_rollup_tick_repr.next tick) in
    m

  let set_input_monadic input =
    match input with
    | PS.Inbox_message m -> set_inbox_message_monadic m
    | PS.Reveal s -> reveal_monadic s

  let set_input input = set_input_monadic input |> ticked |> state_of

  let next_char =
    let open Monad.Syntax in
    Lexer_state.(
      let* start, len = get in
      set (start, len + 1))

  let no_message_to_lex () =
    internal_error "lexer: There is no input message to lex"

  let current_char =
    let open Monad.Syntax in
    let* start, len = Lexer_state.get in
    let* msg = Next_message.get in
    match msg with
    | None -> no_message_to_lex ()
    | Some s ->
        if Compare.Int.(start + len < String.length s) then
          return (Some s.[start + len])
        else return None

  let lexeme =
    let open Monad.Syntax in
    let* start, len = Lexer_state.get in
    let* msg = Next_message.get in
    match msg with
    | None -> no_message_to_lex ()
    | Some s ->
        let* () = Lexer_state.set (start + len, 0) in
        return (String.sub s start len)

  let push_int_literal =
    let open Monad.Syntax in
    let* s = lexeme in
    match int_of_string_opt s with
    | Some x -> Code.inject (IPush x)
    | None -> (* By validity of int parsing. *) assert false

  let push_var =
    let open Monad.Syntax in
    let* s = lexeme in
    Code.inject (IStore s)

  let start_evaluating : unit t =
    let open Monad.Syntax in
    let* () = Status.set Evaluating in
    let* () = Evaluation_result.set None in
    return ()

  let stop_parsing outcome =
    let open Monad.Syntax in
    let* () = Parsing_result.set (Some outcome) in
    start_evaluating

  let stop_evaluating outcome =
    let open Monad.Syntax in
    let* () = Evaluation_result.set (Some outcome) in
    Status.set Waiting_for_input_message

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
      let* () = Parser_state.set SkipLayout in
      return ()
    in
    let produce_var =
      let* () = push_var in
      let* () = Parser_state.set SkipLayout in
      return ()
    in
    let is_digit d = Compare.Char.(d >= '0' && d <= '9') in
    let is_letter d =
      Compare.Char.((d >= 'a' && d <= 'z') || (d >= 'A' && d <= 'Z'))
    in
    let is_identifier_char d =
      is_letter d || is_digit d
      || Compare.Char.(d = ':')
      || Compare.Char.(d = '%')
    in
    let* parser_state = Parser_state.get in
    match parser_state with
    | ParseInt -> (
        let* char = current_char in
        match char with
        | Some d when is_digit d -> next_char
        | Some '+' ->
            let* () = produce_int in
            let* () = produce_add in
            return ()
        | Some (' ' | '\n') ->
            let* () = produce_int in
            let* () = next_char in
            return ()
        | None ->
            let* () = push_int_literal in
            stop_parsing true
        | _ -> stop_parsing false)
    | ParseVar -> (
        let* char = current_char in
        match char with
        | Some d when is_identifier_char d -> next_char
        | Some '+' ->
            let* () = produce_var in
            let* () = produce_add in
            return ()
        | Some (' ' | '\n') ->
            let* () = produce_var in
            let* () = next_char in
            return ()
        | None ->
            let* () = push_var in
            stop_parsing true
        | _ -> stop_parsing false)
    | SkipLayout -> (
        let* char = current_char in
        match char with
        | Some (' ' | '\n') -> next_char
        | Some '+' -> produce_add
        | Some d when is_digit d ->
            let* _ = lexeme in
            let* () = next_char in
            let* () = Parser_state.set ParseInt in
            return ()
        | Some d when is_letter d ->
            let* _ = lexeme in
            let* () = next_char in
            let* () = Parser_state.set ParseVar in
            return ()
        | None -> stop_parsing true
        | _ -> stop_parsing false)

  let output (destination, entrypoint) v =
    let open Monad.Syntax in
    let open Sc_rollup_outbox_message_repr in
    let* counter = Output_counter.get in
    let* () = Output_counter.set (Z.succ counter) in
    let unparsed_parameters =
      Micheline.(Int ((), Z.of_int v) |> strip_locations)
    in
    let transaction = {unparsed_parameters; destination; entrypoint} in
    let message = Atomic_transaction_batch {transactions = [transaction]} in
    let* outbox_level = Current_level.get in
    let output =
      Sc_rollup_PVM_sig.{outbox_level; message_index = counter; message}
    in
    Output.set (Z.to_string counter) output

  let identifies_target_contract x =
    let open Option_syntax in
    match String.split_on_char '%' x with
    | destination :: entrypoint -> (
        match Contract_hash.of_b58check_opt destination with
        | None ->
            if Compare.String.(x = "out") then
              return (Contract_hash.zero, Entrypoint_repr.default)
            else fail
        | Some destination ->
            let* entrypoint =
              match entrypoint with
              | [] -> return Entrypoint_repr.default
              | _ ->
                  let* entrypoint =
                    Non_empty_string.of_string (String.concat "" entrypoint)
                  in
                  let* entrypoint =
                    Entrypoint_repr.of_annot_lax_opt entrypoint
                  in
                  return entrypoint
            in
            return (destination, entrypoint))
    | [] -> fail

  let evaluate =
    let open Monad.Syntax in
    let* i = Code.pop in
    match i with
    | None -> stop_evaluating true
    | Some (IPush x) -> Stack.push x
    | Some (IStore x) -> (
        let len = String.length x in
        if Compare.Int.(len > 5) && Compare.String.(String.sub x 0 5 = "hash:")
        then
          let hash = String.sub x 5 (len - 5) in
          match PS.Input_hash.of_b58check_opt hash with
          | None -> stop_evaluating false
          | Some hash ->
              let* () = Required_reveal.set (Some hash) in
              let* () = Status.set Waiting_for_reveal in
              return ()
        else
          let* v = Stack.top in
          match v with
          | None -> stop_evaluating false
          | Some v -> (
              match identifies_target_contract x with
              | Some contract_entrypoint -> output contract_entrypoint v
              | None -> Vars.set x v))
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
    let* () = Status.set Waiting_for_input_message in
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
        | Waiting_for_input_message | Waiting_for_reveal -> (
            let* msg = Next_message.get in
            match msg with
            | None -> internal_error "An input state was not provided an input."
            | Some _ -> start_parsing)
        | Parsing -> parse
        | Evaluating -> evaluate)

  let eval state = state_of (ticked eval_step) state

  let step_transition input_given state =
    let open Lwt_syntax in
    let* request = is_input_state state in
    let* state =
      match request with
      | PS.No_input_required -> eval state
      | PS.Initial | PS.First_after _ -> (
          match input_given with
          | Some (PS.Inbox_message _ as input_given) ->
              set_input input_given state
          | None | Some (PS.Reveal _) ->
              state_of
                (internal_error
                   "Invalid set_input: expecting inbox message, got a reveal.")
                state)
      | PS.Needs_reveal _hash -> (
          match input_given with
          | Some (PS.Reveal _ as input_given) -> set_input input_given state
          | None | Some (PS.Inbox_message _) ->
              state_of
                (internal_error
                   "Invalid set_input: expecting a reveal, got an inbox \
                    message.")
                state)
    in
    return (state, request)

  type error += Arith_proof_verification_failed

  let verify_proof input_given proof =
    let open Lwt_tzresult_syntax in
    let*! result = Context.verify_proof proof (step_transition input_given) in
    match result with
    | None -> fail Arith_proof_verification_failed
    | Some (_state, request) -> return request

  let produce_proof context input_given state =
    let open Lwt_tzresult_syntax in
    let*! result =
      Context.produce_proof context state (step_transition input_given)
    in
    match result with
    | Some (tree_proof, _requested) -> return tree_proof
    | None -> fail Arith_proof_production_failed

  let verify_origination_proof proof boot_sector =
    let open Lwt_syntax in
    let before = Context.proof_before proof in
    if State_hash.(before <> reference_initial_state_hash) then return false
    else
      let* result =
        Context.verify_proof proof (fun state ->
            let* state = install_boot_sector state boot_sector in
            return (state, ()))
      in
      match result with None -> return false | Some (_, ()) -> return true

  let produce_origination_proof context boot_sector =
    let open Lwt_tzresult_syntax in
    let*! state = initial_state context in
    let*! result =
      Context.produce_proof context state (fun state ->
          let open Lwt_syntax in
          let* state = install_boot_sector state boot_sector in
          return (state, ()))
    in
    match result with
    | Some (proof, ()) -> return proof
    | None -> fail Arith_proof_production_failed

  (* TEMPORARY: The following definitions will be extended in a future commit. *)

  type output_proof = {
    output_proof : Context.proof;
    output_proof_state : hash;
    output_proof_output : PS.output;
  }

  let output_proof_encoding =
    let open Data_encoding in
    conv
      (fun {output_proof; output_proof_state; output_proof_output} ->
        (output_proof, output_proof_state, output_proof_output))
      (fun (output_proof, output_proof_state, output_proof_output) ->
        {output_proof; output_proof_state; output_proof_output})
      (obj3
         (req "output_proof" Context.proof_encoding)
         (req "output_proof_state" State_hash.encoding)
         (req "output_proof_output" PS.output_encoding))

  let output_of_output_proof s = s.output_proof_output

  let state_of_output_proof s = s.output_proof_state

  let output_key (output : PS.output) = Z.to_string output.message_index

  let has_output output tree =
    let open Lwt_syntax in
    let* equal = Output.mapped_to (output_key output) output tree in
    return (tree, equal)

  let verify_output_proof p =
    let open Lwt_syntax in
    let transition = has_output p.output_proof_output in
    let* result = Context.verify_proof p.output_proof transition in
    match result with None -> return false | Some _ -> return true

  let produce_output_proof context state output_proof_output =
    let open Lwt_result_syntax in
    let*! output_proof_state = state_hash state in
    let*! result =
      Context.produce_proof context state @@ has_output output_proof_output
    in
    match result with
    | Some (output_proof, true) ->
        return {output_proof; output_proof_state; output_proof_output}
    | Some (_, false) -> fail Arith_invalid_claim_about_outbox
    | None -> fail Arith_output_proof_production_failed

  module Internal_for_tests = struct
    let insert_failure state =
      let add n = Tree.add state ["failures"; string_of_int n] Bytes.empty in
      let open Lwt_syntax in
      let* n = Tree.length state ["failures"] in
      add n
  end
end

module Protocol_implementation = Make (struct
  module Tree = struct
    include Context.Tree

    type tree = Context.tree

    type t = Context.t

    type key = string list

    type value = bytes
  end

  type tree = Context.tree

  let hash_tree t = State_hash.context_hash_to_state_hash (Tree.hash t)

  type proof = Context.Proof.tree Context.Proof.t

  let verify_proof p f =
    Lwt.map Result.to_option (Context.verify_tree_proof p f)

  let produce_proof _context _state _f =
    (* Can't produce proof without full context*)
    Lwt.return None

  let kinded_hash_to_state_hash = function
    | `Value hash | `Node hash -> State_hash.context_hash_to_state_hash hash

  let proof_before proof = kinded_hash_to_state_hash proof.Context.Proof.before

  let proof_after proof = kinded_hash_to_state_hash proof.Context.Proof.after

  let proof_encoding = Context.Proof_encoding.V2.Tree32.tree_proof_encoding
end)
