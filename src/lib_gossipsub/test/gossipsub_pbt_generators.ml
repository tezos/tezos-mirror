(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_gossipsub
open Gossipsub_intf
open Test_gossipsub_shared
module M = QCheck2.Gen

(* We need monadic sequences to represent {!Fragments}. *)
module SeqM = Seqes.Monadic.Make1 (M)

(* In the signature of {!GS} below, the [Time.t = int] constraint is required
   to be able to pretty-print the time obtained from
   [Test_gossipsub_shared.Time.now]. To clean this, one could expose
   [now] in [GS.Time] but it's not much cleaner. *)

(** [Make] instantiates a generator for gossipsub transitions. *)
module Make
    (GS :
      AUTOMATON
        with type Time.t = Milliseconds.t
         and type Span.t = Milliseconds.Span.t) =
struct
  open M

  (** We re-export {!GS} modules and types for convenience. *)

  module Peer = GS.Peer
  module Topic = GS.Topic
  module Message_id = GS.Message_id

  type _ input =
    | Add_peer : GS.add_peer -> [`Add_peer] input (* case 0 *)
    | Remove_peer : GS.remove_peer -> [`Remove_peer] input (* case 1 *)
    | Ihave : GS.ihave -> [`IHave] input (* case 2 *)
    | Iwant : GS.iwant -> [`IWant] input (* case 3 *)
    | Graft : GS.graft -> [`Graft] input (* case 4 *)
    | Prune : GS.prune -> [`Prune] input (* case 5 *)
    | Publish_message :
        GS.publish_message
        -> [`Publish_message] input (* case 6 *)
    | Receive_message :
        GS.receive_message
        -> [`Receive_message] input (* case 7 *)
    | Heartbeat : [`Heartbeat] input (* case 8 *)
    | Join : GS.join -> [`Join] input (* case 9 *)
    | Leave : GS.leave -> [`Leave] input (* case 10 *)
    | Subscribe : GS.subscribe -> [`Subscribe] input (* case 11 *)
    | Unsubscribe : GS.unsubscribe -> [`Unsubscribe] input (* case 12 *)
    | Set_application_score :
        GS.set_application_score
        -> [`Set_application_score] input (* case 13 *)

  type ex_input = I : _ input -> ex_input

  type output = O : _ GS.output -> output

  type event = Input : 'a input -> event | Elapse of GS.Span.t

  type transition =
    | Transition : {
        time : GS.Time.t;
        input : 'a input;
        state : GS.state;
        state' : GS.state;
        output : 'a GS.output;
      }
        -> transition

  type trace = transition list

  let pp_input fmtr (type a) (i : a input) =
    let open Format in
    match i with
    | Add_peer add_peer -> fprintf fmtr "Add_peer %a" GS.pp_add_peer add_peer
    | Remove_peer remove_peer ->
        fprintf fmtr "Remove_peer %a" GS.pp_remove_peer remove_peer
    | Ihave handle_ihave -> fprintf fmtr "Ihave %a" GS.pp_ihave handle_ihave
    | Iwant handle_iwant -> fprintf fmtr "Iwant %a" GS.pp_iwant handle_iwant
    | Graft handle_graft -> fprintf fmtr "Graft %a" GS.pp_graft handle_graft
    | Prune handle_prune -> fprintf fmtr "Prune %a" GS.pp_prune handle_prune
    | Receive_message handle_receive_message ->
        fprintf
          fmtr
          "Receive_message %a"
          GS.pp_receive_message
          handle_receive_message
    | Publish_message publish_message ->
        fprintf fmtr "Publish %a" GS.pp_publish_message publish_message
    | Heartbeat -> fprintf fmtr "Heartbeat"
    | Join join -> fprintf fmtr "Join %a" GS.pp_join join
    | Leave leave -> fprintf fmtr "Leave %a" GS.pp_leave leave
    | Subscribe subscribe ->
        fprintf fmtr "Subscribe %a" GS.pp_subscribe subscribe
    | Unsubscribe unsubscribe ->
        fprintf fmtr "Unsubscribe %a" GS.pp_unsubscribe unsubscribe
    | Set_application_score set_application_score ->
        fprintf
          fmtr
          "Set_application_score %a"
          GS.pp_set_application_score
          set_application_score

  let pp_output fmtr (O o) = GS.pp_output fmtr o

  let pp_trace ?pp_state ?pp_output () fmtr trace =
    let open Format in
    let pp fmtr (Transition {time; input; state; state' = _; output}) =
      Option.iter (fun pp -> fprintf fmtr "%a@," pp state) pp_state ;
      fprintf fmtr "[%a] => " GS.Time.pp time ;
      pp_input fmtr input ;
      Option.iter (fun pp -> fprintf fmtr " / %a" pp (O output)) pp_output
    in
    let pp_last_state fmtr trace =
      match List.hd (List.rev trace) with
      | None -> ()
      | Some (Transition {state'; _}) ->
          Option.iter (fun pp -> fprintf fmtr "%a@," pp state') pp_state
    in
    fprintf
      fmtr
      "%a@;%a"
      (pp_print_list ~pp_sep:(fun fmtr () -> fprintf fmtr "@,") pp)
      trace
      pp_last_state
      trace

  let add_peer ~gen_peer ~gen_direct ~gen_outbound =
    let+ direct = gen_direct
    and+ outbound = gen_outbound
    and+ peer = gen_peer in
    ({direct; outbound; peer; bootstrap = false} : GS.add_peer)

  let remove_peer ~gen_peer =
    let+ peer = gen_peer in
    ({peer} : GS.remove_peer)

  let ihave ~gen_peer ~gen_topic ~gen_message_id ~gen_msg_count =
    let* msg_count = gen_msg_count in
    let+ peer = gen_peer
    and+ topic = gen_topic
    and+ message_ids = list_repeat msg_count gen_message_id in
    ({peer; topic; message_ids} : GS.ihave)

  let iwant ~gen_peer ~gen_message_id ~gen_msg_count =
    let* msg_count = gen_msg_count in
    let+ peer = gen_peer
    and+ message_ids = list_repeat msg_count gen_message_id in
    ({peer; message_ids} : GS.iwant)

  let graft ~gen_peer ~gen_topic =
    let+ peer = gen_peer and+ topic = gen_topic in
    ({peer; topic} : GS.graft)

  let prune ~gen_peer ~gen_topic ~gen_backoff ~gen_px_count =
    let* px_count = gen_px_count in
    let+ peer = gen_peer
    and+ topic = gen_topic
    and+ px =
      let+ l = list_repeat px_count gen_peer in
      List.to_seq l
    and+ backoff = gen_backoff in
    ({peer; topic; px; backoff} : GS.prune)

  let receive_message ~gen_peer ~gen_topic ~gen_message_id ~gen_message =
    let+ sender = gen_peer
    and+ topic = gen_topic
    and+ message_id = gen_message_id
    and+ message = gen_message in
    ({sender; topic; message_id; message} : GS.receive_message)

  let publish_message ~gen_topic ~gen_message_id ~gen_message =
    let+ topic = gen_topic
    and+ message_id = gen_message_id
    and+ message = gen_message in
    ({topic; message_id; message} : GS.publish_message)

  let join ~gen_topic =
    let+ topic = gen_topic in
    ({topic} : GS.join)

  let leave ~gen_topic =
    let+ topic = gen_topic in
    ({topic} : GS.leave)

  let subscribe ~gen_topic ~gen_peer =
    let+ topic = gen_topic and+ peer = gen_peer in
    ({topic; peer} : GS.subscribe)

  let unsubscribe ~gen_topic ~gen_peer =
    let+ topic = gen_topic and+ peer = gen_peer in
    ({topic; peer} : GS.unsubscribe)

  let set_application_score ~gen_peer ~gen_score =
    let+ peer = gen_peer and+ score = gen_score in
    ({peer; score} : GS.set_application_score)

  let input (I i) = Input i

  module I = struct
    let i input = I input

    let add_peer x = Add_peer x |> i

    let remove_peer x = Remove_peer x |> i

    let ihave x = Ihave x |> i

    let iwant x = Iwant x |> i

    let graft x = Graft x |> i

    let prune x = Prune x |> i

    let receive_message x = Receive_message x |> i

    let publish_message x = Publish_message x |> i

    let join x = Join x |> i

    let leave x = Leave x |> i

    let subscribe x = Subscribe x |> i

    let unsubscribe x = Unsubscribe x |> i

    let heartbeat = i Heartbeat

    let set_application_score x = Set_application_score x |> i
  end

  let dispatch : type a.
      a input ->
      ?batching_configuration:GS.message_handling ->
      GS.state ->
      GS.state * a GS.output =
   fun i ?(batching_configuration = Sequentially) state ->
    match i with
    | Add_peer m -> GS.add_peer m state
    | Remove_peer m -> GS.remove_peer m state
    | Ihave m -> GS.handle_ihave m state
    | Iwant m -> GS.handle_iwant m state
    | Graft m -> GS.handle_graft m state
    | Prune m -> GS.handle_prune m state
    | Receive_message m ->
        GS.handle_receive_message ~batching_configuration m state
    | Publish_message m -> GS.publish_message m state
    | Heartbeat -> GS.heartbeat state
    | Join m -> GS.join m state
    | Leave m -> GS.leave m state
    | Subscribe m -> GS.handle_subscribe m state
    | Unsubscribe m -> GS.handle_unsubscribe m state
    | Set_application_score m -> GS.set_application_score m state

  (** A fragment is a sequence of events encoding a basic interaction with
      the gossipsub automaton. Fragments support sequential and parallel
      composition. *)
  module Fragment = struct
    type raw = Thread of event Seq.t | Par of raw list | Seq of raw list

    type t = raw M.t

    let raw_of_list l = Thread (List.to_seq l |> Seq.map input)

    let of_list (l : ex_input list) = raw_of_list l |> M.return

    (* Smart [raw] constructors *)

    let empty_raw = Thread Seq.empty

    let seq rs =
      match rs with
      | [] -> empty_raw
      | [raw] -> raw
      | _ ->
          let flattened =
            List.fold_right
              (fun raw acc ->
                match raw with Seq rs -> rs @ acc | _ -> raw :: acc)
              rs
              []
          in
          Seq flattened

    let par rs =
      match rs with
      | [] -> empty_raw
      | [raw] -> raw
      | _ ->
          let flattened =
            List.fold_right
              (fun raw acc ->
                match raw with Par rs -> rs @ acc | _ -> raw :: acc)
              rs
              []
          in
          Par flattened

    (* Sample the next event from a [raw]. *)
    let rec next :
        raw ->
        ((event * raw) option -> (event * raw) option M.t) ->
        (event * raw) option M.t =
     fun raw k ->
      let open M in
      match raw with
      | Thread seq -> (
          match Seq.uncons seq with
          | None -> k None
          | Some (hd, tail) -> k (Some (hd, Thread tail)))
      | Seq [] -> k None
      | Seq (hd :: rest) ->
          next hd (function
            | Some (event, hd') -> k (Some (event, seq (hd' :: rest)))
            | None -> next (seq rest) k)
      | Par [] -> k None
      | Par parallel_components -> (
          let length = List.length parallel_components in
          let* index = int_bound (length - 1) in
          let rev_prefix, tail = List.rev_split_n index parallel_components in
          match tail with
          | [] -> assert false
          | fragment :: rest ->
              next fragment (function
                | None -> next (par (List.rev_append rev_prefix rest)) k
                | Some (elt, fragment') ->
                    k
                      (Some
                         ( elt,
                           par (List.rev_append rev_prefix (fragment' :: rest))
                         ))))

    let next : raw -> (event * raw) option M.t =
     fun fragment -> next fragment M.return

    (* Combinators *)

    let bind_gen m f : t =
      let* m in
      f m

    let of_input_gen gen f : t =
      let+ x = gen in
      raw_of_list (f x)

    let tick : t =
      Thread ([Elapse (Milliseconds.Span.of_int_s 1)] |> List.to_seq)
      |> M.return

    let repeat : int -> t -> t =
     fun n fragment ->
      let+ rs = M.list_repeat n fragment in
      seq rs

    let repeat_at_most : int -> t -> t =
     fun n fragment ->
      let* n = M.int_bound n in
      repeat n fragment

    let ( @% ) : t -> t -> t =
     fun x y ->
      let+ x and+ y in
      seq [x; y]

    let interleave : t list -> t =
     fun fs ->
      let+ rs = M.flatten_l fs in
      par rs

    let fork : int -> t -> t =
     fun n fragment ->
      let frags = List.repeat n fragment in
      interleave frags

    let fork_at_most n fragment =
      let* n = M.int_bound n in
      fork n fragment

    (* Shrinking *)

    (*
       fold_zip (fun rev_prefix elt tail acc -> (List.rev rev_prefix, elt, tail) :: acc) [1;2;3] [];;
       - : (int list * int * int list) list =  [([1; 2], 3, []); ([1], 2, [3]); ([], 1, [2; 3])]
     *)
    let fold_zip :
        ('a list -> 'a -> 'a list -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc =
     fun f l acc ->
      let rec loop l rev_prefix acc =
        match l with
        | [] -> acc
        | hd :: tl -> loop tl (hd :: rev_prefix) (f rev_prefix hd tl acc)
      in
      loop l [] acc

    (*
       fold_over_sublists (fun l acc -> l :: acc) [1;2;3] [];;
       - : int list list = [[1; 2]; [1; 3]; [2; 3]]
     *)
    let fold_over_sublists :
        ('a list -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc =
     fun f l acc ->
      fold_zip
        (fun rev_prefix _elt tail -> f (List.rev_append rev_prefix tail))
        l
        acc

    (* We only shrink leaf [Par] nodes, i.e. [Par] nodes that do not
       contain [Par] nodes as subterms *)

    let rec par_free : raw -> bool =
     fun raw ->
      match raw with
      | Thread _ -> true
      | Par _ -> false
      | Seq rs -> List.for_all par_free rs

    let rec shrink_raw : raw -> raw Seq.t =
     fun raw ->
      match raw with
      | Thread _ ->
          (* Can't shrink a thread *)
          Seq.return raw
      | Par components when List.for_all par_free components ->
          (* We try to shrink leaf-level [Par] by dropping one component *)
          fold_over_sublists
            (fun subcomponents acc -> Seq.cons (par subcomponents) acc)
            components
            Seq.empty
      | Par components ->
          (* If the [Par] is not leaf-level, we try to shrink each component
             one after the other. *)
          fold_zip
            (fun rev_prefix elt tail acc ->
              Seq.flat_map
                (fun shrunk_elt ->
                  Seq.cons
                    (par (List.rev_append rev_prefix (shrunk_elt :: tail)))
                    acc)
                (shrink_raw elt))
            components
            Seq.empty
      | Seq components ->
          (* For [Seq] nodes, we shrink all components in parallel. *)
          List.to_seq components |> Seq.map shrink_raw |> Seq.transpose
          |> Seq.flat_map (fun components ->
                 Seq.return (seq (List.of_seq components)))

    (* Construction of the trace generator. *)

    (* Evaluate a [raw] on an initial state yields a trace. *)
    let raw_to_trace ?batching_configuration state raw =
      let open M in
      let seq = SeqM.M.unfold next raw in
      let* _, _, rev_trace =
        SeqM.fold_left
          (fun (time, state, acc) event ->
            match event with
            | Input i ->
                Time.set time ;
                let state', output = dispatch ?batching_configuration i state in
                let step =
                  Transition {time; input = i; state; state'; output}
                in
                (time, state', step :: acc)
            | Elapse d -> (Milliseconds.add time d, state, acc))
          (Milliseconds.zero, state, [])
          seq
      in
      return (List.rev rev_trace)

    (* By default, shrinking is deactivated as it may be very costly, or even
       diverge for unknown reasons. We leave the option to reactivate it should
       you need it. *)
    let shrinking_deactivated = true

    let raw_generator (fragment : t) : raw M.t =
      if shrinking_deactivated then fragment
      else M.set_shrink shrink_raw fragment
  end

  let fold fragment f init =
    let open M in
    let* raw = Fragment.raw_generator fragment in
    let seq = SeqM.M.unfold Fragment.next raw in
    SeqM.fold_left (fun acc event -> f event acc) init seq

  let run ?batching_configuration state fragment : trace t =
    let open M in
    let* raw = Fragment.raw_generator fragment in
    Fragment.raw_to_trace ?batching_configuration state raw

  let check_fold (type e inv) (f : transition -> inv -> (inv, e) result) init
      trace : (unit, e * trace) result =
    let exception Predicate_failed of e * trace in
    try
      let _, _ =
        List.fold_left
          (fun (invariant, rev_trace) step ->
            let rev_trace = step :: rev_trace in
            match f step invariant with
            | Ok invariant -> (invariant, rev_trace)
            | Error e -> raise (Predicate_failed (e, List.rev rev_trace)))
          (init, [])
          trace
      in
      Ok ()
    with Predicate_failed (e, prefix) -> Error (e, prefix)

  let check_final f (trace : trace) =
    match List.rev trace with [] -> Ok () | t :: _ -> f t
end

include Make (GS)
