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

(* In order to keep this algorithm reasonably efficient even for large
   expressions, it is written in a tail-recursive manner. This means that
   no node can descend into a recursive call directly to diff its children.
   Instead a queue of nodes to be processed is maintained and any children
   a node might have are pushed into this queue to be processed later.
   [dequeue] and [diff_step] functions are responsible for this.

   This means that the algorithm also needs to maintain a state in order to
   know how to build the resulting expression. This state has the form of
   a stack (see [state] and [state_stack] types). Each item on the stack
   represents a [Seq] or a [Prim] node currently being built. Multiple levels
   that might exist on the stack represent nesting of those nodes.

   Whenever a [Seq] or a [Prim] node is discovered in both versions of the
   expression, a corresponding item is added to the stack and all the children
   of both nodes are paired in order and pushed into the queue (see [diff_item]
   type and [zip_nodes] function). The queue is organised in levels (represented
   as a list of lists). When a level is finished, one level of the stack is
   folded into a complete expression, which is then added to the accumulator a
   level up, from where it'll be included in the encompassing expression when
   that is finished (see [fold_stack_level] function).

   Eventually there will be no more nodes to dequeue, at which point all the
   remaining stack levels are folded and the final result is produced. *)

open Micheline

let repr_node = function
  | Int (_, i) -> Format.asprintf "%a" Z.pp_print i
  | String (_, s) -> Format.sprintf "\"%s\"" s
  | Bytes (_, b) -> Format.sprintf "0x%s" (Bytes.to_string b)
  | Prim (_, prim, _, _) -> prim
  | Seq (_, _) -> "{...}"

let no_comment : Micheline_printer.location = {comment = None}

let added = Micheline_printer.{comment = Some "+"}

let removed = Micheline_printer.{comment = Some "-"}

let replaced repl = Micheline_printer.{comment = Some ("-> " ^ repr_node repl)}

let rec replace_location :
    'a.
    Micheline_printer.location -> ('a, string) node -> Micheline_printer.node =
 fun loc -> function
  | Int (_, i) -> Int (loc, i)
  | String (_, s) -> String (loc, s)
  | Bytes (_, b) -> Bytes (loc, b)
  | Prim (_, p, args, annots) ->
      Prim (loc, p, List.map (replace_location no_comment) args, annots)
  | Seq (_, es) -> Seq (loc, List.map (replace_location no_comment) es)

let seq diff nodes = Seq (diff, nodes)

let prim ~name ?(annots = []) diff nodes = Prim (diff, name, nodes, annots)

type node = Micheline_printer.node

type ('a, 'b) diff_item = Both of 'a * 'b | Left_only of 'a | Right_only of 'b

type node_state = {
  is_different : bool;
  accum : node list;
  constr : node list -> node;
}

type ('a, 'b) state_level = {
  queue : ('a, 'b) diff_item list;
  node_state : node_state;
}

(* Essentially a non-empty list. *)
type ('a, 'b) state_stack =
  | Bottom of ('a, 'b) state_level
  | Level of ('a, 'b) state_level * ('a, 'b) state_stack

let initial =
  {
    queue = [];
    node_state = {is_different = false; accum = []; constr = List.hd};
  }

let rec zip_nodes = function
  | [], [] -> []
  | p :: prevs, [] -> Left_only p :: zip_nodes (prevs, [])
  | [], c :: curs -> Right_only c :: zip_nodes ([], curs)
  | p :: prevs, c :: curs -> Both (p, c) :: zip_nodes (prevs, curs)

let add_stack_level ~constr ~children ~diff state_stack =
  let Micheline_printer.{comment} = diff in
  let level =
    {
      queue = zip_nodes children;
      node_state =
        {
          is_different = Option.is_some comment;
          accum = [];
          constr = constr diff;
        };
    }
  in
  Level (level, state_stack)

let update_node_state is_different node state =
  {
    state with
    is_different = is_different || state.is_different;
    accum = node :: state.accum;
  }

let fold_state {is_different; accum; constr} =
  (is_different, constr @@ List.rev accum)

let accumulate_child (is_different, node) = function
  | Bottom {queue; node_state} ->
      Bottom
        {queue; node_state = update_node_state is_different node node_state}
  | Level ({queue; node_state}, stack) ->
      Level
        ( {queue; node_state = update_node_state is_different node node_state},
          stack )

let diff_simple prev cur state =
  match (prev, cur) with
  | Int (_, p), Int (_, c) when Z.equal p c ->
      accumulate_child (false, Int (no_comment, p)) state
  | String (_, p), String (_, c) when String.equal p c ->
      accumulate_child (false, String (no_comment, p)) state
  | Bytes (_, p), Bytes (_, c) when Bytes.equal p c ->
      accumulate_child (false, Bytes (no_comment, p)) state
  (* This function won't be called with pairs (Seq, Seq) or (Prim, Prim),
     so we don't care about looking inside those. This is taken care of
     elsewhere. *)
  | prev, cur ->
      accumulate_child (true, replace_location (replaced cur) prev) state

let rec dequeue = function
  | Bottom {queue = []; node_state} -> fold_state node_state
  | Bottom {queue = item :: items; node_state} ->
      (diff_step [@ocaml.tailcall]) (Bottom {queue = items; node_state}) item
  | Level ({queue = []; node_state}, stack) ->
      (dequeue [@ocaml.tailcall])
        (accumulate_child (fold_state node_state) stack)
  | Level ({queue = item :: items; node_state}, stack) ->
      (diff_step [@ocaml.tailcall])
        (Level ({queue = items; node_state}, stack))
        item

and diff_step state nodes =
  dequeue
  @@
  match nodes with
  | Right_only (Seq (_, curs)) ->
      add_stack_level ~constr:seq ~children:([], curs) ~diff:added state
  | Right_only (Prim (_, name, args, annots)) ->
      add_stack_level
        ~constr:(prim ~name ~annots)
        ~children:([], args)
        ~diff:added
        state
  | Right_only ((Int _ | String _ | Bytes _) as expr) ->
      accumulate_child (true, replace_location added expr) state
  | Left_only (Seq (_, prevs)) ->
      add_stack_level ~constr:seq ~children:(prevs, []) ~diff:removed state
  | Left_only (Prim (_, name, args, annots)) ->
      add_stack_level
        ~constr:(prim ~name ~annots)
        ~children:(args, [])
        ~diff:removed
        state
  | Left_only ((Int _ | String _ | Bytes _) as expr) ->
      accumulate_child (true, replace_location removed expr) state
  | Both (Seq (_, prevs), Seq (_, curs)) ->
      add_stack_level ~constr:seq ~children:(prevs, curs) ~diff:no_comment state
  | Both
      ( Prim (_, prev_name, args_prev, annots),
        (Prim (_, cur_name, args_cur, _) as cur) ) ->
      add_stack_level
        ~constr:(prim ~name:prev_name ~annots)
        ~children:(args_prev, args_cur)
        ~diff:(if prev_name = cur_name then no_comment else replaced cur)
        state
  | Both (Seq (_, prevs), (Prim (_, _, args_cur, _) as cur)) ->
      add_stack_level
        ~constr:seq
        ~children:(prevs, args_cur)
        ~diff:(replaced cur)
        state
  | Both (Prim (_, name, args_prev, annots), (Seq (_, curs) as cur)) ->
      add_stack_level
        ~constr:(prim ~name ~annots)
        ~children:(args_prev, curs)
        ~diff:(replaced cur)
        state
  | Both ((Seq (_, _) as prevs), ((Int _ | String _ | Bytes _) as cur)) ->
      accumulate_child (true, replace_location (replaced cur) prevs) state
  | Both ((Prim (_, _, _, _) as prev), ((Int _ | String _ | Bytes _) as cur)) ->
      accumulate_child (true, replace_location (replaced cur) prev) state
  | Both (((Int _ | String _ | Bytes _) as prev), cur) ->
      diff_simple prev cur state

let diff ~prev ~current () =
  let is_different, diff = diff_step (Bottom initial) (Both (prev, current)) in
  if is_different then Some diff else None
