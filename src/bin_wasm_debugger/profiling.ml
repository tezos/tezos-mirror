(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Tezos_scoru_wasm
open Tezos_lazy_containers
open Tezos_webassembly_interpreter
module Vector = Lazy_vector.Int32Vector

(** Call graph representation and construction. *)

(** The call stack computation algorithm is the following:

    There are two components: the current node (or stack frame) and the
    continuation (a list of stack frames). There's a "toplevel node" describing
    the execution at the toplevel of the interpreter.
    A node contains:
    - [id]: a function call representation (an identifier)
    - [t]: the ticks elapsed during the call
    - [sub]: the subcalls.

    The algorithm starts with an empty toplevel and an empty continuation.
    - on function call (id, current_tick, current_node, continuation):
    1. create a node N_id: (id, t: current_tick, sub:[])
    2. update current_node N_curr with t:(current_tick - t)
      => the number of ticks is now the diff between the moment the call started
          and the subcall started.
    3. push N_curr on the continuation
    4. return N_id, continuation

    - on function end (current_tick, current_node, continuation):
    1. update current_node N_curr with t:(current_tick - t)
    2. pop N_prev from the continuation
    3. update N_prev: t:(current_tick - t) sub:(sub + N_curr)
    4. return N_prev, continuation

    Let's take an example:
    call: f () { ...... g () { .... h () { ...... } .......... } ......... }
    tick: 0 ----------> 10 -------> 30 ---------> 60 --------> 100 -----> 160
          [  10 ticks  ] [ 20 ticks ] [ 30 ticks ] [ 40 ticks ] [ 60 ticks ]

    - `f` takes 10 + 60 = 70 ticks
    - `g` takes 20 + 40 = 60 ticks
    - `h` takes 30 ticks

    T ([nodes]) : toplevel
    K : continuation (list)
    N : current node (N(id) means it hasn't changed from previous step)

    N, K |- exec

    Start:
    T, [] |- f () { g () { h () { } } }
    ==> at tick 0
    N (f, 0, []), [T] |- g () { h () { } } }
    ==> at tick 10
    N (g, 10, []), [N (f, 10 - 0 = 10, []); T] |- h () { } } }
    ==> at tick 30
    N (h, 30, []), [N (g, 30 - 10 = 20, []); N(f); T] |- } } }
    ==> at tick 60
    N (g, 60 - 20 = 40, [N (h, 60 - 30 = 30, [])]), [N(f); T] |- } }
    ==> at tick 100
    N (f, 100 - 10 = 90, [N (g, 100 - 40 = 60, [N(h)])]), [T] |- }
    ==> at tick 160
    T [N (f, 160 - 90 = 70, [N(g, 60, [N(h, 30, [])])])], [] |- _

*)

type 'function_call call_stack =
  | Node of 'function_call * Z.t * 'function_call call_stack list
  | Toplevel of 'function_call call_stack list

(** [end_function_call current_tick current_function call_stack] implements an
    ending call. Please refer to the prelude of the file. *)
let end_function_call current_tick current_function call_stack =
  match current_function with
  | Node (call, starting_tick, subcalls) -> (
      let final_node =
        Node (call, Z.sub current_tick starting_tick, List.rev subcalls)
      in
      match call_stack with
      | [] -> assert false
      | Toplevel finalized :: stack ->
          (Toplevel (final_node :: finalized), stack)
      | Node (call, ticks, subcalls) :: stack ->
          (Node (call, Z.sub current_tick ticks, final_node :: subcalls), stack)
      )
  (* A toplevel call cannot reduce. *)
  | Toplevel _ -> (current_function, call_stack)

(** [call_function called_function current_tick current_function call_stack]
    implements a function start. Please refere to the prelude of the module. *)
let call_function called_function current_tick current_function call_stack =
  match current_function with
  | Toplevel _ as top ->
      let func = Node (called_function, current_tick, []) in
      (func, top :: call_stack)
  | Node (current_call, ticks, subcalls) ->
      let stack =
        Node (current_call, Z.sub current_tick ticks, subcalls) :: call_stack
      in
      let func = Node (called_function, current_tick, []) in
      (func, stack)
