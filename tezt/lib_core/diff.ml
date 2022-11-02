(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Base

exception Found of int * int

type item_report = Kept of int * int | Added of int | Removed of int

type 'a t = {
  before : string;
  after : string;
  merged : (item_report * 'a) array;
  different : bool;
}

let arrays ?(max_sync_distance = 100) ?(equal = ( = )) ?(before = "before")
    ?(after = "after") a b =
  let alen = Array.length a in
  let blen = Array.length b in
  if alen = 0 && blen = 0 then {before; after; merged = [||]; different = false}
  else
    let dummy = if alen > 0 then a.(0) else b.(0) in
    let merged = Array.make (alen + blen) (Kept (0, 0), dummy) in
    let merged_next = ref 0 in
    let different = ref false in
    let push report item =
      merged.(!merged_next) <- (report, item) ;
      incr merged_next
    in
    let push_range report array first last =
      for i = first to last do
        push (report i) array.(i)
      done
    in
    let push_range_added = push_range @@ fun i -> Added i in
    let push_range_removed = push_range @@ fun i -> Removed i in
    let rec loop ia ib =
      match (ia < alen, ib < blen) with
      | false, false ->
          (* Nothing to read left. *)
          ()
      | false, true ->
          (* Done reading a: everything left in b was added. *)
          push_range_added b ib (blen - 1) ;
          different := true
      | true, false ->
          (* Done reading b: everything left in a was removed. *)
          push_range_removed a ia (alen - 1) ;
          different := true
      | true, true -> (
          if equal a.(ia) b.(ib) then (
            (* Still synchronized, continue. *)
            push (Kept (ia, ib)) a.(ia) ;
            loop (ia + 1) (ib + 1))
          else
            (* Desynchronized: try to resynchronize.
               Search for two equal items, starting from the current items.
               Search for them at increasing distances. *)
            try
              for d = 1 to max_sync_distance do
                for db = 0 to d do
                  let da = d - db in
                  (* Total distance is d = da + db.
                     Since the outer loop is on d, we'll find the closest match first. *)
                  if ia + da < alen && ib + db < blen then
                    if equal a.(ia + da) b.(ib + db) then
                      raise (Found (ia + da, ib + db))
                done
              done ;
              (* Didn't find equal items, consider everything to be different. *)
              push_range_removed a ia (alen - 1) ;
              push_range_added b ib (blen - 1) ;
              different := true
            with Found (sync_ia, sync_ib) ->
              (* Found a synchronization point. Report the diff... *)
              push_range_removed a ia (sync_ia - 1) ;
              push_range_added b ib (sync_ib - 1) ;
              push (Kept (sync_ia, sync_ib)) a.(sync_ia) ;
              different := true ;
              (* ...and continue from just after the synchronization point. *)
              loop (sync_ia + 1) (sync_ib + 1))
    in
    loop 0 0 ;
    {
      before;
      after;
      merged = Array.sub merged 0 !merged_next;
      different = !different;
    }

let files ?max_sync_distance ?before ?after a b =
  let before = Option.value before ~default:a in
  let after = Option.value after ~default:b in
  let read filename =
    read_file filename |> String.split_on_char '\n' |> Array.of_list
  in
  let a = read a in
  let b = read b in
  arrays ?max_sync_distance ~equal:String.equal ~before ~after a b

(* Circular buffers for context before. *)
module Context : sig
  type 'a t

  val create : int -> 'a -> 'a t

  val add : 'a t -> 'a -> unit

  val iter_and_clear : 'a t -> ('a -> unit) -> unit
end = struct
  (* Items are from [start] to [start + count - 1] modulo capacity. *)
  type 'a t = {items : 'a array; mutable start : int; mutable count : int}

  let create capacity dummy_item =
    {items = Array.make capacity dummy_item; start = 0; count = 0}

  let add context item =
    let capacity = Array.length context.items in
    let index = (context.start + context.count) mod capacity in
    context.items.(index) <- item ;
    if context.count < capacity then context.count <- context.count + 1
    else context.start <- context.start + 1

  let iter_and_clear context f =
    let capacity = Array.length context.items in
    for i = 0 to context.count - 1 do
      let index = (context.start + i) mod capacity in
      f context.items.(index)
    done ;
    context.count <- 0
end

let reduce_context_array ?(before = 3) ?(after = 3) array =
  if Array.length array = 0 then [||]
  else
    let push, return_all =
      let list = ref [] in
      let push item = list := item :: !list in
      let return_all () = Array.of_list (List.rev !list) in
      (push, return_all)
    in
    let context_before = Context.create before array.(0) in
    let remaining_context_after = ref 0 in
    let handle_item ((report, _) as item) =
      match report with
      | Kept _ ->
          if !remaining_context_after > 0 then (
            decr remaining_context_after ;
            push item)
          else Context.add context_before item
      | Added _ | Removed _ ->
          Context.iter_and_clear context_before push ;
          push item ;
          remaining_context_after := after
    in
    Array.iter handle_item array ;
    return_all ()

let reduce_context ?before ?after diff =
  if diff.different then
    {diff with merged = reduce_context_array ?before ?after diff.merged}
  else {diff with merged = [||]}

let output_item_report output_line show_item
    (previous_index_before, previous_index_after) (report, item) =
  let item = show_item item in
  let gap, char, color, index_before, index_after =
    match report with
    | Kept (index_before, index_after) ->
        (* We expect gaps to occur because of [reduce_context], which only breaks
           continuity before [Kept] lines. *)
        let gap =
          index_before <> previous_index_after + 1
          && index_after <> previous_index_after + 1
        in
        (gap, ' ', None, index_before, index_after)
    | Added index_after ->
        (false, '+', Some Log.Color.FG.green, previous_index_before, index_after)
    | Removed index_before ->
        (false, '-', Some Log.Color.FG.red, index_before, previous_index_after)
  in
  if gap then
    output_line
      (Some Log.Color.FG.cyan)
      (sf "@@ -%d +%d @@" index_before index_after) ;
  output_line color (String.make 1 char ^ item) ;
  (index_before, index_after)

let output output_line show_item diff =
  if diff.different then (
    output_line None ("--- " ^ diff.before) ;
    output_line None ("+++ " ^ diff.after) ;
    let _, _ =
      Array.fold_left
        (output_item_report output_line show_item)
        (-2, -2)
        diff.merged
    in
    ())

let log ?(level = Cli.Info) diff =
  output (fun color -> Log.log ~level ?color "%s") Fun.id diff
