(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Events = struct
  include Internal_event.Simple

  let section = ["gc_setup"]

  let gc_space_overhead_changed =
    declare_3
      ~section
      ~level:Info
      ~name:"gc_space_overhead_changed"
      ~msg:
        "{process_name}: default space_overhead changed: {new} (default \
         {previous})"
      ("process_name", Data_encoding.string)
      ("new", Data_encoding.int31)
      ("previous", Data_encoding.int31)

  let gc_space_overhead_set_with_ocamlrunparam =
    declare_2
      ~section
      ~level:Info
      ~name:"gc_space_overhead_set_with_ocamlrunparam"
      ~msg:
        "{process_name}: default space_overhead changed by OCAMLRUNPARAM: \
         {new} (default 120)"
      ("process_name", Data_encoding.string)
      ("new", Data_encoding.int31)
end

let get_ocamlrunparam_param param =
  match Sys.getenv_opt "OCAMLRUNPARAM" with
  | None -> None
  | Some content ->
      let param =
        String.split_on_char ',' content
        |> List.find_opt (fun params ->
               params = param || String.starts_with ~prefix:(param ^ "=") params)
      in
      Option.map
        (fun param ->
          match String.index_opt param '=' with
          | Some index ->
              String.sub param (index + 1) (String.length param - index - 1)
          | None -> param)
        param

let set_gc_space_overhead ?(space_overhead = 40) process_name =
  (* The default [space-overhead] value of the OCaml GC is too high
     in OCaml 5.2.1, leading to out of memory (OOM) crashes for some bakers.

     Lowering the [space-overhead] gives a good compromise in terms
     of performances and memory consumption. This default policy can be changed if the user set
     an environment variable. *)
  let current = Gc.get () in
  (* "o" is the OCAMLRUNPARAM option that allows to specify Gc.space_overhead *)
  let space_overhead =
    match get_ocamlrunparam_param "o" with
    | None -> space_overhead
    | Some v ->
        let v = int_of_string v in
        Events.(
          emit_at_top_level
            gc_space_overhead_set_with_ocamlrunparam
            (process_name, v)) ;
        v
  in
  Gc.set {current with space_overhead} ;
  if space_overhead <> current.space_overhead then
    Events.(
      emit_at_top_level
        gc_space_overhead_changed
        (process_name, space_overhead, current.space_overhead))
