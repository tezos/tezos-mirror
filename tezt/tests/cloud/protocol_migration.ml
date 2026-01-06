(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type offset = Cycle_offset of int | Level_offset of int

let offset_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"cycle_offset"
        (obj1 (req "cycle_offset" int31))
        (function Cycle_offset c -> Some c | _ -> None)
        (fun c -> Cycle_offset c);
      case
        (Tag 1)
        ~title:"level_offset"
        (obj1 (req "level_offset" int31))
        (function Level_offset d -> Some d | _ -> None)
        (fun d -> Level_offset d);
    ]

let offset_pp fmt = function
  | Cycle_offset c -> Format.fprintf fmt "Cycle %d" c
  | Level_offset l -> Format.fprintf fmt "Level %d" l

type t = {migration_offset : offset; termination_offset : offset}

let encoding =
  let open Data_encoding in
  conv
    (fun {migration_offset; termination_offset} ->
      (migration_offset, termination_offset))
    (fun (migration_offset, termination_offset) ->
      {migration_offset; termination_offset})
    (obj2 (req "migration" offset_encoding) (req "termination" offset_encoding))

let typ =
  let parse s =
    let parse_aux string =
      try
        match string |> String.split_on_char ' ' with
        | ["Cycle_offset"; n] -> Cycle_offset (int_of_string n)
        | ["Level_offset"; n] -> Level_offset (int_of_string n)
        | _ ->
            failwith
              {|Migration offset should be of the form "Level_offset <n>" or "Cycle_offset <n>"|}
      with exn -> raise exn
    in
    match String.split_on_char ',' s with
    | [migration; termination] ->
        let migration_offset = parse_aux migration in
        let termination_offset = parse_aux termination in
        Some {migration_offset; termination_offset}
    | _ ->
        failwith
          {|Protocol migration should be of the form "Level_offset <n1>,Level_offset <n2>" or "Cycle_offset <c1>, Cycle_offset <c2>".|}
  in
  let show t =
    Format.asprintf
      "(Migration offset: %a; Termination offset: %a)"
      offset_pp
      t.migration_offset
      offset_pp
      t.termination_offset
  in
  Clap.typ
    ~name:"migration"
    ~dummy:
      {migration_offset = Cycle_offset 0; termination_offset = Level_offset 20}
    ~parse
    ~show
