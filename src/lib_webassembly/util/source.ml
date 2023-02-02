type pos = {file : string; line : int; column : int} [@@deriving show]

type region = {left : pos; right : pos} [@@deriving show]

type 'a phrase = {at : region; it : 'a} [@@deriving show]

let ( @@ ) x region = {it = x; at = region}

let at region x = x @@ region

(* Positions and regions *)

let no_pos = {file = ""; line = 0; column = 0}

let no_region = {left = no_pos; right = no_pos}

let string_of_pos pos =
  if pos.line = -1 then Format.sprintf "0x%x" pos.column
  else string_of_int pos.line ^ "." ^ string_of_int (pos.column + 1)

let string_of_region r =
  r.left.file ^ ":" ^ string_of_pos r.left
  ^ if r.right = r.left then "" else "-" ^ string_of_pos r.right
