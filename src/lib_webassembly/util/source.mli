type pos = {file : string; line : int; column : int} [@@deriving show]

type region = {left : pos; right : pos} [@@deriving show]

type 'a phrase = {at : region; it : 'a} [@@deriving show]

val no_pos : pos

val no_region : region

val string_of_pos : pos -> string

val string_of_region : region -> string

val ( @@ ) : 'a -> region -> 'a phrase

val at : region -> 'a -> 'a phrase
