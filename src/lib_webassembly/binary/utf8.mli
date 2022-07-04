val decode : string -> int list (* raises Utf8 *)
val encode : int list -> string (* raises Utf8 *)

(** [decode_step get stream] reads a character (or more) from a stream [stream]
    using [get] and returns its Utf8 representation and the number of characters
    read from the stream.

    @raise Binary_exn.Utf8 in case the encoding is illformed with regards to the Utf8
      conventions. *)
val decode_step : ('a -> int) -> 'a -> int * int

(** [encode_int i] encodes an Utf8 represented into an integer into its char
    codes.

    @raise Binary_exn.Utf8 in case the encoding is illformed with regards to the Utf8
      conventions. *)
val encode_int : int -> int list
