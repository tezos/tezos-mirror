(** [decode_step get stream] reads a character (or more) from a stream [stream]
    using [get] and returns its Utf8 representation and the bytes read from the
    stream. Reads at most 4 bytes.

    @raise Binary_exn.Utf8 in case the encoding is illformed with regards to the Utf8
      conventions. *)
val decode_step : ('a -> int Lwt.t) -> 'a -> (int * int list) Lwt.t

(** [encode_int i] encodes an Utf8 represented into an integer into its char
    codes.

    @raise Binary_exn.Utf8 in case the encoding is illformed with regards to the Utf8
      conventions. *)
val encode_int : int -> int list

(** [encode vec] decodes a string into its UTF8 vector representation.

    @raise Binary_exn.Utf8 in case the encoding is illformed with regards to the Utf8
      conventions. *)
val decode : string -> int Lazy_vector.Int32Vector.t

(** [encode vec] encodes an UTF8 vector into its string representation.

    @raise Binary_exn.Utf8 in case the encoding is illformed with regards to the Utf8
      conventions. *)
val encode : int Lazy_vector.Int32Vector.t -> string Lwt.t

(** [encode_unsafe vec] encodes an UTF8 vector into its string representation
    without forcing the values in the lazy_vector. This function is witness of
    the internal mutation, and as such should only be used in the binary part of
    the interpreter, never in the PVM.

    @raise Binary_exn.Utf8 in case the encoding is illformed with regards to the Utf8
      conventions. *)
val encode_unsafe : int Lazy_vector.Int32Vector.t -> string

(** [encode_list vec] encodes an UTF8 bytes list into its string representation.

    @raise Binary_exn.Utf8 in case the encoding is illformed with regards to the Utf8
      conventions. *)
val encode_list : int list -> string
