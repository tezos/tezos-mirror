(** UTF8 exceptions *)
exception Utf8

(** Decoder exceptions *)
module Decode_error = Error.Make ()

exception EOS

(** Encoder exceptions *)
module Encode_error = Error.Make ()

module Floating_point = Error.Make ()
