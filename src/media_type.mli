(**************************************************************************)
(*  resto                                                                 *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*  Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>       *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

module Make (Encoding : Resto.ENCODING) : sig
  type t = {
    name : Cohttp.Accept.media_range;
    q : int option;
    pp : 'a. 'a Encoding.t -> Format.formatter -> string -> unit;
    (* [construct] constructs the answer in one go. This is fine for values that
       are small enough: the serialising takes little time, the serialised value
       takes little space, the writing on the network connection takes little
       time. For large values this is problematic: the serialisation is
       non-blocking leading to a Lwt hang-up, the memory footprint of the
       serialisation is at least as big as the serialised value (large), the
       writing on the network takes a long time. In that case, there is
       [construct_seq] below. *)
    construct : 'a. 'a Encoding.t -> 'a -> string;
    (* [construct_seq] constructs the answer lazily as chunks of text to be
       blitted onto whatever buffer/socket/other the server is handling
       internally. This is meant to circumvent the issues mentioned above. Note
       that Resto will yield in between the use of two consecutive chunks of
       text. *)
    construct_seq : 'a. 'a Encoding.t -> 'a -> (Bytes.t * int * int) Seq.t;
    destruct : 'a. 'a Encoding.t -> string -> ('a, string) result;
  }

  val name : t -> string

  val has_complete_media : t list -> bool

  val first_complete_media : t list -> ((string * string) * t) option

  val find_media : string * string -> t list -> t option

  val resolve_accept_header : t list -> string option -> (string * t) option

  val accept_header : t list -> string

  val acceptable_encoding : t list -> string
end
