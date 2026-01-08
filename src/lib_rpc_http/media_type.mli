(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = Resto_cohttp.Media_type.Make(Tezos_rpc.Encoding).t = {
  name : Cohttp.Accept.media_range;
  q : int option;
  pp : 'a. 'a Data_encoding.t -> Format.formatter -> string -> unit;
  construct : 'a. 'a Data_encoding.t -> 'a -> string;
  construct_seq : 'a. 'a Data_encoding.t -> 'a -> (Bytes.t * int * int) Seq.t;
  destruct : 'a. 'a Data_encoding.t -> string -> ('a, string) result;
  destruct_many : 'a. 'a Data_encoding.t -> string -> 'a list * string;
}

val name : t -> string

val json : t

val bson : t

val octet_stream : t

val all_media_types : t list

val accept_header : t list -> string

val acceptable_encoding : t list -> string

val first_complete_media : t list -> ((string * string) * t) option

val encoding : t Data_encoding.t

module Content_type : sig
  type t = string * string

  val json : t

  val bson : t

  val octet_stream : t

  val pp : Format.formatter -> t -> unit
end

val of_content_type : Content_type.t -> t option

module Command_line : sig
  type t = Any | Json | Binary

  val parse_cli_parameter : string -> t option

  val pp_parameter : Format.formatter -> t -> unit

  val of_command_line :
    t -> Resto_cohttp.Media_type.Make(Tezos_rpc__RPC_encoding).t list

  val encoding : t Data_encoding.encoding
end
