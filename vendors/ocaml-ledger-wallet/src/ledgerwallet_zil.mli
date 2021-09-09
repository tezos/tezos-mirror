(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

val get_version :
  ?pp:Format.formatter -> ?buf:Cstruct.t -> Hidapi.t ->
  (int * int * int, Ledgerwallet.Transport.error) result

val get_pk :
  ?display_addr:bool ->
  ?pp:Format.formatter -> ?buf:Cstruct.t -> Hidapi.t -> int32 ->
  (Cstruct.t * [`Zil] Bech32.Segwit.t, Ledgerwallet.Transport.error) result

val sign_hash :
  ?pp:Format.formatter -> ?buf:Cstruct.t -> Hidapi.t -> int32 -> Cstruct.t ->
  (Cstruct.t, Ledgerwallet.Transport.error) result
(** data must be a 32 bytes-len hash. *)

val sign_txn :
  ?pp:Format.formatter -> ?buf:Cstruct.t -> Hidapi.t -> int32 -> Cstruct.t ->
  (Cstruct.t, Ledgerwallet.Transport.error) result
(** data can be arbitrary len. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
