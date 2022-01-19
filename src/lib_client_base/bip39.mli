(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Abstract type of a mnemonic *)
type t

val pp : Format.formatter -> t -> unit

val show : t -> string

(** [find_index word] is [Some i] where is is the index of [word] in
    the BIP39 word list, or [None] if no such word is in the list. *)
val index_of_word : string -> int option

(** [of_indices idxs] is [Some mnemonic] if indices are all in range
    [0-2047] or [None] otherwise. *)
val of_indices : int list -> t option

(** [to_indices t] is the list of indices corresponding to [t]. *)
val to_indices : t -> int list

(** [of_words words] is [Some mnemonic] if [words] is a list
    containing a valids number of valid english words. *)
val of_words : string list -> t option

(** [to_words mnemonic] is the list of words corresponding to
    [mnemonic]. *)
val to_words : t -> string list

(** [of_entropy bytes] is the mnemonic derived from [bytes].

    @raise Invalid_argument is [List.length bytes] is not in [ 16,
    20, 24, 28, 32 ]. *)
val of_entropy : Bytes.t -> t

(** [to_seed ?passphrase mnemonic] is 64 bytes derived from a BIP39
    mnemonic [mnemonic], using the optional passphrase [passphrase] if
    provided. *)
val to_seed : ?passphrase:Bytes.t -> t -> Bytes.t

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

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
