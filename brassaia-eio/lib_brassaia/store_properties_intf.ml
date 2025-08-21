(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Import

module type Batch = sig
  type 'a t

  (** [batch t f] applies the writes in [f] in a separate batch. The exact
      guarantees depend on the implementation. *)
  val batch : read t -> ([read | write] t -> 'a) -> 'a
end

module type Closeable = sig
  type 'a t

  (** [close t] frees up all the resources associated with [t]. Any operations
      run on a closed handle will raise [Closed]. *)
  val close : 'a t -> unit
end

module type Of_config = sig
  type 'a t

  (** [init config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)
  val init : Conf.t -> read t
end

module type Clearable = sig
  type 'a t

  (** Clear the store. This operation is expected to be slow. *)
  val clear : 'a t -> unit
end

module type Sigs = sig
  exception Closed

  module type Batch = sig
    (** @inline *)
    include Batch
  end

  module type Closeable = sig
    (** @inline *)
    include Closeable
  end

  module type Of_config = sig
    (** @inline *)
    include Of_config
  end

  module type Clearable = sig
    (** @inline *)
    include Clearable
  end
end
