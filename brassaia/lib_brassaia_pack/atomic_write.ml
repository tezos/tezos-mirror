(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

open! Import
include Atomic_write_intf

module Value = struct
  module type S = Value

  module Of_hash (X : Brassaia.Hash.S) = struct
    type t = X.t [@@deriving brassaia ~of_bin_string]

    let encoding = X.encoding

    let null =
      match of_bin_string (String.make X.hash_size '\000') with
      | Ok x -> x
      | Error _ -> assert false
  end
end

module Closeable (AW : S) = struct
  include Brassaia.Atomic_write.Check_closed_store (AW)

  let flush t = get_if_open_exn t |> AW.flush
end
