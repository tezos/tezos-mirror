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

include Key_intf

module Of_hash (Hash : Type.S) = struct
  type t = Hash.t [@@deriving brassaia]
  type hash = Hash.t

  let to_hash x = x [@@inline]
  let of_hash x = x [@@inline]

  let encoding =
    Data_encoding.conv (Type.to_string Hash.t)
      Type.(of_string_exn ~path:"lib_brassaia/key.ml/Of_hash/of_string" Hash.t)
      Data_encoding.string

  let pp ppf t = Type.(pp Hash.t) ppf t
end
