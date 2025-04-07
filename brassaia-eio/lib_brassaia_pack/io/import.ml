(*
 * Copyright (c)2018-2022 Tarides <contact@tarides.com>
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

module Brassaia = Brassaia_eio.Brassaia
module Brassaia_pack = Brassaia_eio_pack.Brassaia_pack
include Brassaia.Export_for_backends
module Path = Brassaia.Path

let src = Logs.Src.create "brassaia-pack.unix" ~doc:"brassaia-pack unix backend"

module Log = (val Logs.src_log src : Logs.LOG)

module Array = struct
  include Array

  let find_opt p a =
    let n = length a in
    let rec loop i =
      if i = n then None
      else
        let x = get a i in
        if p x then Some x else loop (succ i)
    in
    loop 0
end

module List = struct
  include List

  let rec iter_result f = function
    | [] -> Ok ()
    | hd :: tl -> Result.bind (f hd) (fun () -> iter_result f tl)
end

module Int63 = struct
  include Optint.Int63

  let t = Brassaia.Type.int63

  let encoding =
    (* [is_immediate] is [True] if on a 64 bits architecture, [False] on a 32 bits one *)
    match is_immediate with
    | True -> Data_encoding.conv to_int64 of_int64 Data_encoding.int64
    | False ->
        failwith "Int63.encoding: 32 bits architectures are not supported"

  module Syntax = struct
    let ( + ) = add

    let ( - ) = sub

    let ( * ) = mul

    let ( / ) = div

    let ( < ) a b = compare a b < 0

    let ( <= ) a b = compare a b <= 0

    let ( > ) a b = compare a b > 0

    let ( >= ) a b = compare a b >= 0

    let ( = ) = equal
  end
end

type int63 = Int63.t [@@deriving brassaia]

module Version = Brassaia_pack.Version

module type S = Brassaia_pack.S

module Conf = Brassaia_pack.Conf
module Layout = Brassaia_pack.Layout
module Indexable = Brassaia_pack.Indexable

module Result_syntax = struct
  let ( let+ ) res f = Result.map f res

  let ( let* ) = Result.bind

  let ( >>= ) = Result.bind
end

module Varint = struct
  type t = int [@@deriving brassaia ~decode_bin]

  (** LEB128 stores 7 bits per byte. An OCaml [int] has at most 63 bits.
      [63 / 7] equals [9]. *)
  let max_encoded_size = 9
end

module Mtime = struct
  include Mtime

  module Span = struct
    include Mtime.Span

    let to_s span = Mtime.Span.to_float_ns span *. 1e-9

    let to_us span = Mtime.Span.to_float_ns span *. 1e-3
  end
end
