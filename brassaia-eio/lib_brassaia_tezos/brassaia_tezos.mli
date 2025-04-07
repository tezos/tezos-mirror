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

module Schema = Schema
module Brassaia_pack = Brassaia_eio_pack.Brassaia_pack
module Brassaia_pack_unix = Brassaia_eio_pack_io.Brassaia_pack_unix

module Conf : Brassaia_pack.Conf.S

module Store :
  Brassaia_pack_unix.S
    with type Schema.Hash.t = Schema.Hash.t
     and type Schema.Branch.t = Schema.Branch.t
     and type Schema.Contents.t = Schema.Contents.t
     and type Backend.Remote.endpoint = unit
     and type contents_key = Schema.Hash.t Brassaia_pack_unix.Pack_key.t
     and type node_key = Schema.Hash.t Brassaia_pack_unix.Pack_key.t
     and type commit_key = Schema.Hash.t Brassaia_pack_unix.Pack_key.t
