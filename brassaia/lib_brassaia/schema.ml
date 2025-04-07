(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2020-2021 Craig Ferguson <me@craigfe.io>
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

module type S = sig
  module Hash : Hash.S
  module Branch : Branch.S
  module Info : Info.S
  module Contents : Contents.S
end

(* module Hash = Hash.BLAKE2B *)
(* module Branch = Branch.String *)
(* module Contents = Contents.String *)

module type Extended = sig
  include S

  module Node
      (Contents_key : Key.S with type hash = Hash.t)
      (Node_key : Key.S with type hash = Hash.t) :
    Node.Generic_key.S
      with type hash = Hash.t
       and type contents_key = Contents_key.t
       and type node_key = Node_key.t

  module Commit
      (Node_key : Key.S with type hash = Hash.t)
      (Commit_key : Key.S with type hash = Hash.t) :
    Commit.Generic_key.S
      with module Info := Info
       and type node_key = Node_key.t
       and type commit_key = Commit_key.t
end

open struct
  module Extended_is_a_schema (X : Extended) : S = X
end

type default_hash = Hash.BLAKE2B.t

module type KV =
  Extended
    with type Hash.t = default_hash
     and type Branch.t = string
     and type Info.t = Info.default

module KV (C : Contents.S) : KV with module Contents = C = struct
  module Hash = Hash.BLAKE2B
  module Info = Info.Default
  module Branch = Branch.String
  module Contents = C
  module Node = Node.Generic_key.Make (Hash)
  module Commit = Commit.Generic_key.Make (Hash)
end
