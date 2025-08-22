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

module Atomic_write (K : Brassaia.Type.S) (V : Brassaia.Hash.S) = struct
  module AW = Brassaia_mem.Atomic_write (K) (V)
  include AW

  let init () = AW.init (Brassaia_mem.config ())
  let flush _t = ()
end

module Indexable_mem
    (Hash : Brassaia.Hash.S)
    (Value :
      Brassaia_pack.Pack_value.S with type hash := Hash.t and type key = Hash.t) =
struct
  module Pack = Indexable.Maker (Hash)
  module Indexable_mem = Pack.Make (Value)
  include Brassaia_pack.Indexable.Closeable (Indexable_mem)

  let init x = Indexable_mem.init x >|= make_closeable
end

module Maker (Config : Brassaia_pack.Conf.S) = struct
  type endpoint = unit

  include Brassaia.Key.Store_spec.Hash_keyed

  module Make (Schema : Brassaia.Schema.Extended) = struct
    module H = Schema.Hash
    module C = Schema.Contents
    module B = Schema.Branch
    module Pack = Indexable.Maker (H)

    module XKey = struct
      include Brassaia.Key.Of_hash (H)

      let unfindable_of_hash x = x
    end

    module X = struct
      module Schema = Schema
      module Hash = H
      module Info = Schema.Info

      module Contents = struct
        module Pack_value =
          Brassaia_pack.Pack_value.Of_contents (Config) (H) (XKey) (C)

        module Indexable = Indexable_mem (H) (Pack_value)
        include Brassaia.Contents.Store_indexable (Indexable) (H) (C)
      end

      module Node = struct
        module Value = Schema.Node (XKey) (XKey)

        module Indexable = struct
          module Inter =
            Brassaia_pack.Inode.Make_internal (Config) (H) (XKey) (Value)

          module CA = Pack.Make (Inter.Raw)
          include Brassaia_pack.Inode.Make (H) (XKey) (Value) (Inter) (CA)

          let init = CA.init
        end

        include
          Brassaia.Node.Generic_key.Store (Contents) (Indexable) (H)
            (Indexable.Val)
      end

      module Node_portable = Node.Indexable.Val.Portable

      module Commit = struct
        module Value = struct
          include Schema.Commit (Node.Key) (XKey)
          module Info = Schema.Info

          type hash = Hash.t [@@deriving brassaia]
        end

        module Pack_value =
          Brassaia_pack.Pack_value.Of_commit (H) (XKey) (Value)

        module Indexable = Indexable_mem (H) (Pack_value)

        include
          Brassaia.Commit.Generic_key.Store (Info) (Node) (Indexable) (H)
            (Value)
      end

      module Commit_portable = Brassaia.Commit.Portable.Of_commit (Commit.Value)

      module Branch = struct
        module Key = B

        module Val = struct
          include H
          include Commit.Key
        end

        module AW = Atomic_write (Key) (Val)
        include Brassaia_pack.Atomic_write.Closeable (AW)

        let init () = AW.init () >|= make_closeable
      end

      module Slice = Brassaia.Backend.Slice.Make (Contents) (Node) (Commit)
      module Remote = Brassaia.Backend.Remote.None (H) (B)

      module Repo = struct
        type t = {
          config : Brassaia.Backend.Conf.t;
          contents : read Contents.Indexable.t;
          node : read Node.Indexable.t;
          commit : read Commit.Indexable.t;
          branch : Branch.t;
        }

        let contents_t t : 'a Contents.t = t.contents
        let node_t t : 'a Node.t = (contents_t t, t.node)
        let commit_t t : 'a Commit.t = (node_t t, t.commit)
        let branch_t t = t.branch
        let config t = t.config

        let batch t f =
          Commit.Indexable.batch t.commit (fun commit ->
              Node.Indexable.batch t.node (fun node ->
                  Contents.Indexable.batch t.contents (fun contents ->
                      let contents : 'a Contents.t = contents in
                      let node : 'a Node.t = (contents, node) in
                      let commit : 'a Commit.t = (node, commit) in
                      f contents node commit)))

        let init config =
          let root = Brassaia_pack.Conf.root config in
          let* contents = Contents.Indexable.init root in
          let* node = Node.Indexable.init root in
          let* commit = Commit.Indexable.init root in
          let+ branch = Branch.init () in
          { contents; node; commit; branch; config }

        let close t =
          Contents.Indexable.close (contents_t t) >>= fun () ->
          Node.Indexable.close (snd (node_t t)) >>= fun () ->
          Commit.Indexable.close (snd (commit_t t)) >>= fun () ->
          Branch.close t.branch
      end
    end

    include Brassaia.Of_backend (X)
  end
end
