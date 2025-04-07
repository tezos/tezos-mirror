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

module Brassaia = Brassaia_eio.Brassaia
module Brassaia_pack = Brassaia_eio_pack.Brassaia_pack
module Brassaia_pack_unix = Brassaia_eio_pack_io.Brassaia_pack_unix

let rm_dir data_dir =
  if Sys.file_exists data_dir then
    let cmd = Printf.sprintf "rm -rf %s" data_dir in
    let _ = Sys.command cmd in
    ()

module Generator = struct
  module Conf = struct
    include Brassaia_eio_tezos.Brassaia_tezos.Conf

    let entries = 2

    let stable_hash = 3
  end

  module Schema = Brassaia.Schema.KV (Brassaia.Contents.String)

  module Store = struct
    open Brassaia_pack_unix.Maker (Conf)

    include Make (Schema)
  end

  let config ~indexing_strategy root =
    Brassaia_pack.config ~indexing_strategy ~readonly:false ~fresh:true root

  let info = Store.Info.empty

  let create_store ?(before_closing = fun _repo _head -> ()) indexing_strategy
      path =
    rm_dir path ;
    let large_contents = String.make 4096 'Z' in
    let rw = Store.Repo.init (config ~indexing_strategy path) in
    let tree = Store.Tree.singleton ["a"; "b1"; "c1"; "d1"; "e1"] "x1" in
    let tree = Store.Tree.add tree ["a"; "b1"; "c1"; "d2"; "e2"] "x2" in
    let tree = Store.Tree.add tree ["a"; "b1"; "c1"; "d3"; "e3"] "x2" in
    let tree = Store.Tree.add tree ["a"; "b2"; "c2"; "e3"] "x2" in
    let c1 = Store.Commit.init rw ~parents:[] ~info tree in

    let tree = Store.Tree.add tree ["a"; "b3"] large_contents in
    let c2 = Store.Commit.init rw ~parents:[Store.Commit.key c1] ~info tree in

    let tree = Store.Tree.remove tree ["a"; "b1"; "c1"] in
    let c3 = Store.Commit.init rw ~parents:[Store.Commit.key c2] ~info tree in

    let () = before_closing rw (Store.Commit.key c3) in

    let _ = Store.Repo.close rw in

    c3

  let create_gced_store path =
    let before_closing repo head =
      let _ = Store.Gc.start_exn repo head in
      let _ = Store.Gc.wait repo in
      ()
    in
    create_store ~before_closing Brassaia_pack.Indexing_strategy.minimal path

  let create_snapshot_store ~src ~dest =
    let before_closing repo head =
      rm_dir dest ;
      Store.create_one_commit_store repo head dest
    in
    create_store ~before_closing Brassaia_pack.Indexing_strategy.minimal src
end

let register_test title f =
  let title = Format.sprintf "Brassaia_eio: %s" title in
  Tezt.Test.register
    ~__FILE__
    ~tags:[Tag.layer1; Tag.flaky; "brassaia_eio"; "store"]
    ~title
    f

let register () =
  register_test "create store (minimal indexing strategy)" (fun () ->
      let _ =
        Generator.create_store
          Brassaia_pack.Indexing_strategy.always
          "data/minimal"
      in
      unit) ;
  register_test "create store (always indexing strategy)" (fun () ->
      let _ =
        Generator.create_store
          Brassaia_pack.Indexing_strategy.always
          "data/always"
      in
      unit) ;
  register_test "create gced store" (fun () ->
      let _ = Generator.create_gced_store "data/gced" in
      unit) ;
  register_test "create snapshot store" (fun () ->
      let _ =
        Generator.create_snapshot_store
          ~src:"data/snapshot_src"
          ~dest:"data/snapshot"
      in
      unit)
