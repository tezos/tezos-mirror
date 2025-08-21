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

module Brassaia = Brassaia_eio.Brassaia
module Test_node =
  Brassaia_eio_test_helpers.Brassaia_test.Node.Make
    (Brassaia.Node.Generic_key.Make)

let suite =
  [
    ("lru", Test_lru.suite);
    ("tree", Test_tree.suite);
    ("node", Test_node.suite);
    ("hash", Test_hash.suite);
    ("conf", Test_conf.suite);
  ]

let () =
  Logs.set_level (Some Info) ;
  Logs.set_reporter (Brassaia_eio_test_helpers.Brassaia_test.reporter ()) ;
  Random.self_init () ;
  Alcotest.run ~__FILE__ ~tags:["ci_disabled"] "brassaia-eio" suite
