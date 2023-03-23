(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    P2P
    Invocation:   dune exec src/lib_p2p/test/main.exe
    Subject:      On banning peers and usage of Access Control Lists (ACL)
                  using sets and POSIX timestamps.
*)

include Internal_event.Legacy_logging.Make (struct
  let name = "test-p2p-banned_peers"
end)

let assert_equal_bool ~msg a b = if a <> b then Alcotest.fail msg

let a (peer, addr) =
  (P2p_peer.Id.hash_string [peer], Ipaddr.V6.of_string_exn addr)

let foo = a ("foo", "ffff::3")

let bar = a ("bar", "ffff:00::ff")

let baz = a ("baz", "a::2")

let peers = [foo; bar; baz]

(** In an empty ACL of size 10, nobody is tagged as blacklisted or
    greylisted yet.
*)
let test_empty _ =
  let empty =
    P2p_acl.create
      ~peer_id_size:10
      ~ip_size:1024
      ~ip_cleanup_delay:(Time.System.Span.of_seconds_exn 60.)
  in
  List.iter
    (fun (_peer, addr) ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.banned_addr empty addr))
    peers ;
  Lwt.return_unit

(** From an empty ACL of size 10, peers [foo], [bar] and [baz] are
    greylisted.
*)
let test_ban _ =
  let set =
    P2p_acl.create
      ~peer_id_size:10
      ~ip_size:1024
      ~ip_cleanup_delay:(Time.System.Span.of_seconds_exn 60.)
  in
  List.iter (fun (_, addr) -> P2p_acl.IPGreylist.add set addr) peers ;
  List.iter
    (fun (_, addr) ->
      assert_equal_bool ~msg:__LOC__ true (P2p_acl.banned_addr set addr))
    peers ;
  Lwt.return_unit

(** From an empty ACL of size 10, peers [peers] are greylisted since
    epoch, then a garbage collection is triggered on the table at the
    [Ptime.max] date (e.g., far in the future).  All point should have
    been removed.
*)
let test_clear _ =
  let set =
    P2p_acl.create
      ~peer_id_size:10
      ~ip_size:1024
      ~ip_cleanup_delay:(Time.System.Span.of_seconds_exn 60.)
  in
  List.iter (fun (_, addr) -> P2p_acl.IPGreylist.add set addr) peers ;
  List.iter
    (fun (_peer, addr) ->
      assert_equal_bool ~msg:__LOC__ true (P2p_acl.banned_addr set addr))
    peers ;
  (* remove all peers *)
  P2p_acl.IPGreylist.clear set ;
  List.iter
    (fun (_peer, addr) ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.banned_addr set addr))
    peers ;
  Lwt.return_unit

(** From an empty ACL of size 10, peers [peers] are greylisted.
    The GC is configured to run 16 times in 0.1 seconds. Since
    16 is hardcoded as the number of GCs to evict a peer from the
    table (see p2p_acl.ml), the table must be empty at the end. *)

(* flaky test, see below *)

let _test_gc _ =
  let open Lwt_syntax in
  let set =
    P2p_acl.create
      ~peer_id_size:10
      ~ip_size:1024
      ~ip_cleanup_delay:(Time.System.Span.of_seconds_exn (0.1 /. 16.))
  in
  List.iter (fun (_, addr) -> P2p_acl.IPGreylist.add set addr) peers ;
  (* 1.0 is an overapproximation of 0.1 :) *)
  let* () = Lwt_unix.sleep 1.0 in
  List.iter
    (fun (_peer, addr) ->
      assert_equal_bool ~msg:__LOC__ false (P2p_acl.banned_addr set addr))
    peers ;
  Lwt.return_unit

let () =
  let init_logs = lazy (Tezos_base_unix.Internal_event_unix.init ()) in
  let wrap (n, f) =
    Alcotest_lwt.test_case n `Quick (fun _ () ->
        let open Lwt_syntax in
        let* () = Lazy.force init_logs in
        f ())
  in
  Alcotest_lwt.run
    "tezos-p2p"
    [
      ( "p2p.peerset",
        List.map
          wrap
          [
            ("empty", test_empty);
            ("ban", test_ban);
            ("clear", test_clear)
            (* FIXME flaky test:
               ("test_gc", test_gc)
            *);
          ] );
    ]
  |> Lwt_main.run
