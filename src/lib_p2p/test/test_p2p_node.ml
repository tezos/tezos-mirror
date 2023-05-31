(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Testing
   -------
   Component:    lib_p2p (test lib)
   Invocation:   dune exec src/lib_p2p/test/test_p2p_node.exe
   Subject:      Check p2p test framework.
*)
module Event = struct
  include Internal_event.Simple

  let section = ["test"; "p2p"; "node"]

  let port_conflicts =
    declare_0
      ~section
      ~name:"port_conflicts"
      ~msg:"conflict on ports, retry the test"
      ~level:Warning
      ()
end

type error += Some_error | Invalid_test_result

let () =
  register_error_kind
    `Permanent
    ~id:"test_p2p_node.some_error"
    ~title:"An error use to simulate a failing test."
    ~description:"This error is used to simulate a failing test."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "This error is used to simulate a failing test.")
    Data_encoding.unit
    (function Some_error -> Some () | _ -> None)
    (fun () -> Some_error) ;
  register_error_kind
    `Permanent
    ~id:"test_p2p_node.invalid_test_result"
    ~title:"The result of the test is invalid."
    ~description:"The result of the test is invalid."
    ~pp:(fun ppf () -> Format.fprintf ppf "The result of the test is invalid.")
    Data_encoding.unit
    (function Invalid_test_result -> Some () | _ -> None)
    (fun () -> Invalid_test_result)

(* Test.
   In [propagation_tzresult] a random [node] is selected to fail with an error.
   Then it is checked that the result of [Node.detach_nodes] is an error. *)
let propagation_tzresult points =
  let open Lwt_result_syntax in
  let x = Random.int (List.length points) in
  let*! r =
    Node.detach_nodes
      (fun i _ -> if x = i then tzfail Some_error else return_unit)
      points
  in
  match r with Ok () -> tzfail Invalid_test_result | Error _ -> return_unit

let wrap addr n f =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      let rec aux n f =
        let open Lwt_syntax in
        let points = Node.gen_points 4 addr in
        let* r = f points in
        match r with
        | Ok () -> Lwt.return_unit
        | Error (Exn (Unix.Unix_error ((EADDRINUSE | EADDRNOTAVAIL), _, _)) :: _)
          ->
            let* () = Event.(emit port_conflicts) () in
            aux n f
        | Error error ->
            Format.kasprintf Stdlib.failwith "%a" pp_print_trace error
      in
      aux n f)

let main () =
  let rules =
    match Tezt.Cli.options.log_level with
    | Quiet | Error | Warn | Report -> None
    | Info | Debug -> Some "test.p2p.node -> info;"
  in
  let log_cfg = Lwt_log_sink_unix.create_cfg ?rules () in
  let () =
    Lwt_main.run (Tezos_base_unix.Internal_event_unix.init ~log_cfg ())
  in
  let addr = Node.default_ipv6_addr in
  Lwt_main.run
  @@ Alcotest_lwt.run
       ~__FILE__
       "tezos-p2p"
       [
         ( "p2p-node",
           [
             wrap addr "propagation-tzresult" (fun points ->
                 propagation_tzresult points);
           ] );
       ]

let () =
  Sys.catch_break true ;
  try main () with _ -> ()

let () = Tezt.Test.run ()
