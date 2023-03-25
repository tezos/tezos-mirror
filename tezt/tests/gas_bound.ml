(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file gas_bound.ml
   Subject:      Gas bound
*)

let first_explosion =
  {| 
parameter unit; 
storage unit; 
code{ 
      DROP; PUSH nat 0 ; 
      DUP ; PAIR ; DUP ; 
      PAIR ; DUP ; PAIR ; 
      DUP ; PAIR ; DUP ; 
      PAIR ; DUP ; PAIR ; 
      DUP ; PAIR ; DUP ; 
      PAIR ; DROP ; UNIT ; 
      NIL operation ; PAIR}; 
|}

let first_explosion_bigtype =
  {| 
parameter unit; 
storage unit; 
code{ 
      DROP; PUSH nat 0 ; DUP ; 
      PAIR ; DUP ; PAIR ; DUP ; 
      PAIR ; DUP ; PAIR ; DUP ; 
      PAIR ; DUP ; PAIR ; DUP ; 
      PAIR ; DUP ; PAIR ; DUP ; 
      PAIR ; DUP ; PAIR ; DROP ; 
      UNIT ; NIL operation ; PAIR}; 
|}

let second_explosion =
  {| 
parameter (list int) ;
storage (list (list (list int))) ;
code { CAR ; DIP { NIL (list int) } ;
       DUP ; ITER { DROP ; DUP ; DIP { CONS } } ;
       DROP ; DIP { NIL (list (list int)) } ;
       DUP ; ITER { DROP ; DUP ; DIP { CONS } } ;
       DROP ; NIL operation ; PAIR }; 
|}

let test_originate_first_explosion client protocol () =
  let expected_msg =
    rex "Gas limit exceeded during typechecking or execution"
  in
  let* () = Client.typecheck_script ~script:first_explosion client in
  let gas_limit =
    match protocol with
    | Protocol.Nairobi | Alpha -> 645
    | Lima | Mumbai -> 1479
  in
  let process =
    Client.spawn_originate_contract
      ~alias:"first_explosion"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:first_explosion
      ~init:"Unit"
      ~burn_cap:(Tez.of_int 10)
      ~gas_limit
      client
  in
  let* () = Process.check_error ~exit_code:1 ~msg:expected_msg process in
  unit

let test_typecheck_script_big_type client _protocol () =
  let expected_msg = rex "type exceeded maximum type size" in
  let process =
    Client.spawn_typecheck_script ~script:first_explosion_bigtype client
  in
  let _ = Process.check_error ~exit_code:1 ~msg:expected_msg process in
  unit

let test_run_script_second_explosion client _protocol () =
  let* _storage =
    Client.run_script
      ~prg:second_explosion
      ~storage:"{}"
      ~input:"{1;2;3;4;5;6;7;8;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}"
      client
  in
  unit

let test_run_script_second_explosion_fail client _protocol () =
  let input =
    "{1;2;3;4;5;6;7;8;9;0;1;2;3;4;5;6;7;1;1;1;1;1;1;1;1;1;1;1"
    ^ ";1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1}"
  in
  let process =
    Client.spawn_run_script
      ~prg:second_explosion
      ~storage:"{}"
      ~gas:9290
      ~input
      client
  in
  let msg =
    rex
      ("Cannot serialize the resulting storage"
     ^ " value within the provided gas bounds.")
  in
  let _ = Process.check_error ~exit_code:1 ~msg process in
  unit

let assert_typecheck_data_failure client ~data ~typ ~msg =
  let process = Client.spawn_typecheck_data ~data ~typ client in
  Process.check_error ~msg process

let test_typecheck_map_dup_key client _protocol () =
  let msg =
    rex
      ("Map literals cannot contain duplicate"
     ^ " keys, however a duplicate key was found")
  in
  let* _ =
    assert_typecheck_data_failure
      client
      ~data:"{ Elt 0 1 ; Elt 0 1}"
      ~typ:"(map nat nat)"
      ~msg
  in
  unit

let test_typecheck_map_bad_ordering client _protocol () =
  let msg =
    rex
      ("Keys in a map literal must be in strictly"
     ^ " ascending order, but they were unordered in literal")
  in
  let* _ =
    assert_typecheck_data_failure
      client
      ~data:"{ Elt 0 1 ; Elt 10 1 ; Elt 5 1 }"
      ~typ:"(map nat nat)"
      ~msg
  in
  unit

let test_typecheck_set_bad_ordering client _protocol () =
  let msg =
    rex
      ("Values in a set literal must be in strictly"
     ^ " ascending order, but they were unordered in literal")
  in
  let* _ =
    assert_typecheck_data_failure
      client
      ~data:"{ \"A\" ; \"C\" ; \"B\" }"
      ~typ:"(set string)"
      ~msg
  in
  unit

let test_typecheck_set_no_duplicates client _protocol () =
  let msg =
    rex
      ("Set literals cannot contain duplicate values,"
     ^ " however a duplicate value was found")
  in
  let* _ =
    assert_typecheck_data_failure
      client
      ~data:"{ \"A\" ; \"B\" ; \"B\" }"
      ~typ:"(set string)"
      ~msg
  in
  unit

let register ~protocols =
  List.iter
    (fun (title, test_function) ->
      Protocol.register_test
        ~__FILE__
        ~title
        ~tags:["client"; "michelson"; "gas_bound"]
        (fun protocol ->
          let* client = Client.init_mockup ~protocol () in
          test_function client protocol ())
        protocols)
    [
      ("first explosion", test_originate_first_explosion);
      ("first explosion big type", test_typecheck_script_big_type);
      ("second explosion", test_run_script_second_explosion);
      ("second explosion fail", test_run_script_second_explosion_fail);
      ("typecheck map dup key", test_typecheck_map_dup_key);
      ("typecheck map bad ordering", test_typecheck_map_bad_ordering);
      ("typecheck set bad ordering", test_typecheck_set_bad_ordering);
      ("typecheck set no duplicates", test_typecheck_set_no_duplicates);
    ]
