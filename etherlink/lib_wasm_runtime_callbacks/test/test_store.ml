(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Wasm_runtime_callbacks.Internal_for_tests

let error_code = Check.(convert (fun (i : Error_code.t) -> (i :> int)) int)

let ( !! ) = Bytes.unsafe_of_string

let lorem_ipsum =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur in \
   posuere turpis, imperdiet efficitur velit. Proin eu ultricies mi. Praesent \
   lacinia lectus eget odio ultrices, ac aliquam eros tincidunt. Lorem ipsum \
   dolor sit amet, consectetur adipiscing elit. Nullam vel turpis sapien. \
   Nulla facilisi. Sed sed tempor arcu, quis finibus orci. Ut id sapien felis. \
   Pellentesque semper enim sed turpis placerat gravida. Nullam sagittis at \
   est et dignissim. In vehicula libero a ipsum commodo lobortis. Aliquam \
   rutrum ornare elit, varius fringilla erat volutpat nec.\n\n\
   Nulla quis ipsum sed justo fermentum dictum. Morbi vehicula augue interdum \
   suscipit rutrum. Sed at leo ipsum. Fusce non lectus odio. Pellentesque a \
   eros egestas, sagittis felis scelerisque, auctor eros. Duis velit est, \
   ornare sed fermentum id, cursus id leo. Pellentesque dignissim fermentum \
   ipsum, sit amet auctor augue consequat et. Nulla facilisi. Cras mattis urna \
   magna, non aliquet diam finibus sit amet. Suspendisse at justo sit amet \
   nibh efficitur imperdiet at in leo. Cras posuere interdum ultricies. Aenean \
   sed laoreet felis, at pharetra tortor. Quisque mattis sagittis nisl eu \
   laoreet. Aenean volutpat ante et lacus eleifend, sit amet finibus quam \
   molestie. Nam magna eros, rutrum et leo vel, vestibulum accumsan nunc."

let hello_world = "hello, world!"

let register ?(tags = []) =
  Test.register
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
    ~tags:("store" :: tags)

let expect_error msg = function Ok _ -> Test.fail msg | Error err -> err

let expect_ok msg = function Ok res -> res | Error _ -> Test.fail msg

let run f = Lwt_preemptive.detach f ()

let init_durable_storage l =
  let rec set_next_key tree = function
    | [] -> return tree
    | (key, value) :: rst ->
        let* tree =
          run @@ fun () -> store_write tree key 0 Bytes.(unsafe_of_string value)
        in
        let tree, _ =
          expect_ok "Could not initialize the durable storage" tree
        in
        set_next_key tree rst
  in
  set_next_key (Irmin_context.Tree.empty ()) l

let test_store_has () =
  register ~tags:["store_has"] ~title:"store_has host function" @@ fun () ->
  let* tree = init_durable_storage [("/test/a", ""); ("/test/a/b", "")] in

  let* res = run @@ fun () -> store_has tree "/test/c" in
  let res = expect_ok "should be able to call store_has" res in
  Check.((res = 0) int ~error_msg:"Expected %R (unknown key), got %L") ;

  let* res = run @@ fun () -> store_has tree "/test/a/b" in
  let res = expect_ok "should be able to call store_has" res in
  Check.((res = 1) int ~error_msg:"Expected %R (only value), got %L") ;

  let* res = run @@ fun () -> store_has tree "/test" in
  let res = expect_ok "should be able to call store_has" res in
  Check.((res = 2) int ~error_msg:"Expected %R (directory), got %L") ;

  let* res = run @@ fun () -> store_has tree "/test/a" in
  let res = expect_ok "should be able to call store_has" res in
  Check.((res = 3) int ~error_msg:"Expected %R (directories and value), got %L") ;

  unit

let test_mem_tree () =
  register ~tags:["mem_tree"] ~title:"mem_tree (store_exists)" @@ fun () ->
  let* tree = init_durable_storage [("/test/a", ""); ("/test/a/b", "")] in

  let* res = run @@ fun () -> mem_tree tree "/test/c" in
  let c_exists = expect_ok "should be able to call mem_tree" res in
  Check.((c_exists = false) bool ~error_msg:"/test/c does not exist") ;

  let* res = run @@ fun () -> mem_tree tree "/test/a/b" in
  let b_exists = expect_ok "should be able to call mem_tree" res in
  Check.((b_exists = true) bool ~error_msg:"/test/a/b exists") ;

  let* res = run @@ fun () -> mem_tree tree "/test/a" in
  let a_exists = expect_ok "should be able to call mem_tree" res in
  Check.((a_exists = true) bool ~error_msg:"/test/a exists") ;

  unit

let test_store_copy () =
  register
    ~tags:["store_copy"; "mem_tree"; "get_hash"]
    ~title:"store_copy host function"
  @@ fun () ->
  let* tree = init_durable_storage [("/test/a", "")] in

  let* tree = run @@ fun () -> store_copy tree "/test/a" "/test/b" in
  let tree = expect_ok "should be able to copy /test/a" tree in

  let* res = run @@ fun () -> mem_tree tree "/test/b" in
  let b_exists = expect_ok "should be able to call mem_tree" res in
  Check.((b_exists = true) bool ~error_msg:"/test/b exists") ;

  let* res = run @@ fun () -> store_get_hash tree "/test/a" in
  let a_hash = expect_ok "should be able to call store_get_hash" res in
  let* res = run @@ fun () -> store_get_hash tree "/test/b" in
  let b_hash = expect_ok "should be able to call store_get_hash" res in
  Check.(
    (Bytes.to_string a_hash = Bytes.to_string b_hash)
      string
      ~error_msg:"/test/b is not equal to /test/a") ;

  let* tree = run @@ fun () -> store_copy tree "/test/c" "/test/b" in
  let error = expect_error "should be able to copy /test/a" tree in

  Check.(
    (error = Error_code.store_not_a_node)
      error_code
      ~error_msg:"Should have failed with error code %R (got %L)") ;

  unit

let test_store_move () =
  register
    ~tags:["store_move"; "mem_tree"; "get_hash"]
    ~title:"store_move host function"
  @@ fun () ->
  let* tree = init_durable_storage [("/test/a", "")] in

  let* res = run @@ fun () -> store_get_hash tree "/test/a" in
  let a_hash = expect_ok "should be able to call store_get_hash" res in

  let* tree = run @@ fun () -> store_move tree "/test/a" "/test/b" in
  let tree = expect_ok "should be able to move /test/a" tree in

  let* res = run @@ fun () -> mem_tree tree "/test/a" in
  let a_exists = expect_ok "should be able to call mem_tree" res in
  Check.((a_exists = false) bool ~error_msg:"/test/a should not exist anymore") ;

  let* res = run @@ fun () -> mem_tree tree "/test/b" in
  let b_exists = expect_ok "should be able to call mem_tree" res in
  Check.((b_exists = true) bool ~error_msg:"/test/b exists") ;

  let* res = run @@ fun () -> store_get_hash tree "/test/b" in
  let b_hash = expect_ok "should be able to call store_get_hash" res in
  Check.(
    (Bytes.to_string a_hash = Bytes.to_string b_hash)
      string
      ~error_msg:"/test/b is not equal to former /test/a") ;

  let* tree = run @@ fun () -> store_move tree "/test/c" "/test/b" in
  let error = expect_error "should be able to move /test/a" tree in

  Check.(
    (error = Error_code.store_not_a_node)
      error_code
      ~error_msg:"Should have failed with error code %R (got %L)") ;

  unit

let test_delete () =
  register
    ~tags:["store_delete"; "mem_tree"]
    ~title:"store_delete host function"
  @@ fun () ->
  let* tree = init_durable_storage [("/test/a", "")] in

  let* res = run @@ fun () -> mem_tree tree "/test/a" in
  let a_exists = expect_ok "should be able to test if /test/a exists" res in
  Check.((a_exists = true) bool ~error_msg:"/test/a should exist") ;

  let* res = run @@ fun () -> store_delete tree "/test" false in
  let tree_test_deleted = expect_ok "should be able to delete /test" res in

  let* res = run @@ fun () -> mem_tree tree_test_deleted "/test/a" in
  let a_exists = expect_ok "should be able to test if /test/a exists" res in
  Check.((a_exists = false) bool ~error_msg:"/test/a should not exist anymore") ;

  let* res = run @@ fun () -> mem_tree tree_test_deleted "/test" in
  let test_exists = expect_ok "should be able to test if /test/a exists" res in
  Check.((test_exists = false) bool ~error_msg:"/test should not exist anymore") ;

  unit

let test_store_value_size () =
  register
    ~tags:["store_value_size"; "store_write"]
    ~title:"store_value_size host function"
  @@ fun () ->
  let* tree = init_durable_storage [("/test/a", hello_world)] in

  let* res = run @@ fun () -> store_value_size tree "/test/a" in
  let size = expect_ok "should be able to test if /test/a exists" res in
  Check.(
    (size = String.length hello_world)
      int
      ~error_msg:"/test/a is %R character long, got %L") ;

  let* res = run @@ fun () -> store_write tree "/test/a" 0 !!lorem_ipsum in
  let tree, len = expect_ok "should be able to write in /test/a" res in

  let* res = run @@ fun () -> store_value_size tree "/test/a" in
  let size = expect_ok "should be able to get the size of /test/a" res in
  Check.((size = len) int ~error_msg:"/test/a is %R character long, got %L") ;

  let* res = run @@ fun () -> store_value_size tree "/test/b" in
  let error = expect_error "should not be able fetch /test/b’s size" res in
  Check.(
    (error = Error_code.store_not_a_value)
      error_code
      ~error_msg:"store_value_size should have failed with %R (got %L)") ;

  let* res = run @@ fun () -> store_value_size tree "/test" in
  let error = expect_error "should not be able to fetch /test’s size" res in
  Check.(
    (error = Error_code.store_not_a_value)
      error_code
      ~error_msg:"store_value_size should have failed with %R (got %L)") ;

  unit

let test_store_list_size () =
  register
    ~tags:["store_list_size"; "store_delete"; "store_write"]
    ~title:"store_list_size host function"
  @@ fun () ->
  let* tree =
    init_durable_storage [("/test/a", ""); ("/test/b", ""); ("/test/c", "")]
  in

  let* res = run @@ fun () -> store_list_size tree "/test" in
  let size =
    expect_ok "should be able to count the number of children /test" res
  in
  Check.((size = 3) int ~error_msg:"/test should have %R children, got %L") ;

  let* res = run @@ fun () -> store_delete tree "/test/a" false in
  let tree = expect_ok "should be able to delete in /test/a" res in

  let* res = run @@ fun () -> store_list_size tree "/test" in
  let size =
    expect_ok "should be able to count the number of children /test" res
  in
  Check.((size = 2) int ~error_msg:"/test should have %R children, got %L") ;

  let* res = run @@ fun () -> store_write tree "/test/d" 0 Bytes.empty in
  let tree, _ = expect_ok "should be able to write in /test/d" res in

  let* res = run @@ fun () -> store_list_size tree "/test" in
  let size =
    expect_ok "should be able to count the number of children /test" res
  in
  Check.((size = 3) int ~error_msg:"/test should have %R children, got %L") ;

  let* res = run @@ fun () -> store_list_size tree "/test/foo" in
  let size =
    expect_ok
      "should be able to count the number of children /test/foo even if it \
       does not exist"
      res
  in
  Check.((size = 0) int ~error_msg:"/test/foo should have %R children, got %L") ;

  unit

let test_check_reboot_flag () =
  register
    ~tags:["mem_tree"; "store_write"]
    ~title:"check_reboot_flag helper function"
  @@ fun () ->
  let* tree = init_durable_storage [("/kernel/env/reboot", "")] in

  let* needs_reboot, tree = run @@ fun () -> check_reboot_flag tree in

  Check.(
    (needs_reboot = true)
      bool
      ~error_msg:"Incorrect result from check_reboot_flag (expected %R)") ;

  let* needs_reboot, tree = run @@ fun () -> check_reboot_flag tree in

  Check.(
    (needs_reboot = false)
      bool
      ~error_msg:"Incorrect result from check_reboot_flag (expected %R)") ;

  let* res =
    run @@ fun () -> store_write tree "/kernel/env/reboot" 0 Bytes.empty
  in
  let tree, _ = expect_ok "should be able to write the reboot flag" res in

  let* needs_reboot, _tree = run @@ fun () -> check_reboot_flag tree in

  Check.(
    (needs_reboot = true)
      bool
      ~error_msg:"Incorrect result from check_reboot_flag (expected %R)") ;

  unit

let () =
  test_store_has () ;
  test_mem_tree () ;
  test_store_copy () ;
  test_store_move () ;
  test_delete () ;
  test_store_value_size () ;
  test_store_list_size () ;
  test_check_reboot_flag ()
