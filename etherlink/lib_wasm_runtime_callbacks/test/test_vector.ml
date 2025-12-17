(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Wasm_runtime_callbacks.Internal_for_tests

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

let register ?(tags = []) =
  Test.register
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
    ~tags:("vector" :: tags)

let empty_tree = !(Irmin_context.PVMState.empty ())

let expect_error msg = function Ok _ -> Test.fail msg | Error err -> err

let expect_ok msg = function Ok res -> res | Error _ -> Test.fail msg

let test_vector_get () =
  register ~title:"test vector get" @@ fun () ->
  let* res = Vector.get empty_tree ["test"] in
  let _ =
    expect_error
      "It should not be possible to fetch a vector from an empty tree"
      res
  in

  let* res = Vector.get empty_tree ~create_if_absent:true ["test"] in
  let vector =
    expect_ok
      "It should be possible to fetch a vector from the empty tree if \
       [create_if_absent] is [true]"
      res
  in

  let* len = Vector.length vector in
  let len =
    expect_ok "We should be able to fetch the length of the vector" len
  in
  Check.((len = 0) int ~error_msg:"Vector should be empty") ;

  unit

let ( !! ) = Bytes.unsafe_of_string

let test_vector_write_bytes () =
  register ~title:"test vector write_bytes" @@ fun () ->
  let* res = Vector.get empty_tree ~create_if_absent:true ["test"] in
  let vector =
    expect_ok
      "It should be possible to fetch a vector from the empty tree if \
       [create_if_absent] is [true]"
      res
  in

  let* res = Vector.write_bytes vector 10 !!"Hello, world!" in
  let _ =
    expect_error
      "We should not be able to write at offset 10 on a newly created vector"
      res
  in

  let* res = Vector.write_bytes vector 0 !!"Hello, world!" in
  let vector =
    expect_ok
      "We should be able to write at offset 0 on a newly created vector"
      res
  in

  let* res = Vector.load_all vector in
  Check.(
    (res = "Hello, world!")
      string
      ~error_msg:"After writing to the vector, expected %R, got %L") ;

  let* res = Vector.write_bytes vector 12 !!lorem_ipsum in
  let vector =
    expect_ok
      "We should be able to write at offset 12 after writing “Hello, world!”"
      res
  in

  let* res = Vector.load_all vector in
  Check.(
    (res = sf "Hello, world%s" lorem_ipsum)
      string
      ~error_msg:"After writing to the vector, expected %R, got %L") ;

  unit

let test_vector_load_bytes () =
  register ~title:"test vector load_bytes" @@ fun () ->
  let* res = Vector.get empty_tree ~create_if_absent:true ["test"] in
  let vector =
    expect_ok
      "It should be possible to fetch a vector from the empty tree if \
       [create_if_absent] is [true]"
      res
  in

  let* res = Vector.load_bytes vector ~offset:12 ~num_bytes:10 in
  let _ =
    expect_error
      "We should not be able to read at offset 12 on a newly created vector"
      res
  in

  let* res = Vector.write_bytes vector 0 !!lorem_ipsum in
  let vector =
    expect_ok
      "We should be able to write at offset 0 on a newly created vector"
      res
  in

  let* res = Vector.load_bytes vector ~offset:0 ~num_bytes:15 in
  let res =
    expect_ok
      "We should be able to read at offset 0 aftrer writing a lorem ipsum"
      res
  in
  Check.(
    (Bytes.unsafe_to_string res = String.sub lorem_ipsum 0 15)
      string
      ~error_msg:"After writing to the vector, expected %R, got %L") ;

  let* res = Vector.load_bytes vector ~offset:12 ~num_bytes:1100 in
  let res =
    expect_ok
      "We should be able to write at offset 12 after writing a lorem ipsum"
      res
  in
  Check.(
    (Bytes.unsafe_to_string res = String.sub lorem_ipsum 12 1100)
      string
      ~error_msg:"After writing to the vector, expected %R, got %L") ;

  unit

let () =
  test_vector_get () ;
  test_vector_write_bytes () ;
  test_vector_load_bytes ()
