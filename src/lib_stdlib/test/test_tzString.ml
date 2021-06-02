(* Verify the default behavior of split is handling multiple instances of the
   separator in a row
*)
let test_split_duplicated_separator_default_behavior () =
  let inputs =
    [
      "Hello World";
      "Hello  World";
      "HelloWorld";
      "Hello                       World  ";
      "  Hello                       World  ";
      "  Hello                       World";
    ]
  in
  List.iter
    (fun s -> assert (TzString.split ?dup:None ' ' s = TzString.split ' ' s))
    inputs

let test_split_dup_param_to_true_with_duplicated_separator () =
  let inputs =
    [
      "Hello World";
      "Hello  World";
      "Hello                       World  ";
      "  Hello                       World  ";
      "  Hello                       World";
    ]
  in
  List.iter
    (fun s -> assert (["Hello"; "World"] = TzString.split ~dup:true ' ' s))
    inputs

let test_split_dup_param_to_false_with_duplicated_separator () =
  let inputs =
    [
      ("Hello World", ["Hello"; "World"]);
      ("Hello  World", ["Hello"; ""; "World"]);
      ("Hello  World  ", ["Hello"; ""; "World"; ""]);
      ("HelloWorld", ["HelloWorld"]);
      ("  Hello World  ", [""; "Hello"; "World"; ""]);
      ("  Hello World", [""; "Hello"; "World"]);
      ("  hello world  ", [""; "hello"; "world"; ""]);
    ]
  in
  List.iter
    (fun (input, expected_output) ->
      assert (expected_output = TzString.split ~dup:false ' ' input))
    inputs

let test_split_roundtrip () =
  (* NOTE: the property only holds if there are no sep at the beginning / ending
     of the string *)
  let inputs =
    [
      "HelloWorld";
      "Hello World";
      "Hello  World";
      "H  e  l    l o  W    o rl  d";
      "";
    ]
  in
  List.iter
    (fun input ->
      let split = TzString.split ~dup:false ' ' input in
      let reassembled = String.concat " " split in
      assert (input = reassembled))
    inputs

let split_tests =
  [
    ( "Default behavior handles a duplicated separator",
      `Quick,
      test_split_duplicated_separator_default_behavior );
    ( "dup parameter to true",
      `Quick,
      test_split_dup_param_to_true_with_duplicated_separator );
    ( "dup parameter to false",
      `Quick,
      test_split_dup_param_to_false_with_duplicated_separator );
    ("roundtrip", `Quick, test_split_roundtrip);
  ]

let () = Alcotest.run "TzString" [("split", split_tests)]
