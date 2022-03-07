(* Verify the default behavior of split is handling multiple instances of the
   separator in a row
*)
let test_split_duplicated_separator () =
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
    (fun s -> assert (TzString.split ' ' s = TzString.split ' ' s))
    inputs

let split_tests =
  [("handles a duplicated separator", `Quick, test_split_duplicated_separator)]

let () = Alcotest.run "TzString" [("split", split_tests)]
