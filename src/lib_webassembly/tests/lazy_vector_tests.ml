open QCheck_alcotest
open QCheck2
open Lazy_vector

let gen_of_list gen_items =
  let open Gen in
  let+ list = gen_items in
  IntVector.of_list list

let gen_create gen_items =
  let open Gen in
  let+ array = gen_items in
  IntVector.create ~produce_value:(fun i -> array.(i)) (Array.length array)

let gen gen_item =
  let open Gen in
  let gen_create =
    oneof [gen_of_list (list gen_item); gen_create (array gen_item)]
  in
  let gen_concat =
    let+ lhs = gen_create and+ rhs = gen_create in
    IntVector.concat lhs rhs
  in
  let gen_base_or_concat = oneof [gen_create; gen_concat] in
  let gen_cons =
    let+ prefix = small_list gen_item and+ map = gen_base_or_concat in
    List.fold_left (fun map prefix -> IntVector.cons prefix map) map prefix
  in
  oneof [gen_base_or_concat; gen_cons]

let of_list_constructs_correctly =
  Test.make
    ~name:"of_list creates the data structure correctly"
    Gen.(list int)
    (fun items ->
      let map = IntVector.of_list items in
      let checked = List.mapi (fun i v -> IntVector.get i map = v) items in
      List.for_all Fun.id checked
      && IntVector.num_elements map = List.length items)

let create_constructs_correctly =
  Test.make ~name:"create constructs correctly" Gen.nat (fun len ->
      let map = IntVector.create ~produce_value:Fun.id len in
      List.init len (fun i -> IntVector.get i map = i) |> List.for_all Fun.id)

let grow_works =
  Test.make
    ~name:"grow works"
    Gen.(pair (gen int) nat)
    (fun (map, len) ->
      let map2 = IntVector.grow ~produce_value:(fun x -> x * 2) len map in
      let check1 =
        List.init (IntVector.num_elements map) (fun i ->
            IntVector.get i map2 = IntVector.get i map)
        |> List.for_all Fun.id
      in
      let check2 =
        List.init len (fun i ->
            let key = i + IntVector.num_elements map in
            IntVector.get key map2 = i * 2)
        |> List.for_all Fun.id
      in
      let check3 =
        IntVector.num_elements map + len = IntVector.num_elements map2
      in
      check1 && check2 && check3)

let cons_works =
  Test.make
    ~name:"cons works"
    Gen.(pair (gen int) int)
    (fun (map, value) ->
      let map2 = IntVector.cons value map in
      let check1 = IntVector.get 0 map2 = value in
      let check2 =
        List.init (IntVector.num_elements map) (fun i ->
            IntVector.get i map = IntVector.get (i + 1) map2)
        |> List.for_all Fun.id
      in
      check1 && check2)

let concat_works () =
  let map1 =
    IntVector.create
      ~produce_value:(fun x ->
        Printf.printf "> map1: %i\n%!" x ;
        Int.succ x)
      1
    |> IntVector.cons 10
  in
  let map2 =
    IntVector.create
      ~produce_value:(fun x ->
        Printf.printf "> map2: %i\n%!" x ;
        Int.pred x)
      1
    |> IntVector.cons 20
  in
  let map = IntVector.concat map1 map2 in
  let open Alcotest in
  check int "exact value" 4 (IntVector.num_elements map) ;
  check int "exact value" 10 (IntVector.get 0 map) ;
  check int "exact value" 1 (IntVector.get 1 map) ;
  check int "exact value" 20 (IntVector.get 2 map) ;
  check int "exact value" (-1) (IntVector.get 3 map)

let tests =
  [
    to_alcotest of_list_constructs_correctly;
    to_alcotest create_constructs_correctly;
    to_alcotest grow_works;
    to_alcotest cons_works;
    ("concat works lazily", `Quick, concat_works);
  ]
