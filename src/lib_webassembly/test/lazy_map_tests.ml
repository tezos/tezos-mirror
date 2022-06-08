open QCheck_alcotest
open QCheck2
open Lazy_map

let gen_of_list gen_items =
  let open Gen in
  let+ list = gen_items in
  IntMap.of_list list

let gen_create gen_items =
  let open Gen in
  let+ array = gen_items in
  IntMap.create ~produce_value:(fun i -> array.(i)) (Array.length array)

let gen gen_item =
  let open Gen in
  let gen_create =
    oneof [gen_of_list (list gen_item); gen_create (array gen_item)]
  in
  let gen_concat =
    let+ lhs = gen_create and+ rhs = gen_create in
    IntMap.concat lhs rhs
  in
  let gen_base_or_concat = oneof [gen_create; gen_concat] in
  let gen_cons =
    let+ prefix = small_list gen_item and+ map = gen_base_or_concat in
    List.fold_left (fun map prefix -> IntMap.cons prefix map) map prefix
  in
  oneof [gen_base_or_concat; gen_cons]

let of_list_constructs_correctly =
  Test.make
    ~name:"of_list creates the data structure correctly"
    Gen.(list int)
    (fun items ->
      let map = IntMap.of_list items in
      let checked = List.mapi (fun i v -> IntMap.get i map = v) items in
      List.for_all Fun.id checked && IntMap.num_elements map = List.length items)

let create_constructs_correctly =
  Test.make ~name:"create constructs correctly" Gen.nat (fun len ->
      let map = IntMap.create ~produce_value:Fun.id len in
      List.init len (fun i -> IntMap.get i map = i) |> List.for_all Fun.id)

let grow_works =
  Test.make
    ~name:"grow works"
    Gen.(pair (gen int) nat)
    (fun (map, len) ->
      let map2 = IntMap.grow ~produce_value:(fun x -> x * 2) len map in
      let check1 =
        List.init (IntMap.num_elements map) (fun i ->
            IntMap.get i map2 = IntMap.get i map)
        |> List.for_all Fun.id
      in
      let check2 =
        List.init len (fun i ->
            let key = i + IntMap.num_elements map in
            IntMap.get key map2 = i * 2)
        |> List.for_all Fun.id
      in
      let check3 = IntMap.num_elements map + len = IntMap.num_elements map2 in
      check1 && check2 && check3)

let cons_works =
  Test.make
    ~name:"cons works"
    Gen.(pair (gen int) int)
    (fun (map, value) ->
      let map2 = IntMap.cons value map in
      let check1 = IntMap.get 0 map2 = value in
      let check2 =
        List.init (IntMap.num_elements map) (fun i ->
            IntMap.get i map = IntMap.get (i + 1) map2)
        |> List.for_all Fun.id
      in
      check1 && check2)

let concat_works () =
  let map1 =
    IntMap.create
      ~produce_value:(fun x ->
        Printf.printf "> map1: %i\n%!" x ;
        Int.succ x)
      1
    |> IntMap.cons 10
  in
  let map2 =
    IntMap.create
      ~produce_value:(fun x ->
        Printf.printf "> map2: %i\n%!" x ;
        Int.pred x)
      1
    |> IntMap.cons 20
  in
  let map = IntMap.concat map1 map2 in
  let open Alcotest in
  check int "exact value" 4 (IntMap.num_elements map) ;
  check int "exact value" 10 (IntMap.get 0 map) ;
  check int "exact value" 1 (IntMap.get 1 map) ;
  check int "exact value" 20 (IntMap.get 2 map) ;
  check int "exact value" (-1) (IntMap.get 3 map)

let tests =
  [
    to_alcotest of_list_constructs_correctly;
    to_alcotest create_constructs_correctly;
    to_alcotest grow_works;
    to_alcotest cons_works;
    ("concat works lazily", `Quick, concat_works);
  ]
