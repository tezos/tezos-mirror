let sf = Printf.sprintf

let echo x = Printf.ksprintf print_endline x

let run ~recursive ~filter ~paths =
  let tests = Record.input ~recursive paths in
  let tests =
    match filter with
    | None -> tests
    | Some filter -> List.filter (Record.matches filter) tests
  in

  let test_count = List.length tests in
  let total_duration =
    List.fold_left
      (fun acc test -> Int64.add acc (Record.duration_ns test))
      0L
      tests
  in
  let seconds ns = Int64.to_float ns /. 1_000_000. in
  let minutes ns = seconds ns /. 60. in
  let hours ns = minutes ns /. 60. in

  let top count ~by ~cmp ~get ~show =
    echo "Top %i (by %s):" count by ;
    let tests = List.sort (fun a b -> cmp (get b) (get a)) tests in
    let rec list index = function
      | [] -> ()
      | (head : Record.test) :: tail ->
          echo "%d) %s (%s)" index head.title (show (get head)) ;
          if index < count then list (index + 1) tail
    in
    list 1 tests
  in

  echo "test_count = %d" test_count ;
  echo
    "total_duration = %.3f seconds = %.2f minutes = %.2f hours"
    (seconds total_duration)
    (minutes total_duration)
    (hours total_duration) ;
  echo
    "average_duration = %.3f seconds"
    (seconds total_duration /. float test_count) ;
  echo "" ;
  top
    10
    ~by:"duration"
    ~cmp:Int64.compare
    ~get:Record.duration_ns
    ~show:(fun duration -> sf "%.2f minutes" (minutes duration))
