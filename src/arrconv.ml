let array_of_list_size l size =
  match l with
  | [] ->
      assert (size = 0) ;
      [||]
  | h :: _ ->
      assert (size > 0) ;
      let arr = Array.make size h in
      List.iteri (Array.unsafe_set arr) l ;
      arr
