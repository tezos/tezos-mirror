(* This function is meant to be called with accurrate size information. If it is
   called with arguments such that [List.length l <> size] then bad things
   happen. *)
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
