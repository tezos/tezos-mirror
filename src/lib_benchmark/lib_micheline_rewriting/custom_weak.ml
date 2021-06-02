(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* Modified for Nomadic Labs, 2020 *)

let additional_values = 2

module Make_table (H : Hashtbl.HashedType) = struct
  type 'a weak_t = 'a Weak.t

  let weak_create = Weak.create

  let emptybucket = weak_create 0

  type data = H.t

  type t = {
    mutable table : data weak_t array;
    mutable hashes : int array array;
    mutable limit : int;
    (* bucket size limit *)
    mutable oversize : int;
    (* number of oversize buckets *)
    mutable rover : int; (* for internal bookkeeping *)
  }

  let get_index t h = h land max_int mod Array.length t.table

  let limit = 7

  let over_limit = 2

  let create sz =
    let sz = if sz < 7 then 7 else sz in
    let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
    {
      table = Array.make sz emptybucket;
      hashes = Array.make sz [||];
      limit;
      oversize = 0;
      rover = 0;
    }

  let clear t =
    for i = 0 to Array.length t.table - 1 do
      t.table.(i) <- emptybucket ;
      t.hashes.(i) <- [||]
    done ;
    t.limit <- limit ;
    t.oversize <- 0

  let fold f t init =
    let rec fold_bucket i b accu =
      if i >= Weak.length b then accu
      else
        match Weak.get b i with
        | Some v -> fold_bucket (i + 1) b (f v accu)
        | None -> fold_bucket (i + 1) b accu
    in
    Array.fold_right (fold_bucket 0) t.table init

  let iter f t =
    let rec iter_bucket i b =
      if i >= Weak.length b then ()
      else
        match Weak.get b i with
        | Some v ->
            f v ;
            iter_bucket (i + 1) b
        | None -> iter_bucket (i + 1) b
    in
    Array.iter (iter_bucket 0) t.table

  let iter_weak f t =
    let rec iter_bucket i j b =
      if i >= Weak.length b then ()
      else
        match Weak.check b i with
        | true ->
            f b t.hashes.(j) i ;
            iter_bucket (i + 1) j b
        | false -> iter_bucket (i + 1) j b
    in
    Array.iteri (iter_bucket 0) t.table

  let rec count_bucket i b accu =
    if i >= Weak.length b then accu
    else count_bucket (i + 1) b (accu + if Weak.check b i then 1 else 0)

  let count t = Array.fold_right (count_bucket 0) t.table 0

  let next_sz n = min ((3 * n / 2) + 3) Sys.max_array_length

  let prev_sz n = (((n - 3) * 2) + 2) / 3

  let test_shrink_bucket t =
    let bucket = t.table.(t.rover) in
    let hbucket = t.hashes.(t.rover) in
    let len = Weak.length bucket in
    let prev_len = prev_sz len in
    let live = count_bucket 0 bucket 0 in
    if live <= prev_len then (
      let rec loop i j =
        if j >= prev_len then
          if Weak.check bucket i then loop (i + 1) j
          else if Weak.check bucket j then (
            Weak.blit bucket j bucket i 1 ;
            hbucket.(i) <- hbucket.(j) ;
            loop (i + 1) (j - 1))
          else loop i (j - 1)
      in
      loop 0 (Weak.length bucket - 1) ;
      (if prev_len = 0 then (
       t.table.(t.rover) <- emptybucket ;
       t.hashes.(t.rover) <- [||])
      else
        let newbucket = weak_create prev_len in
        Weak.blit bucket 0 newbucket 0 prev_len ;
        t.table.(t.rover) <- newbucket ;
        t.hashes.(t.rover) <- Array.sub hbucket 0 prev_len) ;
      if len > t.limit && prev_len <= t.limit then t.oversize <- t.oversize - 1) ;
    t.rover <- (t.rover + 1) mod Array.length t.table

  let rec resize t =
    let oldlen = Array.length t.table in
    let newlen = next_sz oldlen in
    if newlen > oldlen then (
      let newt = create newlen in
      let add_weak ob oh oi =
        let setter nb ni _ = Weak.blit ob oi nb ni 1 in
        let h = oh.(oi) in
        add_aux newt setter None h (get_index newt h)
      in
      iter_weak add_weak t ;
      t.table <- newt.table ;
      t.hashes <- newt.hashes ;
      t.limit <- newt.limit ;
      t.oversize <- newt.oversize ;
      t.rover <- t.rover mod Array.length newt.table)
    else (
      t.limit <- max_int ;
      (* maximum size already reached *)
      t.oversize <- 0)

  and add_aux t setter d h index =
    let bucket = t.table.(index) in
    let hashes = t.hashes.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then (
        let newsz =
          min ((3 * sz / 2) + 3) (Sys.max_array_length - additional_values)
        in
        if newsz <= sz then failwith "Weak.Make: hash bucket cannot grow more" ;
        let newbucket = weak_create newsz in
        let newhashes = Array.make newsz 0 in
        Weak.blit bucket 0 newbucket 0 sz ;
        Array.blit hashes 0 newhashes 0 sz ;
        setter newbucket sz d ;
        newhashes.(sz) <- h ;
        t.table.(index) <- newbucket ;
        t.hashes.(index) <- newhashes ;
        if sz <= t.limit && newsz > t.limit then (
          t.oversize <- t.oversize + 1 ;
          for _i = 0 to over_limit do
            test_shrink_bucket t
          done) ;
        if t.oversize > Array.length t.table / over_limit then resize t)
      else if Weak.check bucket i then loop (i + 1)
      else (
        setter bucket i d ;
        hashes.(i) <- h)
    in
    loop 0

  let add t d =
    let h = H.hash d in
    add_aux t Weak.set (Some d) h (get_index t h)

  let find_or t d ifnotfound =
    let h = H.hash d in
    let index = get_index t h in
    let bucket = t.table.(index) in
    let hashes = t.hashes.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then ifnotfound h index
      else if h = hashes.(i) then
        match Weak.get_copy bucket i with
        | Some v when H.equal v d -> (
            match Weak.get bucket i with Some v -> v | None -> loop (i + 1))
        | _ -> loop (i + 1)
      else loop (i + 1)
    in
    loop 0

  let merge t d =
    find_or t d (fun h index ->
        add_aux t Weak.set (Some d) h index ;
        d)

  let find t d = find_or t d (fun _h _index -> raise Not_found)

  let find_opt t d =
    let h = H.hash d in
    let index = get_index t h in
    let bucket = t.table.(index) in
    let hashes = t.hashes.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then None
      else if h = hashes.(i) then
        match Weak.get_copy bucket i with
        | Some v when H.equal v d -> (
            match Weak.get bucket i with
            | Some _ as v -> v
            | None -> loop (i + 1))
        | _ -> loop (i + 1)
      else loop (i + 1)
    in
    loop 0

  let find_shadow t d iffound ifnotfound =
    let h = H.hash d in
    let index = get_index t h in
    let bucket = t.table.(index) in
    let hashes = t.hashes.(index) in
    let sz = Weak.length bucket in
    let rec loop i =
      if i >= sz then ifnotfound
      else if h = hashes.(i) then
        match Weak.get_copy bucket i with
        | Some v when H.equal v d -> iffound bucket i
        | _ -> loop (i + 1)
      else loop (i + 1)
    in
    loop 0

  let remove t d = find_shadow t d (fun w i -> Weak.set w i None) ()

  let mem t d = find_shadow t d (fun _w _i -> true) false

  let find_all t d =
    let h = H.hash d in
    let index = get_index t h in
    let bucket = t.table.(index) in
    let hashes = t.hashes.(index) in
    let sz = Weak.length bucket in
    let rec loop i accu =
      if i >= sz then accu
      else if h = hashes.(i) then
        match Weak.get_copy bucket i with
        | Some v when H.equal v d -> (
            match Weak.get bucket i with
            | Some v -> loop (i + 1) (v :: accu)
            | None -> loop (i + 1) accu)
        | _ -> loop (i + 1) accu
      else loop (i + 1) accu
    in
    loop 0 []

  let find_all_by_hash t h =
    let index = get_index t h in
    let bucket = t.table.(index) in
    let hashes = t.hashes.(index) in
    let sz = Weak.length bucket in
    let rec loop i accu =
      if i >= sz then accu
      else if h = hashes.(i) then
        match Weak.get_copy bucket i with
        | Some _ -> (
            match Weak.get bucket i with
            | Some v -> loop (i + 1) (v :: accu)
            | None -> loop (i + 1) accu)
        | _ -> loop (i + 1) accu
      else loop (i + 1) accu
    in
    loop 0 []

  let stats t =
    let len = Array.length t.table in
    let lens = Array.map Weak.length t.table in
    Array.sort compare lens ;
    let totlen = Array.fold_left ( + ) 0 lens in
    (len, count t, totlen, lens.(0), lens.(len / 2), lens.(len - 1))
end
