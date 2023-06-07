type bit = int

module Bit = struct
  type t = {f : unit -> bit option; size : int; mutable i : int}

  let of_bytes_le b =
    let length = Bytes.length b in
    if length = 0 then {f = (fun () -> None); size = 0; i = 0}
    else
      let current_byte_int = ref (Bytes.get_uint8 b 0) in
      let i = ref 0 in
      let j = ref 0 in
      let rec f () : bit option =
        if !i = length - 1 && !j = 8 then None
        else if !j = 8 then (
          i := !i + 1 ;
          current_byte_int := Bytes.get_uint8 b !i ;
          j := 0 ;
          f ())
        else
          let b = !current_byte_int mod 2 in
          j := !j + 1 ;
          current_byte_int := !current_byte_int lsr 1 ;
          Some b
      in
      {f; size = length * 8; i = 0}

  let is_processed {f = _; size; i} = size = i

  let of_bool_list bs =
    let bs_ref = ref bs in
    let length = List.length bs in
    if length = 0 then {f = (fun () -> None); size = length; i = 0}
    else
      let f () =
        match !bs_ref with
        | [] -> None
        | b :: bs ->
            bs_ref := bs ;
            Some (if b then 1 else 0)
      in
      {f; size = length; i = 0}

  let next iterator =
    if is_processed iterator then None
    else
      let res = iterator.f () in
      iterator.i <- iterator.i + 1 ;
      res

  let get_chunk iterator ?(default = 0) n =
    if is_processed iterator then List.init n (fun _ -> default)
    else List.init n (fun _ -> Option.value (next iterator) ~default)
end
