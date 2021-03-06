(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  type ('content, 'ptr) cell

  val pp :
    pp_content:(Format.formatter -> 'content -> unit) ->
    pp_ptr:(Format.formatter -> 'ptr -> unit) ->
    Format.formatter ->
    ('content, 'ptr) cell ->
    unit

  val equal :
    ('content -> 'content -> bool) ->
    ('ptr -> 'ptr -> bool) ->
    ('content, 'ptr) cell ->
    ('content, 'ptr) cell ->
    bool

  val encoding :
    'ptr Data_encoding.t ->
    'content Data_encoding.t ->
    ('content, 'ptr) cell Data_encoding.t

  val index : (_, _) cell -> int

  val content : ('content, 'ptr) cell -> 'content

  val back_pointer : ('content, 'ptr) cell -> int -> 'ptr option

  val back_pointers : ('content, 'ptr) cell -> 'ptr list

  val genesis : 'content -> ('content, 'ptr) cell

  val next :
    prev_cell:('content, 'ptr) cell ->
    prev_cell_ptr:'ptr ->
    'content ->
    ('content, 'ptr) cell

  val back_path :
    deref:('ptr -> ('content, 'ptr) cell option) ->
    cell_ptr:'ptr ->
    target_index:int ->
    'ptr list option

  val valid_back_path :
    equal_ptr:('ptr -> 'ptr -> bool) ->
    deref:('ptr -> ('content, 'ptr) cell option) ->
    cell_ptr:'ptr ->
    target_ptr:'ptr ->
    'ptr list ->
    bool

  val search :
    deref:('ptr -> ('content, 'ptr) cell option) ->
    compare:('content -> int Lwt.t) ->
    cell_ptr:'ptr ->
    'ptr list option Lwt.t
end

module Make (Parameters : sig
  val basis : int
end) : S = struct
  let () = assert (Compare.Int.(Parameters.basis >= 2))

  open Parameters

  (*

      A cell of a skip list with some [`content] and back pointers of
      type [`ptr].

      Invariants
      ----------

      - back_pointers[i]
        = Some (pointer to (index - (index mod (basis ** i)) - 1))
          (for all i < length back_pointers)
       - length back_pointers = log basis index

      Notes
      -----

     - The [index] field is not strictly required but helps in making
       the data structure more robust. Indeed, otherwise, we should
       also ask the client to provide the index of the cell to be
       built, which can be error-prone.

     - The back pointers of a cell are chosen from the back pointers of
       its predecessor (except for the genesis cell) and a pointer to this
       predecessor. This locality makes the insertion of new cell very
       efficient in practice.

  *)
  type ('content, 'ptr) cell = {
    content : 'content;
    back_pointers : 'ptr option FallbackArray.t;
    index : int;
  }

  let equal equal_content equal_ptr cell1 cell2 =
    let equal_back_pointers b1 b2 =
      let open FallbackArray in
      Compare.Int.(length b1 = length b2)
      && fst
         @@ fold
              (fun (equal, i) h1 ->
                (equal && Option.equal equal_ptr h1 (get b2 i), i + 1))
              b1
              (true, 0)
    in
    let {content; back_pointers; index} = cell1 in
    equal_content content cell2.content
    && Compare.Int.equal index cell2.index
    && equal_back_pointers back_pointers cell2.back_pointers

  let index cell = cell.index

  let back_pointers_to_list a =
    FallbackArray.fold
      (fun l -> function
        | Some ptr -> ptr :: l
        | None -> (* By [cell] invariants. *) assert false)
      a
      []
    |> List.rev

  let pp ~pp_content ~pp_ptr fmt {content; back_pointers; index} =
    Format.fprintf
      fmt
      {|
       content = %a
       index = %d
       back_pointers = %a
    |}
      pp_content
      content
      index
      (Format.pp_print_list pp_ptr)
      (back_pointers_to_list back_pointers)

  let encoding ptr_encoding content_encoding =
    let of_list =
      FallbackArray.of_list ~fallback:None ~proj:(fun c -> Some c)
    in
    let to_list = back_pointers_to_list in
    let open Data_encoding in
    conv
      (fun {index; content; back_pointers} ->
        (index, content, to_list back_pointers))
      (fun (index, content, back_pointers) ->
        {index; content; back_pointers = of_list back_pointers})
      (obj3
         (req "index" int31)
         (req "content" content_encoding)
         (req "back_pointers" (list ptr_encoding)))

  let content cell = cell.content

  let back_pointers cell = back_pointers_to_list cell.back_pointers

  let genesis content =
    {index = 0; content; back_pointers = FallbackArray.make 0 None}

  let back_pointer cell i = FallbackArray.get cell.back_pointers i

  (* Precondition: i < length cell.back_pointers *)
  let back_pointer_unsafe cell i =
    match FallbackArray.get cell.back_pointers i with
    | Some ptr -> ptr
    | None -> (* By precondition and invariants of cells. *) assert false

  let next ~prev_cell ~prev_cell_ptr content =
    let index = prev_cell.index + 1 in
    let back_pointers =
      let rec aux power accu i =
        if Compare.Int.(index < power) then List.rev accu
        else
          let back_pointer_i =
            if Compare.Int.(index mod power = 0) then prev_cell_ptr
            else
              (* The following call is valid because of
                 - [i < List.length prev_cell.back_pointer]
                   because [log_basis index = log_basis prev_cell.index]
                 - the invariants of [prev_cell] *)
              back_pointer_unsafe prev_cell i
          in
          let accu = back_pointer_i :: accu in
          aux (power * basis) accu (i + 1)
      in
      aux 1 [] 0
    in
    let back_pointers =
      FallbackArray.of_list ~fallback:None ~proj:Option.some back_pointers
    in
    {index; content; back_pointers}

  (* returns the array of [basis^i] forall [i < len (back_pointers cell)] *)
  let list_powers cell =
    let rec aux n prev p =
      if Compare.Int.(n <= 0) then List.rev p
      else aux (n - 1) (basis * prev) (prev :: p)
    in
    FallbackArray.of_list
      ~fallback:0
      ~proj:(fun x -> x)
      (aux (FallbackArray.length cell.back_pointers) 1 [])

  (*
    [back_pointers] are sorted in decreasing order of their pointing cell index
    in the list. So we can do a [binary_search] to find the [cell] with the
    smallest index that is greater than [target] in the list. 

    More formally, min({c : cell | c.index >= target.index}) where [c] is one of
    the pointed cells in the array of back pointers of the [cell] parameter.
  *)
  let best_skip cell target_index powers =
    let open FallbackArray in
    let pointed_cell_index i = cell.index - (cell.index mod get powers i) - 1 in
    let rec binary_search start_idx end_idx =
      if Compare.Int.(start_idx >= end_idx) then Some start_idx
      else
        let mid_idx = start_idx + ((end_idx - start_idx) / 2) in
        let mid_cell_index = pointed_cell_index mid_idx in
        if Compare.Int.(mid_cell_index = target_index) then Some mid_idx
        else if Compare.Int.(mid_cell_index < target_index) then
          binary_search start_idx (mid_idx - 1)
        else
          let prev_mid_cell_index = pointed_cell_index (mid_idx + 1) in
          if Compare.Int.(prev_mid_cell_index = target_index) then
            Some (mid_idx + 1)
          else if Compare.Int.(prev_mid_cell_index < target_index) then
            (*
              If (mid_cell_index > target_index) &&
                 (prev_mid_cell_index < target_index) 
              then we found the closest cell to the target, which is mid_cell. 
              so we return its index [mid_idx] in the array of back_pointers. 
            *)
            Some mid_idx
          else binary_search (mid_idx + 1) end_idx
    in
    binary_search 0 (length cell.back_pointers - 1)

  let back_path ~deref ~cell_ptr ~target_index =
    Option.bind (deref cell_ptr) @@ fun cell ->
    let powers = list_powers cell in
    let rec aux path ptr =
      let path = ptr :: path in
      Option.bind (deref ptr) @@ fun cell ->
      let index = cell.index in
      if Compare.Int.(target_index = index) then Some (List.rev path)
      else if Compare.Int.(target_index > index) then None
      else
        Option.bind (best_skip cell target_index powers) @@ fun best_idx ->
        Option.bind (back_pointer cell best_idx) @@ fun ptr -> aux path ptr
    in
    aux [] cell_ptr

  let mem equal x l =
    let open FallbackArray in
    let n = length l in
    let rec aux idx =
      if Compare.Int.(idx >= n) then false
      else
        match get l idx with
        | None -> aux (idx + 1)
        | Some y -> if equal x y then true else aux (idx + 1)
    in
    aux 0

  let assume_some o f = match o with None -> false | Some x -> f x

  let valid_back_path ~equal_ptr ~deref ~cell_ptr ~target_ptr path =
    assume_some (deref target_ptr) @@ fun target ->
    assume_some (deref cell_ptr) @@ fun cell ->
    let target_index = index target
    and cell_index = index cell
    and powers = list_powers cell in
    let rec valid_path index cell_ptr path =
      match (cell_ptr, path) with
      | final_cell, [] ->
          equal_ptr target_ptr final_cell && Compare.Int.(index = target_index)
      | cell_ptr, cell_ptr' :: path ->
          assume_some (deref cell_ptr) @@ fun cell ->
          assume_some (deref cell_ptr') @@ fun cell' ->
          mem equal_ptr cell_ptr' cell.back_pointers
          && assume_some (best_skip cell target_index powers) @@ fun best_idx ->
             assume_some (back_pointer cell best_idx) @@ fun best_ptr ->
             let minimal = equal_ptr best_ptr cell_ptr' in
             let index' = cell'.index in
             minimal && valid_path index' cell_ptr' path
    in
    match path with
    | [] -> false
    | first_cell_ptr :: path ->
        equal_ptr first_cell_ptr cell_ptr && valid_path cell_index cell_ptr path

  let search ~deref ~compare ~cell_ptr =
    (* TODO: #3321 replace with Lwt_option_syntax when that's in the
       environment V6 *)
    let ( let*? ) x f =
      match x with None -> Lwt.return None | Some y -> f y
    in
    let ( let*! ) = Lwt.bind in
    let rec aux path ptr ix =
      let*? cell = deref ptr in
      let*? candidate_ptr = back_pointer cell ix in
      let*? candidate_cell = deref candidate_ptr in
      let*! comparison = compare candidate_cell.content in
      if Compare.Int.(comparison = 0) then
        (* In this case, we have reached our target cell. *)
        Option.some_s (List.rev (candidate_ptr :: ptr :: path))
      else if Compare.Int.(comparison < 0) then
        if Compare.Int.(ix = 0) then
          (* If the first back pointer is 'too far' ([comparison < 0]),
             that means we won't find a valid target cell. *)
          Option.none_s
        else
          (* If a back pointer other than the first is 'too far'
             we can then backtrack to the previous back pointer. *)
          let*? new_ptr = back_pointer cell (ix - 1) in
          aux (ptr :: path) new_ptr 0
      else if Compare.Int.(ix + 1 >= FallbackArray.length cell.back_pointers)
      then
        (* If we reach the final back pointer and still have
           [comparison > 0], we should continue from that cell. *)
        aux (ptr :: path) candidate_ptr 0
      else
        (* Final case, we just try the next back pointer. *)
        aux path ptr (ix + 1)
    in
    let*? cell = deref cell_ptr in
    let*! comparison = compare cell.content in
    (* We must check that we aren't already at the target cell before
       starting the recursion. *)
    if Compare.Int.(comparison = 0) then Option.some_s [cell_ptr]
    else aux [] cell_ptr 0
end
