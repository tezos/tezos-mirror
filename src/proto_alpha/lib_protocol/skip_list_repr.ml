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
end

module Make (Parameters : sig
  val basis : int
end) : S = struct
  let () = assert (Compare.Int.(Parameters.basis >= 2))

  open Parameters

  (*

      A cell of a skip list with some [`content] and backpointers of
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

     - The back pointers of a node are entirely determined by the
       back pointers of its predecessor (except for the genesis
       node) and a pointer to this predecessor. This locality makes
       the insertion of new node very efficient in practice.

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
      &&
      let i = ref 0 in
      let equal = ref true in
      while Compare.Int.(!i < length b1) && !equal do
        equal := Option.equal equal_ptr (get b1 !i) (get b2 !i) ;
        incr i
      done ;
      !equal
    in
    let {content; back_pointers; index} = cell1 in
    equal_content content cell2.content
    && equal_back_pointers back_pointers cell2.back_pointers
    && Compare.Int.equal index cell2.index

  let index cell = cell.index

  let back_pointers_to_list a =
    FallbackArray.fold
      (fun l -> function
        | Some ptr -> ptr :: l
        | None -> (* By [cell] invariants. *) assert false)
      a
      []
    |> List.rev

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
      FallbackArray.of_list ~fallback:None ~proj:(fun x -> Some x) back_pointers
    in
    {index; content; back_pointers}

  let best_skip cell target_index =
    let index = cell.index in
    let rec aux idx pow best_idx best_skip =
      if Compare.Int.(idx >= FallbackArray.length cell.back_pointers) then
        best_idx
      else
        let idx_index = index - (index mod pow) - 1 in
        let skip = index - idx_index in
        if
          Compare.Int.(idx_index < target_index)
          || Option.equal Compare.Int.equal (Some skip) best_skip
        then best_idx
        else aux (idx + 1) (basis * pow) (Some idx) (Some skip)
    in
    aux 0 1 None None

  let back_path ~deref ~cell_ptr ~target_index =
    let rec aux path ptr =
      let path = ptr :: path in
      Option.bind (deref ptr) @@ fun cell ->
      let index = cell.index in
      if Compare.Int.(target_index = index) then Some (List.rev path)
      else if Compare.Int.(target_index > index) then None
      else
        Option.bind (best_skip cell target_index) @@ fun best_idx ->
        Option.bind (back_pointer cell best_idx) @@ fun ptr -> aux path ptr
    in
    aux [] cell_ptr

  let mem equal x l =
    let open FallbackArray in
    let n = length l in
    let rec aux idx =
      if Compare.Int.(idx >= n) then false
      else
        match FallbackArray.get l idx with
        | None -> aux (idx + 1)
        | Some y -> if equal x y then true else aux (idx + 1)
    in
    aux 0

  let assume_some o f = match o with None -> false | Some x -> f x

  let valid_back_path ~equal_ptr ~deref ~cell_ptr ~target_ptr path =
    assume_some (deref target_ptr) @@ fun target ->
    assume_some (deref cell_ptr) @@ fun cell ->
    let target_index = index target and cell_index = index cell in
    let rec valid_path index cell_ptr path =
      match (cell_ptr, path) with
      | (final_cell, []) ->
          equal_ptr target_ptr final_cell && Compare.Int.(index = target_index)
      | (cell_ptr, cell_ptr' :: path) ->
          assume_some (deref cell_ptr) @@ fun cell ->
          assume_some (deref cell_ptr') @@ fun cell' ->
          mem equal_ptr cell_ptr' cell.back_pointers
          && assume_some (best_skip cell target_index) @@ fun best_idx ->
             assume_some (back_pointer cell best_idx) @@ fun best_ptr ->
             let minimal = equal_ptr best_ptr cell_ptr' in
             let index' = cell'.index in
             minimal && valid_path index' cell_ptr' path
    in
    match path with
    | [] -> false
    | first_cell_ptr :: path ->
        equal_ptr first_cell_ptr cell_ptr && valid_path cell_index cell_ptr path
end
