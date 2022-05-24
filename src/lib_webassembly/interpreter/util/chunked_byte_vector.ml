open Bigarray
open Lib.Bigarray

module Map = Lazy_map.Mutable.Make (Int64)

(** Page within the byte vector *)
type page = (int, int8_unsigned_elt, c_layout) Array1.t

(** Number of bits in an address for the page offset *)
let page_offset_bits = 12

(** Size of a page in bytes - with 12 bits of address space the page is 4KiB *)
let page_size = Int64.shift_left 1L page_offset_bits

(** Get the page index for an address. *)
let page_index address = Int64.shift_right address page_offset_bits

(** Get the offset within its page for a given address. *)
let page_offset address = Int64.(logand address (sub page_size 1L))

let alloc_page () =
  let page = Array1_64.create Int8_unsigned C_layout page_size in
  Array1.fill page 0;
  page

type t = { mutable length : int64; pages : page Map.t }

let num_pages length =
  if Int64.compare length 0L > 0 then
    (* [pred length] is used to cover the edge cases where [length] is an exact
       multiple of [page_size]. For example [div page_size page_size] is 1 but
       would allocate 2 pages without a [pred] applied to the first argument. *)
    Int64.(div (pred length) page_size |> succ)
  else
    0L

let create length =
  let pages =
    Map.create
      ~produce_value:(fun _ -> alloc_page ())
      (num_pages length)
  in
  { length; pages }

let grow vector size_delta =
  if Int64.compare size_delta 0L > 0 then
    let new_size = Int64.add vector.length size_delta in
    let new_pages = num_pages new_size in
    let current_pages = Map.num_elements vector.pages in
    let page_delta = Int64.sub new_pages current_pages in
    if Int64.compare page_delta 0L > 0 then
      Map.grow page_delta vector.pages;
    vector.length <- new_size

let length vector = vector.length

let load_byte vector address =
  if Int64.compare address vector.length >= 0 then raise Memory_exn.Bounds;
  let sub_page = Map.get (page_index address) vector.pages in
  Array1_64.get sub_page (page_offset address)

let store_byte vector address byte =
  if Int64.compare address vector.length >= 0 then raise Memory_exn.Bounds;
  let sub_page = Map.get (page_index address) vector.pages in
  Array1_64.set sub_page (page_offset address) byte

let store_bytes vector address bytes =
  Bytes.iteri
    (fun i c -> store_byte vector Int64.(of_int i |> add address) (Char.code c))
    bytes

let of_string str =
  let vector = String.length str |> Int64.of_int |> create in
  String.iteri
    (fun i c -> store_byte vector (Int64.of_int i) (Char.code c))
    str;
  vector
