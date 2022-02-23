(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type device_info = {
  path : string ;
  vendor_id : int ;
  product_id : int ;
  serial_number : string option ;
  release_number : int ;
  manufacturer_string : string option ;
  product_string : string option ;
  usage_page : int ;
  usage : int ;
  interface_number : int ;
}

type t
type info

external hid_error : t -> string option = "stub_hid_error"

external init : unit -> unit = "stub_hid_init" [@@noalloc]
external deinit : unit -> unit = "stub_hid_exit" [@@noalloc]

external hid_enumerate : int -> int -> info option = "stub_hid_enumerate"
external hid_enumerate_next : info -> device_info * info option = "stub_hid_enumerate_next"
external hid_free_enumeration : info -> unit = "stub_hid_free_enumeration" [@@noalloc]

let enumerate ?(vendor_id=0) ?(product_id=0) () =
  match hid_enumerate vendor_id product_id with
  | None -> []
  | Some info ->
    let rec inner acc i =
      match hid_enumerate_next i with
      | di, None -> di :: acc
      | di, Some next -> inner (di :: acc) next
    in
    let res = inner [] info in
    hid_free_enumeration info ;
    res

external hid_open : int -> int -> t option = "stub_hid_open"

let open_id ~vendor_id ~product_id =
  hid_open vendor_id product_id

external open_path : string -> t option = "stub_hid_open_path"

let open_id_exn ~vendor_id ~product_id =
  match open_id ~vendor_id ~product_id with
  | None -> failwith "open_id_exn"
  | Some t -> t

let open_path_exn path =
  match open_path path with
  | None -> failwith "open_path_exn"
  | Some t -> t

external hid_write :
  t -> Bigstring.t -> int -> int = "stub_hid_write" [@@noalloc]
external hid_read_timeout :
  t -> Bigstring.t -> int -> int -> int = "stub_hid_read_timeout" [@@noalloc]
external hid_set_nonblocking :
  t -> bool -> int = "stub_hid_set_nonblocking" [@@noalloc]
external close :
  t -> unit = "stub_hid_close" [@@noalloc]

let set_nonblocking t v =
  match hid_set_nonblocking t v with
  | -1 -> Error (match hid_error t with None -> "" | Some msg -> msg)
  | _ -> Ok ()

let set_nonblocking_exn t v =
  match set_nonblocking t v with
  | Error err -> failwith err
  | Ok () -> ()

let write t ?len buf =
  let buflen = Bigstring.length buf in
  let len =
    match len with
    | None -> buflen
    | Some l when l < 0 ->
        invalid_arg (Printf.sprintf "write: len = %d must be positive" l)
    | Some l when l > buflen ->
        invalid_arg (Printf.sprintf "write: len is too big (%d > %d)" l buflen)
    | Some l -> l in
  match hid_write t buf len with
  | -1 -> Error (match hid_error t with None -> "" | Some msg -> msg)
  | nb_written -> Ok nb_written

let read ?(timeout=(-1)) t buf len =
  match hid_read_timeout t buf len timeout with
  | -1 -> Error (match hid_error t with None -> "" | Some msg -> msg)
  | nb_read -> Ok nb_read

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
