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

val init : unit -> unit
(** [init ()] initializes the HIDAPI library. Calling it is not
    strictly necessary, however this function should be called at the
    beginning of execution however, if there is a chance of HIDAPI
    handles being opened by different threads simultaneously. *)

val deinit : unit -> unit
(** [deinit ()] frees all of the static data associated with
    HIDAPI. It should be called at the end of execution to avoid memory
    leaks. *)

val enumerate :
  ?vendor_id:int -> ?product_id:int -> unit -> device_info list
(** [enumerate ?vendor_id ?product_id ()] is the list of HID devices
    attached to the system. The optional arguments are a way to filter
    the results returned. *)

val open_id : vendor_id:int -> product_id:int -> t option
(** [open_id ~vendor_id ~product_id] is the device handle of HID
    device (vendor_id, product_id), or None if no such device exist or
    in case of error. *)

val open_path : string -> t option
(** [open_path path] is the device handle of HID device of path
    [path], or None if no such device exist or in case of error. [path]
    can be discovered with [enumerate] or a platform-specific path name
    can be used (eg: /dev/hidraw0 on Linux). *)

val open_id_exn : vendor_id:int -> product_id:int -> t
val open_path_exn : string -> t

val write : t -> ?len:int -> Bigstring.t -> (int, string) result
(** [write t buf] is [Ok nb_bytes_written] on success, or
    [Error description] in case of error. *)

val read : ?timeout:int -> t -> Bigstring.t -> int -> (int, string) result
(** [read ?timeout t buf len] is [Ok nb_bytes_read] on success, or
    [Error description] in case of error. *)

val set_nonblocking : t -> bool -> (unit, string) result
(** [set_nonblocking t v] sets nonblocking mode if [v] is [true], or
    sets blocking mode otherwise. *)

val set_nonblocking_exn : t -> bool -> unit

val close : t -> unit
(** [close t] closes the HID device [t]. *)

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
