(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Data_encoding
open Unix

let unix_error_to_tag = function
  | E2BIG -> 0
  | EACCES -> 1
  | EAGAIN -> 2
  | EBADF -> 3
  | EBUSY -> 4
  | ECHILD -> 5
  | EDEADLK -> 6
  | EDOM -> 7
  | EEXIST -> 8
  | EFAULT -> 9
  | EFBIG -> 10
  | EINTR -> 11
  | EINVAL -> 12
  | EIO -> 13
  | EISDIR -> 14
  | EMFILE -> 15
  | EMLINK -> 16
  | ENAMETOOLONG -> 17
  | ENFILE -> 18
  | ENODEV -> 19
  | ENOENT -> 20
  | ENOEXEC -> 21
  | ENOLCK -> 22
  | ENOMEM -> 23
  | ENOSPC -> 24
  | ENOSYS -> 25
  | ENOTDIR -> 26
  | ENOTEMPTY -> 27
  | ENOTTY -> 28
  | ENXIO -> 29
  | EPERM -> 30
  | EPIPE -> 31
  | ERANGE -> 32
  | EROFS -> 33
  | ESPIPE -> 34
  | ESRCH -> 35
  | EXDEV -> 36
  | EWOULDBLOCK -> 37
  | EINPROGRESS -> 38
  | EALREADY -> 39
  | ENOTSOCK -> 40
  | EDESTADDRREQ -> 41
  | EMSGSIZE -> 42
  | EPROTOTYPE -> 43
  | ENOPROTOOPT -> 44
  | EPROTONOSUPPORT -> 45
  | ESOCKTNOSUPPORT -> 46
  | EOPNOTSUPP -> 47
  | EPFNOSUPPORT -> 48
  | EAFNOSUPPORT -> 49
  | EADDRINUSE -> 50
  | EADDRNOTAVAIL -> 51
  | ENETDOWN -> 52
  | ENETUNREACH -> 53
  | ENETRESET -> 54
  | ECONNABORTED -> 55
  | ECONNRESET -> 56
  | ENOBUFS -> 57
  | EISCONN -> 58
  | ENOTCONN -> 59
  | ESHUTDOWN -> 60
  | ETOOMANYREFS -> 61
  | ETIMEDOUT -> 62
  | ECONNREFUSED -> 63
  | EHOSTDOWN -> 64
  | EHOSTUNREACH -> 65
  | ELOOP -> 66
  | EOVERFLOW -> 67
  | EUNKNOWNERR _ -> 68

let get_constructor_name = function
  | E2BIG -> "E2BIG"
  | EACCES -> "EACCES"
  | EAGAIN -> "EAGAIN"
  | EBADF -> "EBADF"
  | EBUSY -> "EBUSY"
  | ECHILD -> "ECHILD"
  | EDEADLK -> "EDEADLK"
  | EDOM -> "EDOM"
  | EEXIST -> "EEXIST"
  | EFAULT -> "EFAULT"
  | EFBIG -> "EFBIG"
  | EINTR -> "EINTR"
  | EINVAL -> "EINVAL"
  | EIO -> "EIO"
  | EISDIR -> "EISDIR"
  | EMFILE -> "EMFILE"
  | EMLINK -> "EMLINK"
  | ENAMETOOLONG -> "ENAMETOOLONG"
  | ENFILE -> "ENFILE"
  | ENODEV -> "ENODEV"
  | ENOENT -> "ENOENT"
  | ENOEXEC -> "ENOEXEC"
  | ENOLCK -> "ENOLCK"
  | ENOMEM -> "ENOMEM"
  | ENOSPC -> "ENOSPC"
  | ENOSYS -> "ENOSYS"
  | ENOTDIR -> "ENOTDIR"
  | ENOTEMPTY -> "ENOTEMPTY"
  | ENOTTY -> "ENOTTY"
  | ENXIO -> "ENXIO"
  | EPERM -> "EPERM"
  | EPIPE -> "EPIPE"
  | ERANGE -> "ERANGE"
  | EROFS -> "EROFS"
  | ESPIPE -> "ESPIPE"
  | ESRCH -> "ESRCH"
  | EXDEV -> "EXDEV"
  | EWOULDBLOCK -> "EWOULDBLOCK"
  | EINPROGRESS -> "EINPROGRESS"
  | EALREADY -> "EALREADY"
  | ENOTSOCK -> "ENOTSOCK"
  | EDESTADDRREQ -> "EDESTADDRREQ"
  | EMSGSIZE -> "EMSGSIZE"
  | EPROTOTYPE -> "EPROTOTYPE"
  | ENOPROTOOPT -> "ENOPROTOOPT"
  | EPROTONOSUPPORT -> "EPROTONOSUPPORT"
  | ESOCKTNOSUPPORT -> "ESOCKTNOSUPPORT"
  | EOPNOTSUPP -> "EOPNOTSUPP"
  | EPFNOSUPPORT -> "EPFNOSUPPORT"
  | EAFNOSUPPORT -> "EAFNOSUPPORT"
  | EADDRINUSE -> "EADDRINUSE"
  | EADDRNOTAVAIL -> "EADDRNOTAVAIL"
  | ENETDOWN -> "ENETDOWN"
  | ENETUNREACH -> "ENETUNREACH"
  | ENETRESET -> "ENETRESET"
  | ECONNABORTED -> "ECONNABORTED"
  | ECONNRESET -> "ECONNRESET"
  | ENOBUFS -> "ENOBUFS"
  | EISCONN -> "EISCONN"
  | ENOTCONN -> "ENOTCONN"
  | ESHUTDOWN -> "ESHUTDOWN"
  | ETOOMANYREFS -> "ETOOMANYREFS"
  | ETIMEDOUT -> "ETIMEDOUT"
  | ECONNREFUSED -> "ECONNREFUSED"
  | EHOSTDOWN -> "EHOSTDOWN"
  | EHOSTUNREACH -> "EHOSTUNREACH"
  | ELOOP -> "ELOOP"
  | EOVERFLOW -> "EOVERFLOW"
  | EUNKNOWNERR _ -> "EUNKNOWNERR"

(* This function is used to create an encoding for
   the majority of unix error cases, which could be
   encoded as constants.
   It should not be applied to eunknownerr which needs
   to also encode the error code *)
let unix_error_constant_encoding error =
  let error_name = get_constructor_name @@ error in
  obj1 @@ req "unix-error" (constant error_name)

let eunknownerr_encoding =
  let error_name = get_constructor_name @@ EUNKNOWNERR 0 in
  obj1 @@ req "unix-error" @@ obj1 @@ req error_name int31

let constant_case error =
  let title = get_constructor_name error in
  let tag = unix_error_to_tag error in
  case
    ~title
    (Tag tag)
    (unix_error_constant_encoding error)
    (fun x -> if x = error then Some () else None)
    (fun () -> error)

let encoding =
  matching
    (fun error ->
      let tag = unix_error_to_tag error in
      match error with
      | EUNKNOWNERR code -> matched tag eunknownerr_encoding code
      | _ -> matched tag (unix_error_constant_encoding error) ())
    [
      constant_case E2BIG;
      constant_case EACCES;
      constant_case EAGAIN;
      constant_case EBADF;
      constant_case EBUSY;
      constant_case ECHILD;
      constant_case EDEADLK;
      constant_case EDOM;
      constant_case EEXIST;
      constant_case EFAULT;
      constant_case EFBIG;
      constant_case EINTR;
      constant_case EINVAL;
      constant_case EIO;
      constant_case EISDIR;
      constant_case EMFILE;
      constant_case EMLINK;
      constant_case ENAMETOOLONG;
      constant_case ENFILE;
      constant_case ENODEV;
      constant_case ENOENT;
      constant_case ENOEXEC;
      constant_case ENOLCK;
      constant_case ENOMEM;
      constant_case ENOSPC;
      constant_case ENOSYS;
      constant_case ENOTDIR;
      constant_case ENOTEMPTY;
      constant_case ENOTTY;
      constant_case ENXIO;
      constant_case EPERM;
      constant_case EPIPE;
      constant_case ERANGE;
      constant_case EROFS;
      constant_case ESPIPE;
      constant_case ESRCH;
      constant_case EXDEV;
      constant_case EWOULDBLOCK;
      constant_case EINPROGRESS;
      constant_case EALREADY;
      constant_case ENOTSOCK;
      constant_case EDESTADDRREQ;
      constant_case EMSGSIZE;
      constant_case EPROTOTYPE;
      constant_case ENOPROTOOPT;
      constant_case EPROTONOSUPPORT;
      constant_case ESOCKTNOSUPPORT;
      constant_case EOPNOTSUPP;
      constant_case EPFNOSUPPORT;
      constant_case EAFNOSUPPORT;
      constant_case EADDRINUSE;
      constant_case EADDRNOTAVAIL;
      constant_case ENETDOWN;
      constant_case ENETUNREACH;
      constant_case ENETRESET;
      constant_case ECONNABORTED;
      constant_case ECONNRESET;
      constant_case ENOBUFS;
      constant_case EISCONN;
      constant_case ENOTCONN;
      constant_case ESHUTDOWN;
      constant_case ETOOMANYREFS;
      constant_case ETIMEDOUT;
      constant_case ECONNREFUSED;
      constant_case EHOSTDOWN;
      constant_case EHOSTUNREACH;
      constant_case ELOOP;
      constant_case EOVERFLOW;
      case
        ~title:(get_constructor_name @@ EUNKNOWNERR 0)
        (Tag (unix_error_to_tag @@ EUNKNOWNERR 0))
        eunknownerr_encoding
        (function EUNKNOWNERR e -> Some e | _ -> None)
        (fun e -> EUNKNOWNERR e);
    ]

module Internal_for_tests = struct
  let get_constructor_name = get_constructor_name
end
