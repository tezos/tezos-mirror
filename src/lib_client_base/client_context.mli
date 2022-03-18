(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module contains several class types used to provide I/O
    services for the client, the wallet, etc.

    This interface is independent of the backend. This is intended for the client
    library code, so it can be independent of the backend. The client, and each
    other client-like systems (wallet apps, in-browser widgets, etc.), are
    responsible for providing a backend-specific implementation.

    To find examples on how these methods are called, you can have a look at
    - src/bin_client/client_protocols_commands.ml
    - src/lib_client_base/client_aliases.ml
    - src/lib_client_base_unix/client_context_unix.ml
*)

type ('a, 'b) lwt_format = ('a, Format.formatter, unit, 'b Lwt.t) format4

(** [printer] is a class for objects that provide output functions to
    display information to the end-user. *)
class type printer =
  object
    method error : ('a, 'b) lwt_format -> 'a

    method warning : ('a, unit) lwt_format -> 'a

    method message : ('a, unit) lwt_format -> 'a

    method answer : ('a, unit) lwt_format -> 'a

    method log : string -> ('a, unit) lwt_format -> 'a
  end

(** [prompter] is a class of objects that provide input functions to
    request data from the end-user, whether normal inputs or
    passwords. *)
class type prompter =
  object
    method prompt : ('a, string tzresult) lwt_format -> 'a

    method prompt_password : ('a, Bytes.t tzresult) lwt_format -> 'a

    (** when [multiple_password_retries] is [true], password
        prompt should retries more than once. [true] is the default
        value. *)
    method multiple_password_retries : bool
  end

class type io =
  object
    inherit printer

    inherit prompter
  end

(** Operations on the wallet. *)
class type wallet =
  object
    method load_passwords : string Lwt_stream.t option

    (** [read_file path] reads the content of the file given by
        [path]. Note that the whole content of the file is loaded into
        memory: you shouldn't read big files using this method. Errors
        that may be returned are implementation-dependent. *)
    method read_file : string -> string tzresult Lwt.t

    (** [with_lock f] calls [f ()] exclusively from any other function
        that is wrapped within [with_lock]. *)
    method with_lock : (unit -> 'a Lwt.t) -> 'a Lwt.t

    (** [load alias ~default enc] reads the file corresponding to the
        [alias], and parses using [encoding]. If the file does not
        exist, then [default] is returned. *)
    method load :
      string -> default:'a -> 'a Data_encoding.encoding -> 'a tzresult Lwt.t

    (** [write alias x encoding] writes in a file corresponding to the
        [alias] the information given by [x] using the [encoding]. *)
    method write :
      string -> 'a -> 'a Data_encoding.encoding -> unit tzresult Lwt.t

    (** [last_modification_time alias] returns the last modification
        time of the file corresponding to the [alias], if the file exists;
        otherwise [None]. *)
    method last_modification_time : string -> float option tzresult Lwt.t

    (** Current base directory. Stores the information of keys (public
        key hashes, public keys, secret keys) and watermarks. *)
    method get_base_dir : string
  end

(** Accessor on the chain. *)
class type chain =
  object
    method chain : Shell_services.chain
  end

(** Operations on blocks. *)
class type block =
  object
    method block : Shell_services.block

    method confirmations : int option
  end

(** Primitives for input, output and wallet.
    The general organisation of the code in this module is to
    provide small classes that are also combined into bigger
    classes. It allows different client library functions to only
    depend on some features, but not all, so that these functions
    can be used in places that only have access to these
    features. *)
class type io_wallet =
  object
    inherit printer

    inherit prompter

    inherit wallet
  end

(** Primitives for input, output and RPCs. *)
class type io_rpcs =
  object
    inherit printer

    inherit prompter

    inherit RPC_context.generic
  end

(** User interface related operations. *)
class type ui =
  object
    method sleep : float -> unit Lwt.t

    method exit : int -> 'a

    method now : unit -> Ptime.t
  end

(** A comprehensive class type gathering the above class types, that
    is used for #Protocol_client_context.full. *)
class type full =
  object
    inherit printer

    inherit prompter

    inherit wallet

    inherit RPC_context.generic

    inherit chain

    inherit block

    inherit ui
  end

(** A simple printer can be used to implement a printer as it is done
    in class [Client_context_unix.unix_logger]. *)
class simple_printer : (string -> string -> unit Lwt.t) -> printer

class proxy_context : full -> full

val null_printer : printer
