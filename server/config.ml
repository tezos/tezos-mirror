(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type tls_conf = {crt : string; key : string}

type connection = {source : string option; port : int; tls : tls_conf option}

type t = {
  db_uri : string;
  network_interfaces : connection list;
  public_directory : string option;
  admins : (string * string) list;
  users : (string * string) list;
}

let tls_conf_encoding =
  let open Data_encoding in
  conv
    (fun {crt; key} -> (crt, key))
    (fun (crt, key) -> {crt; key})
    (obj2 (req "certfile" string) (req "keyfile" string))

let connection_encoding =
  let open Data_encoding in
  conv
    (fun {source; port; tls} -> (source, port, tls))
    (fun (source, port, tls) -> {source; port; tls})
    (obj3
       (opt
          ~description:"network interface address to bind to"
          "address"
          string)
       (req ~description:"tcp port on which listen" "port" int16)
       (opt
          ~description:"serve in https using the given certificate"
          "tls"
          tls_conf_encoding))

let login_encoding =
  let open Data_encoding in
  obj2 (req "login" string) (req "password" string)

let encoding =
  let open Data_encoding in
  conv
    (fun {db_uri; network_interfaces; public_directory; admins; users} ->
      (db_uri, network_interfaces, public_directory, admins, users))
    (fun (db_uri, network_interfaces, public_directory, admins, users) ->
      {db_uri; network_interfaces; public_directory; admins; users})
    (obj5
       (req
          ~description:
            "Uri to reach the database: sqlite3:path or postgresql://host:port"
          "db"
          string)
       (dft "interfaces" (list connection_encoding) [])
       (opt
          "public_directory"
          ~description:
            "Path of the directory used to server static files (e.g. a dataviz \
             frontend application). If none provided this feature will not be \
             used (i.e. no default value)."
          string)
       (req "admins" (list login_encoding))
       (req "users" (list login_encoding)))
