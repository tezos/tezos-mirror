(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

let product_source = ["lwt_domain"]

include Product (struct
  let name = "lwt_domain"

  let source = product_source
end)

let lwt_domain =
  public_lib
    "octez-lwt-domain"
    ~synopsis:"Fork of lwt_domain (https://github.com/ocsigen/lwt_domain/)"
    ~internal_name:"lwt_domain"
    ~extra_authors:["Sudha Parimala"]
    ~path:"lwt_domain"
    ~deps:[lwt; lwt_unix; domainslib]
    ~conflicts:[external_lib "lwt_domain" V.True]
