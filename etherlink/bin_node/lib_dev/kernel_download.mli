(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [download ~preimages_endpoint ~preimages ~root_hash ?num_download_retries
    ?progress ()] fetches from [preimages_endpoint] all the preimages the kernel
    [root_hash] is made of, and stores them in the [preimages] directory. A
    preimage whose hash does not match the one it was requested for is
    downloaded again at most [num_download_retries] times (once by default).

    A preimage already present in [preimages] is read from there instead of
    being fetched again, so an interrupted download resumes where it stopped.

    When [progress] is set (it is not by default), a progress bar reporting the
    number of preimages downloaded out of the total is displayed on the
    terminal. *)
val download :
  preimages_endpoint:Uri.t ->
  preimages:string ->
  root_hash:Hex.t ->
  ?num_download_retries:int ->
  ?progress:bool ->
  unit ->
  unit tzresult Lwt.t
