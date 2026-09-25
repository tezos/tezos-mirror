(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Number of preimages fetched at a time by default by callers that want a
    concurrent download: enough to hide most of the latency of a preimages
    endpoint without opening so many connections that a modest one starts
    refusing them. *)
val default_concurrency : int

(** [download ~preimages_endpoint ~preimages ~root_hash ?num_download_retries
    ?concurrency ?progress ()] fetches from [preimages_endpoint] all the
    preimages the kernel [root_hash] is made of, and stores them in the
    [preimages] directory. A preimage whose hash does not match the one it was
    requested for is downloaded again at most [num_download_retries] times
    (once by default).

    [concurrency] is how many preimages are fetched at a time (one by default,
    that is, sequentially), and also the number of connections opened onto
    [preimages_endpoint]. It must be at least one; the function fails
    otherwise. Only the content pages of the kernel are fetched concurrently;
    the hash pages naming them are walked one level at a time, as a level
    cannot be known before the level above it has been read.

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
  ?concurrency:int ->
  ?progress:bool ->
  unit ->
  unit tzresult Lwt.t
