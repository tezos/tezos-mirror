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

type launch_mode = Socket | Local | Http

(** A signer instance *)
type t

(** Create a signer instance and import [keys], but do not start the daemon.

    This is useful for running one-shot signer CLI commands (e.g.
    [gen keys], [create bls proof]) without launching a listening daemon.

    The signer is configured to use its private base directory. *)
val create :
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?base_dir:string ->
  ?launch_mode:launch_mode ->
  ?uri:Uri.t ->
  ?runner:Runner.t ->
  ?check_highwatermark:bool ->
  ?magic_byte:string ->
  ?allow_list_known_keys:bool ->
  ?allow_to_prove_possession:bool ->
  ?keys:Account.key list ->
  unit ->
  t Lwt.t

(** Initialize a signer.

    This creates a signer, waits for it to be ready, and then returns it.

    The signer does not rely on a node for its initialization.
    While initializing the signer, we add a list of accounts [keys]
    that the signer is going to use for signing.

    We also pass the [uri] that the signer will use to answer signing
    calls.

    The standard output and standard error output of the signer will
    be logged with prefix [name] and color [color].

    Default [event_pipe] is a temporary file whose name is derived
    from [name]. It will be created as a named pipe so that signer
    events can be received.

    The signer is configured to use its private base directory.

    If [runner] is specified, the signer will be spawned on this
    runner using SSH.

    The allowed magic byte value for the signer can be specified with [magic_byte]. *)
val init :
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?base_dir:string ->
  ?launch_mode:launch_mode ->
  ?uri:Uri.t ->
  ?runner:Runner.t ->
  ?keys:Account.key list ->
  ?check_highwatermark:bool ->
  ?magic_byte:string ->
  ?allow_list_known_keys:bool ->
  ?allow_to_prove_possession:bool ->
  unit ->
  t Lwt.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** Send SIGKILL and wait for the process to terminate. *)
val kill : t -> unit Lwt.t

(** Register an event handler that logs all events.

    Use this when you need to debug or reverse engineer incoming events.
    Usually you do not want to keep that in the final versions of your tests. *)
val log_events : ?max_length:int -> t -> unit

(** Same as [wait_for_full] but ignore metadata from the file descriptor sink.

    More precisely, [filter] is applied to the value of field
    ["fd-sink-item.v0"."event".<name>].

    If the node receives a JSON value that does not match the right
    JSON structure, it is not given to [filter] and the event is
    ignored. See [wait_for_full] to know what the JSON value must
    look like. *)
val wait_for :
  ?timeout:float ->
  ?where:string ->
  t ->
  string ->
  (JSON.t -> 'a option) ->
  'a Lwt.t

(** Raw events. *)
type event = {name : string; value : JSON.t; timestamp : float}

(** Add a callback to be called whenever the node emits an event.

    Contrary to [wait_for] functions, this callback is never removed.

    Listening to events with [on_event] will not prevent [wait_for] promises
    to be fulfilled. You can also have multiple [on_event] handlers, although
    the order in which they trigger is unspecified. *)
val on_event : t -> (event -> unit) -> unit

(** Restart the daemon. *)
val restart : t -> unit Lwt.t

(* Get the [uri] that was passed to [init]. *)
val uri : t -> Uri.t

(** Get the base directory of a signer.

    The base directory is the location where signers store their keys, logs and
    highwatermarks. It corresponds to the [--base-dir] option. *)
val base_dir : t -> string

(** Spawn a signer CLI command.

    This runs [octez-signer --base-dir <base_dir> <command>] and returns
    the resulting process. The signer does not need to be running as a
    daemon for this to work. *)
val spawn_command :
  ?env:string String_map.t ->
  ?hooks:Process.hooks ->
  t ->
  string list ->
  Process.t

(** Import a secret key into the signer's wallet. *)
val import_secret_key : t -> Account.key -> unit Lwt.t

(** Spawn [octez-signer gen keys <alias> --sig <sig_alg>]. *)
val spawn_gen_keys : ?alias:string -> ?sig_alg:string -> t -> Process.t

(** Generate keys in the signer's wallet. Returns the alias used. *)
val gen_keys : ?alias:string -> ?sig_alg:string -> t -> string Lwt.t

(** Spawn [octez-signer show address <alias> --show-secret]. *)
val spawn_show_address : alias:string -> t -> Process.t

(** Show address details for an alias. Returns the public key hash. *)
val show_address : alias:string -> t -> string Lwt.t

(** Spawn [octez-signer create bls proof for <sk_uri>].
    If [override_pk] is provided, the [--override-public-key] option is passed. *)
val spawn_bls_prove_possession :
  ?override_pk:string -> sk_uri:string -> t -> Process.t

(** Create a BLS proof of possession for the given secret key URI.
    If [override_pk] is provided, the proof signs the given public key
    instead of the one derived from the secret key.
    Returns the Base58-encoded BLS signature (BLsig...). *)
val bls_prove_possession :
  ?override_pk:string -> sk_uri:string -> t -> string Lwt.t
