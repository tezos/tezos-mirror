module type IO = Io.S

module type CLOCK = sig
  (** A monotonic time source. See [Mtime_clock] for an OS-dependent
      implementation. *)

  type counter

  val counter : unit -> counter
  val count : counter -> Mtime.span
end

module type SEMAPHORE = sig
  (** Binary semaphores for mutual exclusion *)

  type t
  (** The type of binary semaphore. *)

  val make : bool -> t
  (** [make b] returns a new semaphore with the given initial state. If [b] is
      [true], the semaphore is initially available for acquisition; otherwise,
      the semaphore is initially unavailable. *)

  val acquire : string -> t -> unit
  (** Acquire the given semaphore. Acquisition is not re-entrant. *)

  val release : t -> unit
  (** Release the given semaphore. If any threads are attempting to acquire the
      semaphore, exactly one of them will gain access to the semaphore. *)

  val with_acquire : string -> t -> (unit -> 'a) -> 'a
  (** [with_acquire t f] first obtains [t], then computes [f ()], and finally
      release [t]. *)

  val is_held : t -> bool
  (** [is_held t] returns [true] if the semaphore is held, without acquiring
      [t]. *)
end

module type THREAD = sig
  (** Cooperative threads. *)

  type 'a t
  (** The type of thread handles. *)

  val async : (unit -> 'a) -> 'a t
  (** [async f] creates a new thread of control which executes [f ()] and
      returns the corresponding thread handle. The thread terminates whenever
      [f ()] returns a value or raises an exception. *)

  val await : 'a t -> ('a, [ `Async_exn of exn ]) result
  (** [await t] blocks on the termination of [t]. *)

  val return : 'a -> 'a t
  (** [return ()] is a pre-terminated thread handle. *)

  val yield : unit -> unit
  (** Re-schedule the calling thread without suspending it. *)
end

module type FMT_TTY = sig
  val setup_std_outputs :
    ?style_renderer:Fmt.style_renderer -> ?utf_8:bool -> unit -> unit
end

module type S = sig
  module IO : IO
  module Semaphore : SEMAPHORE
  module Thread : THREAD
  module Clock : CLOCK
  module Progress : Progress_engine.S
  module Fmt_tty : FMT_TTY
end
