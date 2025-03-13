(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(**
   This module defines a "structured event logging framework."

   Internal-Event streams are like traditional logs but they have a proper
   {!Data_encoding} format in order to be processed by software.

   The module defines "Sinks" {!SINK} as the receptacle for structured
   events: pluggable modules which can absorb (i.e. display, store,
   forward) the events emitted within the code-base.
*)

open Tezos_error_monad
open Error_monad

(** {3 Events Definitions and Registration } *)

(** The relative importance of a particular event (compatible with
    traditional logging systems, cf. {!Lwt_log_core.level}). *)
type level = Debug | Info | Notice | Warning | Error | Fatal

(** Module to manipulate values of type {!level}.  *)
module Level : sig
  exception Not_a_level of string

  (** Alias of {!level}. *)
  type t = level

  (** The default level is {!Info}. *)
  val default : t

  val to_string : t -> string

  (** [opt_to_string o] will return [to_string o] if [t] is [Some o] and ["none"] otherwise  *)
  val opt_to_string : t option -> string

  (** [of_string_exn s] will
      - return [Some level] if [s] represents a {!level},
      - return [None] if [s] is ["none"]
      - raise Not_a_level if [s] is not a proper level *)
  val of_string_exn : string -> t option

  val encoding : t Data_encoding.t

  include Compare.S with type t := t
end

(** Sections are a simple way of classifying events at the time of
    their emission. *)
module Section : sig
  type t

  val empty : t

  (** Build a {!Section.t} by replacing special characters with ['_']. *)
  val make_sanitized : string list -> t

  (** [is_prefix ~prefix p] checks that [p] starts with [~prefix].  *)
  val is_prefix : prefix:t -> t -> bool

  val encoding : t Data_encoding.t

  val to_string_list : t -> string list

  val pp : Format.formatter -> t -> unit

  include Compare.S with type t := t
end

(** All the section that has been registered. Currently, sections are registered
    by the `Simple`. *)
val get_registered_sections : unit -> string Seq.t

val register_section : Section.t -> unit

(** Alternative colors usable in console logs. Yellow and Red are already used
  for Warning/alerts log levels*)
type alternative_color = Magenta | Cyan | Green | Blue

(** Parameters defining an inspectable type of events. *)
module type EVENT_DEFINITION = sig
  type t

  (** Defines an optional section for the event.

      {b Warning} [None] is only for legacy events and
     should not be used in new code.  *)
  val section : Section.t option

  (** Defines the identifier for the event. Names should be unique and
      are restricted to alphanumeric characters or [".@-_+=,~"].*)
  val name : string

  (** A display-friendly text which describes what the event means. *)
  val doc : string

  (* Pretty printer for log messages.
     - [~all_fields:true] allows to print remaining fields not referenced in the
          event message after the message with the syntax
          (<field_name> = <field_as_string>)
     - [~block:true] wraps the message in an [hov2] to automatically cut lines
          and add indentation.
  *)
  val pp : all_fields:bool -> block:bool -> Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  (** Return the preferred {!type-level} for a given event instance. *)
  val level : level

  val alternative_color : alternative_color option
end

(** Events created with {!Make} provide the {!EVENT} API. *)
module type EVENT = sig
  include EVENT_DEFINITION

  (** Output an event of type {!t}, if no sinks are listening the
      function won't be applied. *)
  val emit : ?section:Section.t -> t -> unit tzresult Lwt.t

  (** Registers an event of type {!t} to be output when event sinks activate.
      Does not output anything if called after sink activation. *)
  val emit_at_top_level : t -> unit
end

(** Build an event from an event-definition. *)
module Make (E : EVENT_DEFINITION) : EVENT with type t = E.t

(** [event_definition] wraps {!EVENT_DEFINITION} as a first class module. *)
type 'a event_definition = (module EVENT_DEFINITION with type t = 'a)

(** Helper functions to manipulate all kinds of events in a generic way. *)
module Generic : sig
  type definition =
    | Definition :
        (Section.t option * string * 'a event_definition)
        -> definition

  type event = Event : (string * 'a event_definition * 'a) -> event

  type with_name = < doc : string ; name : string >

  (** Get the JSON schema (together with [name] and [doc]) of a given
      event definition. *)
  val json_schema : definition -> < schema : Json_schema.schema ; with_name >

  (** Get the JSON representation and a pretty-printer for a given
        event {i instance}. *)
  val explode_event :
    event ->
    < pp : Format.formatter -> unit -> unit
    ; json : Data_encoding.json
    ; with_name >
end

(** Access to all the event definitions registered with {!Make}. *)
module All_definitions : sig
  (** Get the list of all the known definitions. If [filter] is
  defined, use it to filter the returned events. *)
  val get :
    ?filter:(Generic.definition -> bool) -> unit -> Generic.definition list

  (** Find the definition matching on the given name. *)
  val find : (string -> bool) -> Generic.definition option
end

module Simple : sig
  (** Simple Event Definition *)

  (** This module provides wrappers to declare events without having to
      declare a module and apply the [Make] functor.
      They are pretty-printed as [<DOC> (<PARAMETER> = <VALUE>, ...)]
      (unless there is no parameter at all, in which case they are printed
      as [<DOC>]). You may also use braces to inline some parameters in [<DOC>].
      In the example below, [hash] is inlined.

      For consistency, it is suggested that log messages do not start with a
      capital letter and do not end with a period. For instance, write
      [~msg: "started something important"] instead of
      [~msg: "Started something important."]. The reason is that
      messages that do not inline all of their parameters are followed by
      the remaining parameters in parentheses, and a period does not
      (arguably) look great in those cases. If it does not end with a period,
      it is not a sentence and thus capitalizing the first word does not
      make much sense either.

      Declare events with one of the [declare] functions, for instance:
      [
        let inject =
          Internal_event.Simple.declare_2
            ~name: "inject"
            ~section:["foo"; "bar"]
            ~prefix_name_with_sections:false
            ~msg: "injected block {hash}"
            ("level", int)
            ("hash", string)
      ]
      You must declare events only once for a given name.
      Usually you should thus declare them as global variables.

      A way to enforce name uniqueness is to set [prefix_name_with_sections] to [true]. 
      In the example above, this would compute a new event name of the form "foo.bar.inject".

      There is one [declare_n] function for each number [n] of parameters.
      For instance, the above example uses [declare_2] because
      it has two parameters: [level] and [hash].

      Each parameter has a default pretty-printer that you can override
      using the [?ppX] parameters. For instance, to override the default
      pretty-printer of the second parameter, pass a [~pp2] argument.
      This allows to, for instance, print only the first element of a list,
      remove quotes for strings, decide how many decimal digits are printed
      for floats, etc.

      The default pretty-printer behaves as follows:
      - for base types (booleans, integers, strings, floats), it uses OCaml syntax;
      - for strings which are longer than 64 characters, it prints only the first characters
        followed by an ellipsis [[...]];
      - for null or similar encodings, it prints [N/A];
      - for unions of [Null] and another encoding [e] (this is the encoding of options),
        it prints [null] for [Null] and uses [e] to decide how to print the value otherwise;
      - for other constructed values (arrays, lists, objects, tuples, unions, and recursive
        types), it prints a placeholder like [<list>] or [<obj>] (see remark below).

      Because constructed values such as lists and objects are printed as placeholders
      such as [<list>] and [<obj>] and thus do not give any information, those parameters
      are not automatically added in parentheses at the end of the messages.
      You can override the default pretty-printer of those parameters to override
      this behavior. In that case, values will be printed if your pretty-printer does
      not output an empty string. Note that you should also override the default
      pretty-printer of constructed types that you inline using braces, as they would
      be printed using the placeholder otherwise.

      Then emit this event with some parameters like this:
      [Internal_event.Simple.emit inject (42, "BL654654654645654654564")]
      This event will be printed as:
      [injected block BL654654654645654654564 (level = 42)]

      For all [declare] functions, the default value for [level] is [Info]. *)

  (** Event declarations where ['a] is the type of the event parameters. *)
  type 'a t

  (** Emit an instance of an event. *)
  val emit : 'a t -> 'a -> unit Lwt.t

  (** Emit an instance of an event if called at toplevel *)
  val emit_at_top_level : 'a t -> 'a -> unit

  (** Emit an instance of an event but do not wait for completion. Only use if
      you really need to not wait for logging resolution before continuing. May
      cause increased memory usage and out-of-order log messages. *)
  val emit__dont_wait__use_with_care : 'a t -> 'a -> unit

  (** Declare an event with no parameters. *)
  val declare_0 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    unit ->
    unit t

  (** Declare an event with one parameter. *)
  val declare_1 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    ?pp1:(Format.formatter -> 'a -> unit) ->
    string * 'a Data_encoding.t ->
    'a t

  (** Declare an event with two parameters. *)
  val declare_2 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    ?pp1:(Format.formatter -> 'a -> unit) ->
    string * 'a Data_encoding.t ->
    ?pp2:(Format.formatter -> 'b -> unit) ->
    string * 'b Data_encoding.t ->
    ('a * 'b) t

  (** Declare an event with three parameters. *)
  val declare_3 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    ?pp1:(Format.formatter -> 'a -> unit) ->
    string * 'a Data_encoding.t ->
    ?pp2:(Format.formatter -> 'b -> unit) ->
    string * 'b Data_encoding.t ->
    ?pp3:(Format.formatter -> 'c -> unit) ->
    string * 'c Data_encoding.t ->
    ('a * 'b * 'c) t

  (** Declare an event with four parameters. *)
  val declare_4 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    ?pp1:(Format.formatter -> 'a -> unit) ->
    string * 'a Data_encoding.t ->
    ?pp2:(Format.formatter -> 'b -> unit) ->
    string * 'b Data_encoding.t ->
    ?pp3:(Format.formatter -> 'c -> unit) ->
    string * 'c Data_encoding.t ->
    ?pp4:(Format.formatter -> 'd -> unit) ->
    string * 'd Data_encoding.t ->
    ('a * 'b * 'c * 'd) t

  (** Declare an event with five parameters. *)
  val declare_5 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    ?pp1:(Format.formatter -> 'a -> unit) ->
    string * 'a Data_encoding.t ->
    ?pp2:(Format.formatter -> 'b -> unit) ->
    string * 'b Data_encoding.t ->
    ?pp3:(Format.formatter -> 'c -> unit) ->
    string * 'c Data_encoding.t ->
    ?pp4:(Format.formatter -> 'd -> unit) ->
    string * 'd Data_encoding.t ->
    ?pp5:(Format.formatter -> 'e -> unit) ->
    string * 'e Data_encoding.t ->
    ('a * 'b * 'c * 'd * 'e) t

  (** Declare an event with six parameters. *)
  val declare_6 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    ?pp1:(Format.formatter -> 'a -> unit) ->
    string * 'a Data_encoding.t ->
    ?pp2:(Format.formatter -> 'b -> unit) ->
    string * 'b Data_encoding.t ->
    ?pp3:(Format.formatter -> 'c -> unit) ->
    string * 'c Data_encoding.t ->
    ?pp4:(Format.formatter -> 'd -> unit) ->
    string * 'd Data_encoding.t ->
    ?pp5:(Format.formatter -> 'e -> unit) ->
    string * 'e Data_encoding.t ->
    ?pp6:(Format.formatter -> 'f -> unit) ->
    string * 'f Data_encoding.t ->
    ('a * 'b * 'c * 'd * 'e * 'f) t

  (** Declare an event with seven parameters. *)
  val declare_7 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    ?pp1:(Format.formatter -> 'a -> unit) ->
    string * 'a Data_encoding.t ->
    ?pp2:(Format.formatter -> 'b -> unit) ->
    string * 'b Data_encoding.t ->
    ?pp3:(Format.formatter -> 'c -> unit) ->
    string * 'c Data_encoding.t ->
    ?pp4:(Format.formatter -> 'd -> unit) ->
    string * 'd Data_encoding.t ->
    ?pp5:(Format.formatter -> 'e -> unit) ->
    string * 'e Data_encoding.t ->
    ?pp6:(Format.formatter -> 'f -> unit) ->
    string * 'f Data_encoding.t ->
    ?pp7:(Format.formatter -> 'g -> unit) ->
    string * 'g Data_encoding.t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) t

  (** Declare an event with eight parameters. *)
  val declare_8 :
    ?alternative_color:alternative_color ->
    ?section:string list ->
    ?prefix_name_with_section:bool ->
    name:string ->
    msg:string ->
    ?level:level ->
    ?pp1:(Format.formatter -> 'a -> unit) ->
    string * 'a Data_encoding.t ->
    ?pp2:(Format.formatter -> 'b -> unit) ->
    string * 'b Data_encoding.t ->
    ?pp3:(Format.formatter -> 'c -> unit) ->
    string * 'c Data_encoding.t ->
    ?pp4:(Format.formatter -> 'd -> unit) ->
    string * 'd Data_encoding.t ->
    ?pp5:(Format.formatter -> 'e -> unit) ->
    string * 'e Data_encoding.t ->
    ?pp6:(Format.formatter -> 'f -> unit) ->
    string * 'f Data_encoding.t ->
    ?pp7:(Format.formatter -> 'g -> unit) ->
    string * 'g Data_encoding.t ->
    ?pp8:(Format.formatter -> 'h -> unit) ->
    string * 'h Data_encoding.t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t
end

(** {3 Sink Definitions and Registration } *)

(** An implementation of {!SINK} is responsible for handling/storing
    events, for instance, a sink could be output to a file, to a
    database, or a simple "memory-less" forwarding mechanism.  *)
module type SINK = sig
  (** A sink can store any required state, e.g. a database handle, in
      a value of the [t] type see {!configure}. *)
  type t

  (** Registered sinks are a distinguished by their URI scheme. *)
  val uri_scheme : string

  (** When a registered sink is activated the {!configure} function is
      called to initialize it. The parameters should be encoded or
      obtained from the URI (the scheme of the URI is already
      {!uri_scheme}). *)
  val configure : Uri.t -> t tzresult Lwt.t

  (** Predicate deciding whether a sink should handle the event or
      not. *)
  val should_handle : ?section:Section.t -> t -> _ event_definition -> bool

  (** A sink's main function is to {!handle} incoming events from the
      code base. *)
  val handle :
    t -> 'a event_definition -> ?section:Section.t -> 'a -> unit tzresult Lwt.t

  (** A function to be called on graceful termination of processes
      (e.g. to flush file-descriptors, etc.). *)
  val close : t -> unit tzresult Lwt.t
end

(** [sink_definition] wraps {!SINK_DEFINITION} as a first class module. *)
type 'a sink_definition = (module SINK with type t = 'a)

(** Use {!All_sinks.register} to add a new {i inactive} sink, then
    {!All_sinks.activate} to make it handle events. *)
module All_sinks : sig
  (** Register a new sink (e.g.
      [let () = Internal_event.All_sinks.register (module Sink_implementation)])
      for it to be available (but inactive) in the framework. *)
  val register : 'a sink_definition -> unit

  (** Make a registered sink active: the function finds it by URI
      scheme and calls {!configure}. *)
  val activate : Uri.t -> unit tzresult Lwt.t

  (** Call [close] on all the sinks, except the ones matching the
      predicate [?except] (default: all of them). *)
  val close : ?except:(Uri.t -> bool) -> unit -> unit tzresult Lwt.t

  (** Display the state of registered/active sinks. *)
  val pp_state : Format.formatter -> unit -> unit
end

(** {3 Common Event Definitions } *)

(** The worker logger is meant for use with {!Lwt_utils.worker}. *)
module Lwt_worker_logger : sig
  (** [on_event status] emits an event of type [t] and matches
      the signature required by {!Lwt_utils.worker}. *)
  val on_event : string -> [`Started | `Ended | `Failed of string] -> unit Lwt.t
end
