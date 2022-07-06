(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type STATIC = sig
  val worker_name : string
end

module Make
    (Event : Worker_intf.EVENT)
    (Request : Worker_intf.VIEW)
    (Static : STATIC) :
  Worker_intf.LOGGER with module Event = Event and module Request = Request =
struct
  module Event = Event
  module Request = Request

  type t = (Event.t * Internal_event.level) Time.System.stamped

  let encoding =
    let open Data_encoding in
    Time.System.stamped_encoding
    @@ obj2
         (req "event" @@ dynamic_size Event.encoding)
         (req "level" Internal_event.Level.encoding)

  let pp ppf (evt, _) = Format.fprintf ppf "%a" Event.pp evt

  module Definition : Internal_event.EVENT_DEFINITION with type t = t = struct
    let section = None

    let name = Static.worker_name

    type nonrec t = t

    let encoding =
      let open Data_encoding in
      let v0_encoding = encoding in
      With_version.(encoding ~name (first_version v0_encoding))

    let pp ~short:_ ppf (status : t) = Format.fprintf ppf "%a" pp status.data

    let doc = "Worker status."

    let level (status : t) = snd status.data
  end

  module LogEvent : Internal_event.EVENT with type t = t =
    Internal_event.Make (Definition)
end
