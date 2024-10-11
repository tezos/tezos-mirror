let lock_ : (unit -> unit) ref = ref ignore

let unlock_ : (unit -> unit) ref = ref ignore

let set_mutex ~lock ~unlock : unit =
  lock_ := lock;
  unlock_ := unlock

let[@inline] with_lock f =
  !lock_ ();
  Fun.protect ~finally:!unlock_ f
