type 'a t = {
  mutable len: int;
  mutable l: 'a list list;
  mutable started: Mtime.t;
}

let create () = { len = 0; l = []; started = Mtime_clock.now () }

let push self l =
  if l != [] then (
    if self.l == [] then self.started <- Mtime_clock.now ();
    self.l <- l :: self.l;
    self.len <- self.len + List.length l
  )

let[@inline] len self = self.len

let[@inline] time_started self = self.started

let pop_all self =
  let l = self.l in
  self.l <- [];
  self.len <- 0;
  l
