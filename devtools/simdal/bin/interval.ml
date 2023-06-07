(** [interval] is the type of inclusive intervals. *)
type t = {lo : int; hi : int}

let lo {lo; _} = lo

let hi {hi; _} = hi

let itv lo hi = {lo; hi}

let len {lo; hi} = hi - lo + 1

let mem x {lo; hi} = lo <= x && x <= hi

let centered mid delta =
  assert (delta >= 0) ;
  {lo = Int.max 0 (mid - delta); hi = mid + delta}

let mid {lo; hi} = lo + ((hi - lo) / 2)

let pp fmtr {lo; hi} = Format.fprintf fmtr "[%d; %d]" lo hi

let pp_short fmtr {lo; hi} = Format.fprintf fmtr "%d-%d" lo hi

let smul s {lo; hi} = {lo = s * lo; hi = s * hi}

let to_seq itv =
  let len = len itv in
  Seq.ints itv.lo |> Seq.take len
