type t = {avg : float; std : float; min : float; max : float}

let proj r c =
  match c with `avg -> r.avg | `std -> r.std | `min -> r.min | `max -> r.max

let rescale s r =
  {avg = s *. r.avg; std = s *. r.std; min = s *. r.min; max = s *. r.max}

let of_arr vec =
  {
    avg = Stats.Emp.Float.empirical_mean vec;
    std = sqrt @@ Stats.Emp.Float.empirical_variance vec;
    min = Array.fold_left Float.min Float.max_float vec;
    max = Array.fold_left Float.max Float.min_float vec;
  }
