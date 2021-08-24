let f x y z = x + y + z

let l = ["1"; "2"; "3"]

let _ = f 1 2 3 [@time.timestamp_pre fun_app l]
