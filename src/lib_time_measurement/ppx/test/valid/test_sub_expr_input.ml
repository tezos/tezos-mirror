let _ =
  (let x = ("coucou" [@time.duration sub_expr]) in
   x)
  [@time.duration expr]
