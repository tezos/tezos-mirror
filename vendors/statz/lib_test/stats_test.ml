open StaTz

let coin = Stats.coin ~bias:0.5

let _ =
  Format.printf
    "biased coin:\n%a@."
    (Stats.pp_fin_fun Format.pp_print_bool)
    (coin :> bool Stats.fin_fun)

let bin = Stats.binomial coin 7

let _ =
  Format.printf
    "binomial law on {0; ...; 6}:\n%a@."
    (Stats.pp_fin_fun Format.pp_print_int)
    (bin :> int Stats.fin_fun)
