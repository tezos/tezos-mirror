type t =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `O of (string * t) list
  | `A of t list ]

let f = Format.fprintf

let rec pp fmt = function
  | `Null -> f fmt "null"
  | `Bool x -> f fmt "%b" x
  | `Float x -> f fmt "%g" x
  | `String x -> f fmt "%S" x
  | `O l ->
      f fmt "@[<hv 2>{@ " ;
      List.iteri
        (fun i (k, v) ->
          if i <> 0 then f fmt ",@ " ;
          f fmt "%S: %a" k pp v)
        l ;
      f fmt "@ }@]"
  | `A l ->
      f fmt "@[<hv 2>[@ " ;
      List.iteri
        (fun i x ->
          if i <> 0 then f fmt ",@ " ;
          f fmt "%a" pp x)
        l ;
      f fmt "@ ]@]"
