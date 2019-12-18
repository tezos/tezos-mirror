open Numerics

module Fvec =
struct
  module V = Float64.Vec

  let v1 = V.init 100 (fun i -> float i)
  let v2 = V.init 100 (fun i -> float i)

  let res = V.add v1 v2

  let _ =
    let s =
      V.to_array res
      |> Array.map string_of_float
      |> Array.to_list
      |> String.concat " ; " in
    Printf.printf "%s\n" s

  let res = V.mul v1 v2

  let _ =
    let s =
      V.to_array res
      |> Array.map string_of_float
      |> Array.to_list
      |> String.concat " ; " in
    Printf.printf "%s\n" s

  let res = V.linspace 0.0 100.0 100

  let _ =
    let s =
      V.to_array res
      |> Array.map string_of_float
      |> Array.to_list
      |> String.concat " ; " in
    Printf.printf "%s\n" s

end

module Cvec =
struct
  module V = Complex64.Vec

  let v1 = V.init 100 (fun i -> { Complex.re = float i ; im = float i })
  let v2 = V.init 100 (fun i -> { Complex.re = float i ; im = float i })

  let res = V.add v1 v2

  let string_of_complex { Complex.re ; im } =
    Printf.sprintf "%f+%fi" re im

  let _ =
    let s =
      V.to_array res
      |> Array.map string_of_complex
      |> Array.to_list
      |> String.concat " ; " in
    Printf.printf "%s\n" s

  let res = V.mul v1 v2

  let _ =
    let s =
      V.to_array res
      |> Array.map string_of_complex
      |> Array.to_list
      |> String.concat " ; " in
    Printf.printf "%s\n" s
end
