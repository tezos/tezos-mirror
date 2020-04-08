let c_headers = "#include \"rustc_bls12_381.h\""

module AllBindings (F : Cstubs.FOREIGN) = struct
  include Rustc_bls12_381_bindings.Fq12 (F)
  include Rustc_bls12_381_bindings.Fr (F)
  include Rustc_bls12_381_bindings.G1 (F)
  include Rustc_bls12_381_bindings.G2 (F)
  include Rustc_bls12_381_bindings.Pairing (F)
end

let () =
  let (ml_filename, c_filename) = (Sys.argv.(1), Sys.argv.(2)) in
  let c_out = open_out_bin c_filename in
  let ml_out = open_out_bin ml_filename in
  let c_formatter = Format.formatter_of_out_channel c_out in
  let ml_formatter = Format.formatter_of_out_channel ml_out in
  (* ML file generation *)
  Format.set_formatter_out_channel ml_out ;
  Cstubs.write_ml
    ml_formatter
    ~errno:Cstubs.ignore_errno
    ~concurrency:Cstubs.sequential
    ~prefix:"caml"
    (module AllBindings) ;
  Format.fprintf c_formatter "%s@\n" c_headers ;
  Cstubs.write_c
    c_formatter
    ~errno:Cstubs.ignore_errno
    ~concurrency:Cstubs.sequential
    ~prefix:"caml"
    (module AllBindings) ;
  close_out ml_out ;
  close_out c_out
