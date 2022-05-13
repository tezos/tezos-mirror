let c_headers =
  "\n\
   #include <stdbool.h>\n\
   #include <stddef.h>\n\
   #include \"librustzcash.h\"\n\
   #ifndef _Bool\n\
  \  #define _Bool bool\n\
   #endif\n"

let () =
  let ml_filename, c_filename = (Sys.argv.(1), Sys.argv.(2)) in
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
    ~prefix:""
    (module Rustzcash_ctypes_bindings.Bindings) ;
  Format.fprintf c_formatter "%s@\n" c_headers ;
  Cstubs.write_c
    c_formatter
    ~errno:Cstubs.ignore_errno
    ~concurrency:Cstubs.sequential
    ~prefix:""
    (module Rustzcash_ctypes_bindings.Bindings) ;
  close_out ml_out ;
  close_out c_out
