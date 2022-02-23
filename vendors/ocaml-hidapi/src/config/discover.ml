module C = Configurator.V1

let () =
  C.main ~name:"hidapi" begin fun c ->
    let default : C.Pkg_config.package_conf =
      { libs   = ["-lhidapi"]
      ; cflags = ["-I/usr/include/hidapi"]
      }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
        match
          C.Pkg_config.query pc ~package:"hidapi-libusb",
          C.Pkg_config.query pc ~package:"hidapi" with
        | None, None -> default
        | Some a, _
        | None, Some a -> a
    in
    C.Flags.write_sexp "c_flags.sexp" conf.cflags ;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs
  end
