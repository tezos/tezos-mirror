open Hidapi

let () =
  init () ;
  let devs = Hidapi.enumerate () in
  ListLabels.iter devs ~f:begin fun { path; vendor_id; product_id;
                                      serial_number; release_number;
                                      manufacturer_string; product_string;
                                      interface_number ; _ } ->
    let s = match serial_number with None -> "" | Some s -> s in
    let m = match manufacturer_string with None -> "" | Some s -> s in
    let p = match product_string with None -> "" | Some s -> s in
    Printf.printf "%s 0x%04x 0x%04x %s %d %s %s %d\n"
      path vendor_id product_id
      s release_number m p
      interface_number ;
    match open_id ~vendor_id ~product_id with
    | None ->
      Printf.printf "Impossible to open %d:%d\n" vendor_id product_id
    | Some d ->
      close d ;
      Printf.printf "Ok, opened/closed %d:%d\n" vendor_id product_id ;
  end ;
  deinit ()
