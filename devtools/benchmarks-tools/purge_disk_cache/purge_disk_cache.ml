let command s =
  let res = Sys.command s in
  if res <> 0 then failwith (Printf.sprintf "Error: %s returned %d" s res)

let () =
  command "/usr/bin/sync" ;

  (* echo 3 > /proc/sys/vm/drop_caches *)
  (* No creat! *)
  let oc = open_out_gen [Open_wronly] 0o100 "/proc/sys/vm/drop_caches" in
  output_string oc "3\n" ;
  close_out oc ;

  (* Hard drive cache is relatively small, so we can ignore it. *)
  (* command "hdparm -f /dev/sda"; *)
  command "/usr/bin/free"
