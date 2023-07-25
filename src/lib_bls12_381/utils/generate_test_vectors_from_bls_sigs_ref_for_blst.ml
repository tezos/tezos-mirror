let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done ;
    !lines
  with End_of_file ->
    close_in chan ;
    List.rev !lines

(* Both can be used i.e. MinPk or MinSig. They must share the same interface. *)
module type SIGNATURE_INSTANTIATION = module type of Bls12_381.Signature.MinPk

let sig_files (type a)
    (module SignatureM : SIGNATURE_INSTANTIATION with type signature = a)
    filenames (sign_fn : Bls12_381.Signature.sk -> Bytes.t -> a) =
  let generate filename =
    let contents = read_file filename in
    let output_filename = filename ^ "_blst" in
    let output_chan = open_out output_filename in
    List.iter
      (fun content ->
        let contents = String.split_on_char ' ' content in
        let msg_str, ikm_str, initial_res_str =
          (List.nth contents 0, List.nth contents 1, List.nth contents 2)
        in
        let msg = Hex.(to_bytes (`Hex msg_str)) in
        let ikm = Hex.to_bytes (`Hex ikm_str) in
        let res_str =
          if Bytes.length ikm < 32 then initial_res_str
          else
            let sk = Bls12_381.Signature.generate_sk ikm in
            let res = sign_fn sk msg in
            let res = SignatureM.signature_to_bytes res in
            Hex.(show (of_bytes res))
        in
        Printf.fprintf output_chan "%s %s %s\n" msg_str ikm_str res_str)
      contents ;
    close_out output_chan
  in
  List.iter generate filenames

let pop_files (module SignatureM : SIGNATURE_INSTANTIATION) filenames =
  let generate filename =
    let contents = read_file filename in
    let output_filename = filename ^ "_blst" in
    let output_chan = open_out output_filename in
    List.iter
      (fun content ->
        let contents = String.split_on_char ' ' content in
        let dummy_str, ikm_str, initial_res_str =
          (List.nth contents 0, List.nth contents 1, List.nth contents 2)
        in
        let ikm = Hex.to_bytes (`Hex ikm_str) in
        let res_str =
          if Bytes.length ikm < 32 then initial_res_str
          else
            let sk = Bls12_381.Signature.generate_sk ikm in
            Hex.(show (of_bytes (SignatureM.Pop.pop_prove sk)))
        in
        Printf.fprintf output_chan "%s %s %s\n" dummy_str ikm_str res_str)
      contents ;
    close_out output_chan
  in
  List.iter generate filenames

let exec (module SignatureM : SIGNATURE_INSTANTIATION) arg filenames =
  match arg with
  | "sig_aug" -> sig_files (module SignatureM) filenames SignatureM.Aug.sign
  | "sig_basic" -> sig_files (module SignatureM) filenames SignatureM.Basic.sign
  | "sig_pop" -> sig_files (module SignatureM) filenames SignatureM.Pop.sign
  | "pop" -> pop_files (module SignatureM) filenames
  | _ -> failwith "Use sig_g2_aug, sig_g2_basic, sig_g2_pop or pop_g2"

let () =
  let argc = Array.length Sys.argv in
  let filetype = Sys.argv.(1) in
  let instantiation = Sys.argv.(2) in
  let filenames = Array.to_list (Array.sub Sys.argv 3 (argc - 3)) in
  match instantiation with
  | "minPk" -> exec (module Bls12_381.Signature.MinPk) filetype filenames
  | "minSig" -> exec (module Bls12_381.Signature.MinSig) filetype filenames
  | _ -> failwith "instantiation must be either minPk or minSig"
