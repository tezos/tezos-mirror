module type PARAMETERS = sig
  (** The state size *)
  val width : int

  (** The number of rounds *)
  val rounds : int

  (** The round constants, given in decimal representation *)
  val round_constants : string array

  (** The MDS matrix, given in decimal representation *)
  val mds_matrix : string array array

  val alpha : Z.t

  val alphainv : Z.t
end

module Make (Param : PARAMETERS) (Scalar : Bls12_381.Ff_sig.PRIME) = struct
  open Param

  (* Verify the constants are consistent *)
  let () =
    assert (Array.length mds_matrix = width) ;
    assert (Array.for_all (fun line -> Array.length line = width) mds_matrix)

  let mds_matrix = Array.map (Array.map Scalar.of_string) mds_matrix

  let round_constants = Array.map Scalar.of_string round_constants

  (* Initialize only once an array for the MDS matrix multiplication *)
  let res = Array.make width Scalar.zero

  type state = {mutable i_round_key : int; state : Scalar.t array}

  let init state =
    if Array.length state != width then
      failwith
        (Printf.sprintf
           "State length is %d, but the width of the strategy is %d"
           (Array.length state)
           width)
    else {i_round_key = 0; state = Array.copy state}

  let get_next_round_key s =
    let v = round_constants.(s.i_round_key) in
    s.i_round_key <- s.i_round_key + 1 ;
    v

  (* Functions prefixed with apply_ are modifying the state given in
     parameters *)
  let apply_round_key s =
    let state = s.state in
    for i = 0 to Array.length state - 1 do
      state.(i) <- Scalar.(get_next_round_key s + state.(i))
    done

  let apply_nonlinear_alpha s =
    let s = s.state in
    for i = 0 to Array.length s - 1 do
      s.(i) <- Scalar.pow s.(i) alpha
    done

  let apply_nonlinear_alphainv s =
    let s = s.state in
    for i = 0 to Array.length s - 1 do
      s.(i) <- Scalar.pow s.(i) alphainv
    done

  let apply_linear m v =
    let v = v.state in
    for j = 0 to width - 1 do
      for k = 0 to width - 1 do
        res.(k) <- Scalar.(res.(k) + (m.(k).(j) * v.(j)))
      done
    done ;
    for j = 0 to width - 1 do
      v.(j) <- res.(j) ;
      res.(j) <- Scalar.zero
    done

  let apply_round s =
    apply_nonlinear_alpha s ;
    apply_linear mds_matrix s ;
    apply_round_key s ;
    apply_nonlinear_alphainv s ;
    apply_linear mds_matrix s ;
    apply_round_key s

  let apply s =
    s.i_round_key <- 0 ;
    for _i = 0 to rounds - 1 do
      apply_round s
    done

  let get s = Array.copy s.state
end
