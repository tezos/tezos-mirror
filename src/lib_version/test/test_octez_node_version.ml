(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    _______
    Component: lib_version
    Invocation: dune exec src/lib_version/test/main.exe \
                  -- --file test_octez_node_version.ml
    Subject: Test versions
*)
let all_products : Tezos_version_parser.product array =
  [|Octez; Octez_evm_node|]

(* Values of Beta x, Beta_dev x, RC x or RC_dev x in additional_info are
   ignored. *)
let version_generator ?(product = all_products) ?major ?minor ?build_number
    ?(additional_info =
      ([|Dev; Beta 0; Beta_dev 0; RC 0; RC_dev 0; Release|]
        : Tezos_version_parser.additional_info array)) ?additional_info_value ()
    =
  let product = QCheck2.Gen.oneofa product in
  let major = Option.fold ~none:QCheck2.Gen.int ~some:QCheck2.Gen.pure major in
  let minor = Option.fold ~none:QCheck2.Gen.int ~some:QCheck2.Gen.pure minor in
  let build_number =
    Option.fold ~none:QCheck2.Gen.int ~some:QCheck2.Gen.pure build_number
  in
  let additional_info_value =
    Option.fold
      ~none:QCheck2.Gen.int
      ~some:QCheck2.Gen.pure
      additional_info_value
  in
  let additional_info =
    let open Tezos_version_parser in
    QCheck2.Gen.map2
      (fun additional_info v ->
        match additional_info with
        | Beta _ -> Beta v
        | Beta_dev _ -> Beta_dev v
        | RC _ -> RC v
        | RC_dev _ -> RC_dev v
        | x -> x)
      (QCheck2.Gen.oneofa additional_info)
      additional_info_value
  in
  QCheck2.Gen.map
    (fun (product, major, minor, build_number, additional_info) ->
      Tezos_version_parser.
        {product; major; minor; build_number; additional_info})
    (QCheck2.Gen.tup5 product major minor build_number additional_info)

let commit_info_opt_generator ?(always_some = false) () =
  let hash = QCheck2.Gen.small_string ?gen:None in
  let date = QCheck2.Gen.small_string ?gen:None in
  let commit_info =
    QCheck2.Gen.map2
      (fun commit_hash commit_date : Octez_node_version.commit_info ->
        {commit_hash; commit_date})
      hash
      date
  in
  let commit_info_opt =
    if always_some then QCheck2.Gen.map (fun c -> Some c) commit_info
    else QCheck2.Gen.option commit_info
  in
  commit_info_opt

let commit_info_opt_pp =
  Format.pp_print_option
    ~none:(fun ppf () -> Format.pp_print_string ppf "none")
    Octez_node_version.commit_info_pp

let commit_info_opt_different c1 c2 =
  match (c1, c2) with
  | Some c1, Some c2 -> not (Octez_node_version.commit_info_equivalent c1 c2)
  | None, None -> true
  | _ -> false

let print_opt ?v1 ?c1 ?v2 ?c2 () =
  let pp_opt msg pp fmt = function
    | None -> Format.fprintf fmt "@,"
    | Some v -> Format.fprintf fmt "%s: %a@," msg pp v
  in
  Format.asprintf
    "%a%a%a%a"
    (pp_opt "version1" Tezos_version_parser.pp)
    v1
    (pp_opt "commit1" commit_info_opt_pp)
    c1
    (pp_opt "version2" Tezos_version_parser.pp)
    v2
    (pp_opt "commit2" commit_info_opt_pp)
    c2

let print (v1, c1, v2, c2) = print_opt ~v1 ~c1 ~v2 ~c2 ()

(* Test reciprocity: ab uncomparable the ba uncomparable | a=b then b=a | if
   a>b then b<a *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    QCheck2.Gen.tup4
      (version_generator ())
      (commit_info_opt_generator ())
      (version_generator ())
      (commit_info_opt_generator ())
  in
  QCheck2.Test.make ~name:"partially_compare reciprocity" ~print generator
  @@ fun (v1, c1, v2, c2) ->
  match
    ( Octez_node_version.partially_compare v1 c1 v2 c2,
      Octez_node_version.partially_compare v2 c2 v1 c1 )
  with
  | None, None | Some 0, Some 0 -> true
  | Some x, Some y when x < 0 && y > 0 -> true
  | Some x, Some y when x > 0 && y < 0 -> true
  | _ -> QCheck2.Test.fail_reportf "this operator should have reciprocity"

(* Test uncomparable cases *)
(* Test same version but different commit info *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    QCheck2.Gen.tup3
      (version_generator ())
      (commit_info_opt_generator ~always_some:true ())
      (commit_info_opt_generator ~always_some:true ())
  in
  let print (v1, c1, c2) = print_opt ~v1 ~c1 ~c2 () in
  QCheck2.Test.make
    ~name:"partially_compare uncomparable different existing commits"
    ~print
    generator
  @@ fun (v, c1, c2) ->
  QCheck2.assume (commit_info_opt_different c1 c2) ;
  match Octez_node_version.partially_compare v c1 v c2 with
  | None -> true
  | _ -> QCheck2.Test.fail_reportf "These versions should be uncomparable"

(* Test same version but different commit info or no commit info for dev *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    QCheck2.Gen.tup3
      (version_generator ~additional_info:[|Dev; Beta_dev 0; RC_dev 0|] ())
      (commit_info_opt_generator ())
      (commit_info_opt_generator ())
  in
  let print (v1, c1, c2) = print_opt ~v1 ~c1 ~c2 () in
  QCheck2.Test.make
    ~name:"partially_compare uncomparable different commits"
    ~print
    generator
  @@ fun (v, c1, c2) ->
  QCheck2.assume (commit_info_opt_different c1 c2) ;
  match Octez_node_version.partially_compare v c1 v c2 with
  | None -> true
  | _ -> QCheck2.Test.fail_reportf "These versions should be uncomparable"

(* Test different products *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let products =
    QCheck2.Gen.map
      (fun a -> ([|a.(0)|], Array.sub a 1 (Array.length a - 1)))
      (QCheck2.Gen.shuffle_a all_products)
  in
  let generator =
    QCheck2.Gen.bind products (fun (p1, p2) ->
        QCheck2.Gen.tup3
          (version_generator ~product:p1 ())
          (commit_info_opt_generator ())
          (version_generator ~product:p2 ()))
  in
  let print (v1, c1, v2) = print_opt ~v1 ~c1 ~v2 () in
  QCheck2.Test.make
    ~name:"partially_compare uncomparable different products"
    ~print
    generator
  @@ fun (v1, c, v2) ->
  match Octez_node_version.partially_compare v1 c v2 c with
  | None -> true
  | _ -> QCheck2.Test.fail_reportf "These versions should be uncomparable"

(* Test dev and different commit info *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    QCheck2.Gen.bind (QCheck2.Gen.oneofa all_products) (fun p ->
        let product = [|p|] in
        QCheck2.Gen.tup4
          (version_generator
             ~product
             ~additional_info:[|Dev; Beta_dev 0; RC_dev 0|]
             ())
          (commit_info_opt_generator ())
          (version_generator ~product ())
          (commit_info_opt_generator ()))
  in
  QCheck2.Test.make
    ~name:"partially_compare uncomparable additional_info"
    ~print
    generator
  @@ fun (v1, c1, v2, c2) ->
  QCheck2.assume (v1 <> v2) ;
  QCheck2.assume (commit_info_opt_different c1 c2) ;
  match Octez_node_version.partially_compare v1 c1 v2 c2 with
  | None -> true
  | _ -> QCheck2.Test.fail_reportf "These versions should be uncomparable"

(* Test comparable cases *)
(* Test equal cases *)
(* Test equality with commit info *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    QCheck2.Gen.tup2
      (version_generator ())
      (commit_info_opt_generator ~always_some:true ())
  in
  let print (v1, c1) = print_opt ~v1 ~c1 () in
  QCheck2.Test.make
    ~name:"partially_compare comparable equality"
    ~print
    generator
  @@ fun (v, c) ->
  match Octez_node_version.partially_compare v c v c with
  | Some 0 -> true
  | _ -> QCheck2.Test.fail_reportf "These versions should be equal"

(* Test equality not dev *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    QCheck2.Gen.tup2
      (version_generator ~additional_info:[|Beta 0; RC 0; Release|] ())
      (commit_info_opt_generator ())
  in
  let print (v1, c1) = print_opt ~v1 ~c1 () in
  QCheck2.Test.make
    ~name:"partially_compare comparable equality no commit"
    ~print
    generator
  @@ fun (v, c) ->
  match Octez_node_version.partially_compare v None v c with
  | Some 0 -> true
  | _ -> QCheck2.Test.fail_reportf "These versions should be equal"

(* Test not equal cases *)
(* Test different major *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    let product = QCheck2.Gen.oneofa all_products in
    QCheck2.Gen.bind
      (QCheck2.Gen.tup3 product QCheck2.Gen.int QCheck2.Gen.int)
      (fun (p, x, y) ->
        let product = [|p|] in
        let major1, major2 = if x < y then (x, y) else (y, x) in
        QCheck2.Gen.tup4
          (version_generator
             ~product
             ~major:major1
             ~additional_info:[|Beta 0; RC 0; Release|]
             ())
          (commit_info_opt_generator ())
          (version_generator
             ~product
             ~major:major2
             ~additional_info:[|Beta 0; RC 0; Release|]
             ())
          (commit_info_opt_generator ()))
  in
  QCheck2.Test.make ~name:"partially_compare different major" ~print generator
  @@ fun (v1, c1, v2, c2) ->
  QCheck2.assume (v1.major <> v2.major) ;
  match Octez_node_version.partially_compare v1 c1 v2 c2 with
  | Some x when x < 0 -> (
      match Octez_node_version.partially_compare v2 c2 v1 c1 with
      | Some x when x > 0 -> true
      | _ ->
          QCheck2.Test.fail_reportf "version2 should be greater than version1")
  | _ -> QCheck2.Test.fail_reportf "version1 should be less than version2"

(* Test same major, different minor *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    let product = QCheck2.Gen.oneofa all_products in
    QCheck2.Gen.bind
      (QCheck2.Gen.tup4 product QCheck2.Gen.int QCheck2.Gen.int QCheck2.Gen.int)
      (fun (p, major, x, y) ->
        let product = [|p|] in
        let minor1, minor2 = if x < y then (x, y) else (y, x) in
        QCheck2.Gen.tup4
          (version_generator
             ~product
             ~major
             ~minor:minor1
             ~additional_info:[|Beta 0; RC 0; Release|]
             ())
          (commit_info_opt_generator ())
          (version_generator
             ~product
             ~major
             ~minor:minor2
             ~additional_info:[|Beta 0; RC 0; Release|]
             ())
          (commit_info_opt_generator ()))
  in
  QCheck2.Test.make ~name:"partially_compare different minor" ~print generator
  @@ fun (v1, c1, v2, c2) ->
  QCheck2.assume (v1.minor <> v2.minor) ;
  match Octez_node_version.partially_compare v1 c1 v2 c2 with
  | Some x when x < 0 -> (
      match Octez_node_version.partially_compare v2 c2 v1 c1 with
      | Some x when x > 0 -> true
      | _ ->
          QCheck2.Test.fail_reportf "version2 should be greater than version1")
  | _ -> QCheck2.Test.fail_reportf "version1 should be less than version2"

(* Test same major, minor same additional_info but diff additional_info inner
   value *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    let product = QCheck2.Gen.oneofa all_products in
    let additional_info =
      QCheck2.Gen.oneofa Tezos_version_parser.[|Beta 0; RC 0|]
    in
    QCheck2.Gen.bind
      (QCheck2.Gen.tup6
         product
         QCheck2.Gen.int
         QCheck2.Gen.int
         additional_info
         QCheck2.Gen.int
         QCheck2.Gen.int)
      (fun (p, major, minor, ai, x, y) ->
        let product = [|p|] in
        let additional_info = [|ai|] in
        let beta1, beta2 = if x < y then (x, y) else (y, x) in
        QCheck2.Gen.tup4
          (version_generator
             ~product
             ~major
             ~minor
             ~additional_info
             ~additional_info_value:beta1
             ())
          (commit_info_opt_generator ())
          (version_generator
             ~product
             ~major
             ~minor
             ~additional_info
             ~additional_info_value:beta2
             ())
          (commit_info_opt_generator ()))
  in
  QCheck2.Test.make
    ~name:"partially_compare different additional_info inner version"
    ~print
    generator
  @@ fun (v1, c1, v2, c2) ->
  QCheck2.assume (v1.minor <> v2.minor) ;
  match Octez_node_version.partially_compare v1 c1 v2 c2 with
  | Some x when x < 0 -> (
      match Octez_node_version.partially_compare v2 c2 v1 c1 with
      | Some x when x > 0 -> true
      | _ ->
          QCheck2.Test.fail_reportf "version2 should be greater than version1")
  | _ -> QCheck2.Test.fail_reportf "version1 should be less than version2"

(* Test Beta < RC or Release *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    let product = QCheck2.Gen.oneofa all_products in
    QCheck2.Gen.bind
      (QCheck2.Gen.tup3 product QCheck2.Gen.int QCheck2.Gen.int)
      (fun (p, major, minor) ->
        let product = [|p|] in
        QCheck2.Gen.tup4
          (version_generator
             ~product
             ~major
             ~minor
             ~additional_info:[|Beta 0|]
             ())
          (commit_info_opt_generator ())
          (version_generator
             ~product
             ~major
             ~minor
             ~additional_info:[|RC 0; Release|]
             ())
          (commit_info_opt_generator ()))
  in
  QCheck2.Test.make
    ~name:"partially_compare beta < RC or Release"
    ~print
    generator
  @@ fun (v1, c1, v2, c2) ->
  QCheck2.assume (v1.minor <> v2.minor) ;
  match Octez_node_version.partially_compare v1 c1 v2 c2 with
  | Some x when x < 0 -> (
      match Octez_node_version.partially_compare v2 c2 v1 c1 with
      | Some x when x > 0 -> true
      | _ ->
          QCheck2.Test.fail_reportf "version2 should be greater than version1")
  | _ -> QCheck2.Test.fail_reportf "version1 should be less than version2"

(* Test RC < Release *)
let () =
  Qcheck_tezt.register ~__FILE__ ~tags:["version"]
  @@
  let generator =
    let product = QCheck2.Gen.oneofa all_products in
    QCheck2.Gen.bind
      (QCheck2.Gen.tup3 product QCheck2.Gen.int QCheck2.Gen.int)
      (fun (p, major, minor) ->
        let product = [|p|] in
        QCheck2.Gen.tup4
          (version_generator
             ~product
             ~major
             ~minor
             ~additional_info:[|RC 0|]
             ())
          (commit_info_opt_generator ())
          (version_generator
             ~product
             ~major
             ~minor
             ~additional_info:[|Release|]
             ())
          (commit_info_opt_generator ()))
  in
  QCheck2.Test.make ~name:"partially_compare RC<Release" ~print generator
  @@ fun (v1, c1, v2, c2) ->
  QCheck2.assume (v1.minor <> v2.minor) ;
  match Octez_node_version.partially_compare v1 c1 v2 c2 with
  | Some x when x < 0 -> (
      match Octez_node_version.partially_compare v2 c2 v1 c1 with
      | Some x when x > 0 -> true
      | _ ->
          QCheck2.Test.fail_reportf "version2 should be greater than version1")
  | _ -> QCheck2.Test.fail_reportf "version1 should be less than version2"
