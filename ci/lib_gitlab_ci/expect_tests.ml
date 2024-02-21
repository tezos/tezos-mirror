(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* used only for hosting tests *)

(** {2 Test {!To_yaml}} *)

let%expect_test _ =
  let p config =
    match To_yaml.to_yaml config |> Yaml.to_string with
    | Error (`Msg msg) -> Format.printf "ERROR: %s\n" msg
    | Ok s -> print_endline s
  in
  p [] ;
  [%expect {|
    {} |}] ;
  p
    [
      Workflow
        {
          rules = [Util.workflow_rule ~if_:If.(var "CI_SETTING" == null) ()];
          name = Some "setting_not_set";
        };
    ] ;
  [%expect
    {|
    workflow:
      name: setting_not_set
      rules:
      - if: $CI_SETTING == null
        when: always |}] ;
  p
    [
      Types.Default
        {image = Some (Image "alpine-or-something"); interruptible = Some false};
      Types.Stages ["first"];
      Types.Include
        [
          {local = "foo"; rules = []};
          {local = "bar"; rules = [Util.include_rule ~changes:["src/**/*"] ()]};
        ];
      Variables [("k1", "v"); ("k2", "vv")];
      Types.Job
        (Util.job
           ~stage:"first"
           ~name:"jobbbbbbb"
           ~script:["rm -rf /"; "echo it is all gone now"]
           ());
    ] ;
  [%expect
    {|
    default:
      image: alpine-or-something
      interruptible: false
    stages:
    - first
    include:
    - foo
    - local: bar
      rules:
      - changes:
        - src/**/*
        when: always
    variables:
      k1: v
      k2: vv
    jobbbbbbb:
      stage: first
      script:
      - rm -rf /
      - echo it is all gone now |}] ;
  ()

(** {2 Test {!If}-expressions} *)

let%expect_test _ =
  let p f_expr =
    try print_endline (If.encode (Lazy.force f_expr))
    with Invalid_argument msg -> print_endline ("Invalid_argument: " ^ msg)
  in
  p @@ lazy If.(var "Göteborg" == null) ;
  [%expect {| Invalid_argument: [Var.t] invalid variable name 'Göteborg' |}] ;
  p @@ lazy If.(var "foo" == str "This \"example\" is ok") ;
  [%expect {| $foo == 'This "example" is ok' |}] ;
  p @@ lazy If.(var "foo" == str "This 'example' is ok") ;
  [%expect {| $foo == "This 'example' is ok" |}] ;
  p @@ lazy If.(var "foo" == str "This \"example\" is 'not ok'") ;
  [%expect
    {| Invalid_argument: [If.str] literal strings cannot mix single and double-quotes, got "This \"example\" is 'not ok'". |}]
