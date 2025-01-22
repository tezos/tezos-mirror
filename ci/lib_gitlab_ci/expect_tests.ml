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
          auto_cancel = None;
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
      Workflow
        {
          rules = [];
          name = Some "Woops";
          auto_cancel = Some {on_job_failure = true; on_new_commit = false};
        };
    ] ;
  [%expect
    {|
    workflow:
      name: Woops
      rules: []
      auto_cancel:
        on_job_failure: all |}] ;
  p
    [
      Types.Default
        {
          image = Some (Image "alpine-or-something");
          interruptible = Some false;
          retry = None;
        };
      Types.Stages ["first"];
      Types.Include
        [
          {subkey = Local "foo"; rules = []};
          {
            subkey = Local "bar";
            rules = [Util.include_rule ~changes:["src/**/*"] ()];
          };
          {subkey = Template Jobs_container_scanning; rules = []};
        ];
      Variables [("k1", "v"); ("k2", "vv")];
      Types.(
        Generic_job
          (Job
             (Util.job
                ~stage:"first"
                ~name:"jobbbbbbb"
                ~script:["rm -rf /"; "echo it is all gone now"]
                ())));
    ] ;
  [%expect
    {|
    default:
      image: alpine-or-something
      interruptible: false
    stages:
    - first
    include:
    - local: foo
    - local: bar
      rules:
      - changes:
        - src/**/*
        when: always
    - template: Jobs/Container-Scanning.gitlab-ci.yml
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
