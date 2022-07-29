(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let pp_title ~char ppf title =
  let sub = String.map (fun _ -> char) title in
  Format.fprintf ppf "%s@\n%s@\n@\n" title sub

let pp_h1 = pp_title ~char:'#'

let pp_h2 = pp_title ~char:'*'

let pp_h3 = pp_title ~char:'='

let pp_h4 = pp_title ~char:'`'

let pp_raw_html ppf str =
  Format.fprintf
    ppf
    "@[<v>.. raw:: html@\n  %s@]@\n"
    (Re.Str.global_replace (Re.Str.regexp "\n") "\n  " str)

let pp_html ppf f =
  Format.fprintf
    ppf
    "@[<v 2>.. raw:: html@ @ @[<h>%a@]@]@\n@\n"
    (fun ppf () -> f ppf)
    ()

let pp_ref ppf name = Format.fprintf ppf ".. _%s :@\n@\n" name

let style =
  {css|
<style>
  .rst-content .section ul p {
    margin-bottom: 0;
  }
  span.query {
    font-family: monospace;
    white-space: pre;
  }
  .wy-nav-content {
      max-width: 100%;
  }
  .tab  {
    overflow: hidden;
    border: 1px solid #ccc;
    background-color: #f1f1f1;
  }
  .tab .meth {
    display: inline;
    font-weight: bold;
    padding: 5px;
    margin-right:10px;
    color: white;
  }
  .tab .rpc-path {
    display: inline;
    font-weight: bold;
    font-size: 18px;
  }
  .get-meth {
    background-color: #61affe;
  }
  .post-meth {
    background: #49cc90;
  }
  .delete-meth {
    background: #f93e3e;
  }
  .put-meth {
    background: #fca130;
  }
  .patch-meth {
    background: #50e3c2;
  }
  .tab .dynamic-arg {
    display: inline;
    opacity:0.7;
  }
  .tabcontent {
    padding: 6px 12px;
    border: 1px solid #ccc;
    border-top: none;
  }
  .tabcontent h6 {
    margin: 5px 0px 0px 0px;
  }
  .tabcontent p {
     margin: 10px 0px 0px 10px;
  }
  .tabcontent ul {
     margin: 10px 0px 0px 10px;
  }
  pre {
    font-size: 12px
  }
</style>|css}
