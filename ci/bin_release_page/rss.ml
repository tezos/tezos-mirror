(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Base

type item = {title : string; description : string; guid : string}

let make_item ~title ~description ~guid = {title; description; guid}

type channel = {
  title : string;
  description : string;
  lastBuildDate : Unix.tm;
  items : item list;
}

let make_channel ~title ~description ~lastBuildDate ~items =
  {title; description; lastBuildDate; items}

(* [escape_xml_text text] escapes special XML characters in text *)
let escape_xml_text text =
  let buf = Buffer.create (String.length text) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&apos;"
      | c -> Buffer.add_char buf c)
    text ;
  Buffer.contents buf

(* [show_item_rss channel] converts [item] to string.*)
let show_item_rss {title; description; guid} =
  sf
    "    <item>\n\
    \      <title>%s</title>\n\
    \      <description>%s</description>\n\
    \      <guid>%s</guid>\n\
    \    </item>"
    (escape_xml_text title)
    (escape_xml_text description)
    (escape_xml_text guid)

let show_time (time : Unix.tm) =
  sf
    "%s, %02d %s %04d %02d:%02d:%02d GMT"
    (match time.tm_wday with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> "Sun")
    time.tm_mday
    (match time.tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> "Jan")
    (1900 + time.tm_year)
    time.tm_hour
    time.tm_min
    time.tm_sec

(* [show_channel_rss channel] converts [channel] to string. *)
let show_channel_rss {title; description; lastBuildDate; items} =
  Format.asprintf
    {|  <channel>
          <title>%s</title>
          <description>%s</description>
          <language>en</language>
          <lastBuildDate>%s</lastBuildDate>
          %a
        </channel>|}
    (escape_xml_text title)
    (escape_xml_text description)
    (show_time lastBuildDate)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n          ")
       (fun fmt item -> Format.pp_print_string fmt (show_item_rss item)))
    items

(* [show_rss channel] generates a RSS string from [channel].*)
let show_rss channel =
  sf
    {|<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0">
    %s
</rss>|}
    (show_channel_rss channel)

(* [generate_rss channel] generates and writes a rss file from [channel].*)
let generate_rss channel =
  let output_file = "feed.xml" in
  Format.printf "Generating RSS feed...@." ;
  let rss_content = show_rss channel in
  try
    if Sys.file_exists output_file then
      Format.printf
        "Warning: %s already exists. It will be erased@."
        rss_content ;
    let feed_file = open_out output_file in
    Fun.protect ~finally:(fun () -> close_out feed_file) @@ fun () ->
    output_string feed_file rss_content ;
    Printf.printf "RSS feed generated: %s\n%!" output_file
  with Sys_error msg ->
    failwith (sf "System error writing %s: %s" output_file msg)
