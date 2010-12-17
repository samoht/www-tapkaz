(*
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Cow

type month = int

let html_of_month m =
  let str = match m with
    | 1  -> "Jan"
    | 2  -> "Feb"
    | 3  -> "Mar"
    | 4  -> "Apr"
    | 5  -> "May"
    | 6  -> "Jun"
    | 7  -> "Jul"
    | 8  -> "Aug"
    | 9  -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov"
    | 12 -> "Dec"
    | _  -> "???" in
  <:html<$str:str$>>

type date = {
  month : month;
  day   : int;
  year  : int;
  hour  : int;
  min   : int;
} with html

let date (year, month, day, hour, min) =
  { month; day; year; hour; min }

let css_of_date = <:css<
  .date {
    border: 1px solid #999; 
    line-height: 1; 
    width: 4em;
    position: relative;
    float: left;
    margin-right: 15px;
    text-align: center; 

    .month {
      text-transform: uppercase; 
      font-size: 1.2em;
      padding-top: 0.3em; 
    }
    .day {
      font-size: 2em;
    }
    .year { 
      background-color: #2358B8; 
      color: #FFF; 
      font-size: 1.2em; 
      padding: 0.3em 0; 
      margin-top: 0.3em;
    }
    .hour {
      display: none;
    }
    .min {
      display: none;
    }
  }
>>

type t = {
  date    : date;
  subject : string;
  sent_by : string;
  content : string;
} with html

let css_of_t = <:css<
  $css_of_date$;
  .subject {
    font-style: italic;
    display: inline;
  }
  .sent_by {
    font-weight: bold;
    display: inline;
  }
  .content {
    color: grey;
  }
>>

let messages : (string, t list) Hashtbl.t =
  Hashtbl.create 1024

let find_messages name =
  if Hashtbl.mem messages name then
    Hashtbl.find messages name
  else
    []

let add_message name m =
  let ms =
    if Hashtbl.mem messages name then
      m :: (Hashtbl.find messages name)
    else
      [m] in
  Hashtbl.replace messages name ms
  
let html post player =
  let message m =
    <:html<$html_of_t m$<hr/>&>> in
  Main.make <:html<
    $list:List.map message (find_messages player.Player.name)$
  >>
