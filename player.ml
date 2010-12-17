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

type t = {
  name      : string;
  avatar    : string option;
  mutable x : int;
  mutable y : int;
}

let pretty_string t =
  Printf.sprintf "%s: (%d, %d)" t.name t.x t.y

let x p = p.x
let y p = p.y

let move t = function
  | `up    -> if t.x < Constants.size_x - 1 then t.x <- t.x + 1
  | `down  -> if t.x > 0 then t.x <- t.x - 1
  | `right -> if t.y < Constants.size_y - 1 then t.y <- t.y + 1
  | `left  -> if t.y > 0 then t.y <- t.y - 1

let html_of_t p =
  let avatar = match p.avatar with 
    | None   -> "/sheep.png"
    | Some a -> a in
  <:html<
    <div class="player">
      <img src=$str:avatar$ alt=$str:p.name$/>
    </div>
  >>

let css_of_t = <:css<
  .player img {
    $Css.no_padding$;
    height: 4em;
  }
>>

let default = {
  name   = "Groarr";
  avatar = Some "/lion.png";
  x      = 0;
  y      = 0;
}

let c = ref 0

let random x y =
  incr c; {
    name   = "sheep" ^ string_of_int !c;
    avatar = Some "/sheep.png"; 
    x      = Random.int x;
    y      = Random.int y;
  }

let players = [  
  default;
  random Constants.size_x Constants.size_y;
  random Constants.size_x Constants.size_y;
  random Constants.size_x Constants.size_y;
  random Constants.size_x Constants.size_y;
  random Constants.size_x Constants.size_y;
  random Constants.size_x Constants.size_y;
  random Constants.size_x Constants.size_y;
  random Constants.size_x Constants.size_y;
  random Constants.size_x Constants.size_y;
]

let find name =
  Log.logmod "player" "looking for %s" name;
  try List.find (fun p -> p.name = name) players
  with _ -> default
