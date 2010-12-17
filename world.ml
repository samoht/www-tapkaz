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

type state = {
  mutable water : float;
  mutable grass : float;
}

let make_state () = {
  water = Random.float 100.;
  grass = Random.float 100.;
  sand  = Random.float 100.;
}

type kind =
  | `forest
  | `lake
  | `desert

let kind_of_state s =
  if s.water < 
type elt = {
          x          : int;
          y          : int;
          state      : state;
  mutable z          : int;
  mutable neighbours : elt list;
  mutable players    : Player.t list;
}

let make_elt x y = {
  x; y ; z = 0;
  neighbours = [];
  players = [];
}

let html_of_elt c = <:html<
  <div class=$str:"elt" ^ string_of_int c.z$>
    ($int:c.x$,$int:c.y$,$flo:c.state.water$,$flo:c.state.grass$)
    $list:List.map Player.html_of_t c.players$
  </div>
>>

let css_of_elt = <:css<
  .elt0 {
    color: white;
    background-color: green;
    width: 6em;
    height: 6em;
  }
>>

type t = elt array array

let make x y =
  let w =
    Array.init x (fun i ->
      Array.init y (fun j ->
        make_elt i j)) in
  for i=0 to x-1 do
    for j=0 to y-1 do
      if i>0 then
        w.(i-1).(j).neighbours <- w.(i).(j) :: w.(i-1).(j).neighbours;
      if j>0 then
        w.(i).(j-1).neighbours <- w.(i).(j) :: w.(i).(j-1).neighbours;
      if i<x-1 then
        w.(i+1).(j).neighbours <- w.(i).(j) :: w.(i+1).(j).neighbours;
      if j<y-1 then
        w.(i).(j+1).neighbours <- w.(i).(j) :: w.(i).(j+1).neighbours;
    done
  done;
  w

let add_player t p =
  Log.logmod "world" "Adding player %s" (Player.pretty_string p);
  t.(p.Player.x).(p.Player.y).players <- p :: t.(p.Player.x).(p.Player.y).players

let rm_player t p =
  t.(p.Player.x).(p.Player.y).players <- List.filter ((!=) p) t.(p.Player.x).(p.Player.y).players

type view = {
  dx : int;
  dy : int;
}

let default_view = {
  dx = Constants.view_x;
  dy = Constants.view_y;
}

let html_of_t ?(view=default_view) player t =
  let x = Player.x player in
  let y = Player.y player in
  let dx1 = min x view.dx in
  let dx2 = min (Array.length t - x) view.dx in
  let dy1 = min y view.dy in
  let dy2 = min (Array.length t.(0) - y) view.dy in
  let size_x = dx1 + dx2 + 1 in
  let size_y = dy1 + dy2 + 1 in
  let v = Array.make_matrix size_x size_y Html.nil in
  for i = 0 to size_x - 1 do
    for j = 0 to size_y - 1 do
      v.(size_x - i - 1).(j) <- html_of_elt t.(x - dx1 + i).(y - dy1 +j)
    done
  done;
  <:html<
    <div class="world">$Html.html_of_table v$</div>
  >>

let css_of_t = <:css< $css_of_elt$ >>

let world = 
  let w = make Constants.size_x Constants.size_y in
  List.iter (add_player w) Player.players;
  w

module Action = struct
  type t = 
    [ `up
    | `down
    | `left
    | `right ]

  let of_string = function
    | "up"    -> `up
    | "down"  -> `down
    | "right" -> `right
    | "left"  -> `left
    | s       -> failwith (s ^": unknown map action")

  let process world player (k,v) =
    Log.logmod "action" "process k=%s v=%s" k v;
    match k with
      | "action" ->
        rm_player world player;
        Player.move player (of_string v);
        add_player world player
      | _ ->
        Log.logmod "action" "[ERROR] %s: unknowm action" k
 
  let html_of_t player =
    let url = Printf.sprintf "/%s/map" player.Player.name in
    let option o = <:html<<option>$str:o$</option>&>> in
    let x = Player.x player in
    let y = Player.y player in
    let moves =
      (if x < Constants.size_x - 1 then ["up"] else []) @
      (if x > 0 then ["down"] else []) @
      (if y < Constants.size_y - 1 then ["right"] else []) @
      (if y > 0 then ["left"] else []) in
    <:html<
      <div class="action">
        <form method="post" action=$str:url$>
          <select name="action">
            $list:List.map option moves$
          </select>
          <input type="submit" value="move"/>
        </form>
      </div>
   >>
end

let html post player =
  List.iter (Action.process world player) post;
  Main.make <:html<
    <div class="welcome">Welcome $str:player.Player.name$!</div>
    $html_of_t player world$
    $Action.html_of_t player$
  >>
