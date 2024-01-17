open Graphics;;
Random.self_init ();;

let title = "OGaml - Conway's Game of Life in OCaml"
let width  = 1024
let height = 768
let scale_width  = width / 34 ;;
let scale_height = height / 34 ;;
let size_to_string w h = " " ^ (string_of_int w) ^ "x" ^ (string_of_int h);;

(*Init parameters*)
type coord = {x: int; y: int}
type state = Dead | Alive
type cell  = {coord: coord; state: state}
let  birth = 0.28 ;; (* Probability of a cell being alive at startup *)

let neighbours (coord: coord) : coord list =
  [ {x=coord.x-1; y=coord.y-1}
   ;{x=coord.x-1; y=coord.y  }
   ;{x=coord.x-1; y=coord.y+1}
   ;{x=coord.x  ; y=coord.y-1}
   ;{x=coord.x  ; y=coord.y+1}
   ;{x=coord.x+1; y=coord.y-1}
   ;{x=coord.x+1; y=coord.y  }
   ;{x=coord.x+1; y=coord.y+1}
  ]

let count_neighbours (coord: coord) (world: cell list) = (* Returns the number of Alive neighbours *)
  let neighbours_list = neighbours coord in
  let cur_neighbours   = List.filter (fun cell -> List.mem cell.coord neighbours_list) world in
  let alive_neighbours = List.filter (fun cell -> cell.state = Alive) cur_neighbours in
  List.length alive_neighbours

let next_cell (state: state) (neighbours_num: int) =
  match state with (* Conway's rules *)
  | Alive when neighbours_num = 2 -> Alive
  | _ when neighbours_num = 3 -> Alive
  | _ -> Dead

let random_world w h: cell list=
  let random_state n =
    if n < birth then Alive else Dead
  in
  let rec randomize_cols col size row: cell list =
    let state = random_state (Random.float 1.) in
    if size = 0 then col else randomize_cols ({coord={x=row;y=size};state=state}::col) (size-1) row
  in
  let rec randomize_rows acc_world w h: cell list =
    if h = 0 then acc_world else randomize_rows ((randomize_cols [] w h) @ acc_world) w (h-1)
  in
  randomize_rows [] w h

let next_world (cur_world: cell list): cell list =
  let rec aux next = function
    | [] -> next
    | cell :: rest ->
        let next_state = next_cell cell.state (count_neighbours cell.coord cur_world) in
        aux ({coord=cell.coord;state=next_state}::next) rest
  in
  List.rev (aux [] cur_world)

let world = random_world (width/scale_width) (height/scale_height);;

(* Launch window *)
open_graph (size_to_string width height);;
set_window_title title;;
set_color black;
fill_rect 0 0 width height;; (* black background "hack" *)

let draw_point x y state size_w size_h =
  if state = 0 then set_color black else set_color white;
  fill_rect (x + x * scale_width) (y + y * scale_height) size_w size_h;;

let print (world: cell list): unit =
  let rec aux = function
    | [] -> ()
    | cell :: tl ->
        if cell.state = Alive then
          draw_point cell.coord.x cell.coord.y 1 (scale_width) (scale_height)
        else
          draw_point cell.coord.x cell.coord.y 0 (scale_width) (scale_height);
        aux tl
  in
  aux world

(* Launch life *)
let bigbang world =
  let rec aux generation world =
    print world;
    Printf.printf "\n%d\n" generation;
    aux (generation+1) (next_world world)
  in
  aux 0 world

let () = bigbang world
