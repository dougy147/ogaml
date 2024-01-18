open Graphics;;
Random.self_init ();;

let title  = "OGaml - Conway's Game of Life in OCaml"
let width  = 1024
let height = 1024
let grid   = 40
let scaled_width  = width  / grid ;;
let scaled_height = height / grid ;;
let normalized_width  = width  / scaled_width ;;
let normalized_height = height / scaled_height ;;
let size_to_string w h = " " ^ (string_of_int (w-scaled_width)) ^ "x" ^ (string_of_int (h-scaled_height));;

(*Init parameters*)
type coord = {x: int; y: int}
type state = Dead | Alive
type cell  = {coord: coord; state: state}
let  birth = 0.22 ;; (* Probability of a cell being alive at startup *)

let neighbours (coord: coord) : coord list =
  let nh = normalized_height and nw = normalized_width in
  let above_row = if coord.x = 1 then nh else coord.x - 1 in
  let under_row = if coord.x = nh then 1 else coord.x + 1 in
  let prev_col  = if coord.y = 1 then nw else coord.y - 1 in
  let next_col  = if coord.y = nh then 1 else coord.y + 1 in
  [ {x=above_row; y=prev_col}
   ;{x=above_row; y=coord.y  }
   ;{x=above_row; y=next_col}
   ;{x=coord.x  ; y=prev_col}
   ;{x=coord.x  ; y=next_col}
   ;{x=under_row; y=prev_col}
   ;{x=under_row; y=coord.y  }
   ;{x=under_row; y=next_col}
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
  let rec randomize_cols col col_index row: cell list =
    let state = random_state (Random.float 1.) in
    if col_index = 0 then col else randomize_cols ({coord={x=row;y=col_index};state=state}::col) (col_index-1) row
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

let world = random_world normalized_width normalized_height;;

(* Launch window *)
open_graph (size_to_string width height);;
set_window_title title;;
set_color black;
fill_rect 0 0 width height;; (* black background "hack" *)

let draw_point x y state size_w size_h =
  if state = 0 then set_color black else set_color white;
  fill_rect ((x-1) * scaled_width) ((y-1) * scaled_height) size_w size_h;;

let print (world: cell list): unit =
  let rec aux = function
    | [] -> ()
    | cell :: tl ->
        if cell.state = Alive then
          draw_point cell.coord.x cell.coord.y 1 (scaled_width) (scaled_height)
        else
          draw_point cell.coord.x cell.coord.y 0 (scaled_width) (scaled_height);
        aux tl
  in
  aux world

(* Launch life *)
let bigbang world =
  let rec aux generation world =
    if key_pressed () = true then close_graph() else
    if button_down () = true then aux generation world else (* pause if click *)
      print world;
      (*Printf.printf "\n%d generations.\n" generation;*)
      aux (generation+1) (next_world world)
  in
  aux 0 world

let () = bigbang world
