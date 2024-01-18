open Graphics;;
Random.self_init ();;

let title  = "OGaml - Conway's Game of Life in OCaml"
let width  = 800
let height = 600
let grid   = 50
let scaled_width  = width  / grid ;;
let scaled_height = height / grid ;;
let normalized_width  = width  / scaled_width ;;
let normalized_height = height / scaled_height ;;
let size_to_string w h = " " ^ (string_of_int (w-scaled_width)) ^ "x" ^ (string_of_int (h-scaled_height));;

(*Init parameters*)
type coord = {x: int; y: int}
type state = Dead | Alive
type cell  = {coord: coord; state: state; neighbours: int; neighbours_cpy: int}
let init_life_prob = 0.06 (* Probability of a cell being alive at startup *)

(* Functional way of changing records (see "Real World Ocaml" p.94) *)
let change_cell_state (c: cell) (s: state): cell = {c with state = s}
let change_cell_neigh (c: cell) (n: int)  : cell = {c with neighbours = n}
let change_cell_neigh_cpy (c: cell) (n: int) : cell = {c with neighbours_cpy = n}

let neighbours (pos: coord) : coord list =
  let nh = normalized_height and nw = normalized_width in
  let above_row = if pos.x = 1  then nh else pos.x - 1
  and under_row = if pos.x = nh then 1  else pos.x + 1
  and prev_col  = if pos.y = 1  then nw else pos.y - 1
  and next_col  = if pos.y = nw then 1  else pos.y + 1 in
  [ {x=above_row; y=prev_col}
   ;{x=above_row; y=pos.y  }
   ;{x=above_row; y=next_col}
   ;{x=pos.x    ; y=prev_col}
   ;{x=pos.x    ; y=next_col}
   ;{x=under_row; y=prev_col}
   ;{x=under_row; y=pos.y  }
   ;{x=under_row; y=next_col}
  ]

let random_world (width: int) (height: int): cell list=
  let count_neighbours (pos: coord) (world: cell list) =
    let neighbours_list  = neighbours pos in
    let cur_neighbours   = List.filter (fun c -> List.mem c.coord neighbours_list) world in
    let alive_neighbours = List.filter (fun c -> c.state = Alive) cur_neighbours in
    List.length alive_neighbours
  in
  let random_init_state (alea: float) =
    if alea < init_life_prob then Alive else Dead
  in
  let rec randomize_cols acc_cols (col_index: int) (row: int): cell list =
    let state = random_init_state (Random.float 1.) in
    if col_index = 0 then acc_cols
    else randomize_cols ({coord={x=row;y=col_index};state=state;neighbours=0;neighbours_cpy=0}::acc_cols) (col_index-1) row
  in
  let rec randomize_rows acc_rows width height: cell list =
    if height = 0 then acc_rows
    else randomize_rows ((randomize_cols [] width height) @ acc_rows) width (height-1)
  in
  let rec add_neighbours_count next cur_world = function
    | [] -> next
    | cell :: rest ->
        let c = change_cell_neigh cell (count_neighbours cell.coord cur_world) in
        let c = change_cell_neigh_cpy c c.neighbours in
        add_neighbours_count (c::next) cur_world rest
  in
  let r_world = randomize_rows [] width height in
  add_neighbours_count [] r_world r_world

let update_neighbours_count (cells_list: cell list) (cell: cell): cell list =
  (* I should link every cell to each of its neighbours at startup, that would avoid going through the whole world to change their values *)
  let neighbours_list = neighbours cell.coord in
  match cell.state with
    | Dead  -> cells_list
               |> List.map (fun x -> if List.mem x.coord neighbours_list then change_cell_neigh x (x.neighbours - 1) else x)
    | Alive -> cells_list
               |> List.map (fun x -> if List.mem x.coord neighbours_list then change_cell_neigh x (x.neighbours + 1) else x)

let next_world (cur_world: cell list): cell list =
  let rec aux next = function
    | [] -> next
    | cell :: rest ->
        let new_state =
          match cell.state,cell.neighbours_cpy with
          | Alive,2 -> Alive
          | _,3     -> Alive
          | _,_     -> Dead
        in
        if cell.state = new_state then
          (* If the cell state does not change, then just update the "neighbours_cpy" field *)
          let new_cell = change_cell_neigh_cpy cell cell.neighbours in
          aux (new_cell::next) rest
        else
          let new_cell = change_cell_state cell new_state in
          let new_cell = change_cell_neigh_cpy new_cell new_cell.neighbours in
          (* Here is where CPU consumption increase : *)
          let new_next = update_neighbours_count next new_cell in
          let new_rest = update_neighbours_count rest new_cell in
          aux (new_cell::new_next) new_rest
  in
  List.rev (aux [] cur_world);;

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
let bigbang w =
  let rec aux generation w =
    if key_pressed () = true then close_graph() else
    if button_down () = true then aux generation w else (* pause if click *)
      print w;
      (*Printf.printf "\n%d generations.\n" generation;*)
      aux (generation+1) (next_world w)
  in
  aux 0 w

let world = random_world normalized_width normalized_height

let () = bigbang world
