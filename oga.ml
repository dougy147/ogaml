open Graphics;;
Random.self_init ();;

type cell = { row: int
            ; col: int
            ; mutable n: cell list (* neighbours *)
            ; mutable s:  int (* state *)
            ; mutable ns: int (* next state *)
            }

let life_probability = 0.049

(* window parameters *)
let width  = 1000
let height = 1000
let title  = "OGaml - Conway's Game of Life in OCaml"

(* world parameters *)
let grid = 250 (*number of cells by width or height*)
let scaled_width  = width / grid  (* avoids unwanted space *)
let scaled_height = height / grid (* avoids unwanted space *)
let cell_width  = width / scaled_width
let cell_height = width / scaled_height

let generate_a_cell (row: int) (col: int): cell =
    let state = if Random.float 1.0 <= life_probability then 1 else 0 in
    { row ; col ; n = [] ; s = state ; ns = -1 }

let make_neighbours_list_for_each_cell (world): unit =
    for i = 0 to (Array.length world - 1) do
        let cell = world.(i) in
        let c_row = cell.row in
        let c_col = cell.col in
        let prev_col = (((c_col - 1 mod cell_width) + cell_width) mod cell_width) in
        let prev_row = (((c_row - 1 mod cell_height) + cell_height) mod cell_height) in
        let same_col = (((c_col mod cell_width) + cell_width) mod cell_width) in
        let same_row = (((c_row mod cell_height) + cell_height) mod cell_height) in
        let next_col = (((c_col + 1 mod cell_width) + cell_width) mod cell_width) in
        let next_row = (((c_row + 1 mod cell_height) + cell_height) mod cell_height) in
        cell.n <- [
            world.( (prev_row * cell_width) + prev_col );
                    world.( (prev_row * cell_width) + same_col );
                    world.( (prev_row * cell_width) + next_col );
                    world.( (same_row * cell_width) + prev_col );
                    world.( (same_row * cell_width) + next_col );
                    world.( (next_row * cell_width) + prev_col );
                    world.( (next_row * cell_width) + same_col );
                    world.( (next_row * cell_width) + next_col );
    ];
    done;;

let preliminary_world () = Array.init (cell_height * cell_width)
    (fun index ->
        let row = index / cell_width in (* integer division *)
        let col = index mod cell_width in
        generate_a_cell row col)

let random_world =
    let pre_world = preliminary_world () in
    make_neighbours_list_for_each_cell pre_world;
    Array.to_list pre_world

let world = random_world

let compute_next_state (world): unit =
    let rec comp m =
        match m with
        | [] -> ()
        | cell :: rest ->
                let alive_neighbours =
                    List.fold_left ( + ) 0 (List.map (fun n -> n.s) (List.filter (fun n -> n.s = 1) cell.n))
        in
                let next_state =
                    if      alive_neighbours = 3 then 1
                    else if alive_neighbours = 2 && cell.s = 1 then 1
                    else 0
                in
                cell.ns <- next_state;
                comp rest
                in
    comp world

let refresh_state (world): unit =
    let rec refresh m =
        match m with
        | [] -> ()
        | cell :: rest ->
                cell.s <- cell.ns;
                refresh rest
    in
    refresh world

;;

(* Launch window *)
let dimensions_as_string = " " ^ (string_of_int (width-scaled_width)) ^ "x" ^ (string_of_int (height-scaled_height));;
open_graph dimensions_as_string;;
set_window_title title;;
set_color black;
fill_rect 0 0 width height;; (* black background "hack" *)

let alive_color = 1
let dead_color  = 0

let draw_point x y state size_w size_h =
  if state = 0
  then set_color black
  else set_color white;
  fill_rect ((x-1) * scaled_width) ((y-1) * scaled_height) size_w size_h;;
  (* fill_circle ((x-1) * scaled_width) ((y-1) * scaled_height) (size_w/2);; *)

let print (world: cell list): unit =
  set_color black;
  fill_rect 0 0 width height;
  let rec p = function
    | [] -> ()
    | cell :: tl ->
        draw_point cell.row cell.col alive_color scaled_width scaled_height;
        p tl
  in
  p (List.filter (fun c -> c.s = 1) world)

let update_world world =
    compute_next_state world;
    refresh_state world

let display_info (generation: int) (alive: int) =
  set_color cyan;
  moveto 1 1;
  let text = "Generation " ^ string_of_int generation ^ " (alive: " ^ string_of_int alive ^ ")" in
  draw_string text;;

let rec bigbang w =
  let rec bb generation w (paused: bool) (display: bool) =
    let event = wait_next_event [ Poll ] in
    if event.Graphics.keypressed then
      match (read_key ()) with
      | ' '    -> bb generation w (not paused) display  (* Play/pause *)
      | '\027' -> clear_graph();close_graph()           (* Escape = exit *)
      | 'n'    -> print w; bb (generation+1) (update_world w;w) true display (* Manually go to next generation *)
      | _      -> ()
    else
      if paused = true then
        bb generation w paused display
      else
        print w;
        let alive_number = List.length (List.filter (fun c -> c.s = 1) w) in
        display_info generation alive_number;
        bb (generation+1) (update_world w;w) paused display
  in
  bb 0 w false true

let () = bigbang world
