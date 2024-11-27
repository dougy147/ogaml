open Graphics;;
Random.self_init ();;

type cell = { row: int
            ; col: int
            ; mutable n: cell list (* neighbours *)
            ; mutable s:  int (* state *)
            ; mutable ns: int (* next state *)
            }

let life_probability = 0.049

let width  = 1000
let height = 1000

let title  = "OGaml - Conway's Game of Life in OCaml"
let grid = 250
let scaled_width  = width / grid
let scaled_height = height / grid
let cell_width  = width / scaled_width
let cell_height = width / scaled_height
let dimensions_as_string w h = " " ^ (string_of_int (w-scaled_width)) ^ "x" ^ (string_of_int (h-scaled_height))

let gen_cell (row: int) (col: int): cell =
    let state = if Random.float 1.0 <= life_probability then 1 else 0 in
    { row ; col ; n = [] ; s = state ; ns = -1 }

let init_map = Array.init (cell_height * cell_width)
    (fun index ->
        let row = index / cell_width in (* integer division *)
        let col = index mod cell_width in
        gen_cell row col)

let append_neighbours (map): unit =
    for i = 0 to (Array.length map - 1) do
        let cell = map.(i) in
        let c_row = cell.row in
        let c_col = cell.col in
        let prev_col = (((c_col - 1 mod cell_width) + cell_width) mod cell_width) in
        let prev_row = (((c_row - 1 mod cell_height) + cell_height) mod cell_height) in
        let same_col = (((c_col mod cell_width) + cell_width) mod cell_width) in
        let same_row = (((c_row mod cell_height) + cell_height) mod cell_height) in
        let next_col = (((c_col + 1 mod cell_width) + cell_width) mod cell_width) in
        let next_row = (((c_row + 1 mod cell_height) + cell_height) mod cell_height) in
        cell.n <- [
            map.( (prev_row * cell_width) + prev_col );
                    map.( (prev_row * cell_width) + same_col );
                    map.( (prev_row * cell_width) + next_col );
                    map.( (same_row * cell_width) + prev_col );
                    map.( (same_row * cell_width) + next_col );
                    map.( (next_row * cell_width) + prev_col );
                    map.( (next_row * cell_width) + same_col );
                    map.( (next_row * cell_width) + next_col );
    ];
    done;;

let init_world = append_neighbours init_map
let map = Array.to_list init_map

let compute_next_state (map): unit =
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
    comp map

let refresh_state (map): unit =
    let rec refresh m =
        match m with
        | [] -> ()
        | cell :: rest ->
                cell.s <- cell.ns;
                refresh rest
    in
    refresh map

;;

(* Launch window *)
open_graph (dimensions_as_string width height);;
set_window_title title;;
set_color black;
fill_rect 0 0 width height;; (* black background "hack" *)

let alive_color = 1
let dead_color  = 0

let draw_point x y state size_w size_h =
  if state = 0 then set_color black else set_color white;
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

let update_world map =
    compute_next_state map;
    refresh_state map

let rec bigbang world =
    print world;
    update_world world;
    bigbang world

let () = bigbang map
