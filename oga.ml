open Graphics;;
Random.self_init ();;

type cell = { row: int       (* pos x *)
            ; col: int       (* pos y *)
            ; mutable n: cell list   (* neighbours *)
            ; mutable s: int (* state *)
            ; mutable ns: int (* next state *)
            }

let height = 120
let width  = 120

let gen_cell (row: int) (col: int): cell =
    let state = if Random.float 1.0 <= 0.10 then 1 else 0 in
    { row ; col ; n = [] ; s = state ; ns = -1 }

let map = List.init (height * width)
    (fun index ->
        let row = index / width in (* integer division *)
        let col = index mod width in
        gen_cell row col)

let append_neighbours (map): unit =
    let rec pop m =
        match m with
        | [] -> ()
        | cell :: rest ->
                let c_row = cell.row in
                let c_col = cell.col in
                cell.n <- [
                    (*List.nth map ( ( (((c_row +- 1 mod width) + width) mod width) * width) + (((c_col +- 1 mod width) + width) mod width) )*)
                    List.nth map ( ((((c_row - 1 mod height) + height) mod height) * width) + (((c_col - 1 mod width) + width) mod width) );
                    List.nth map ( ((((c_row - 1 mod height) + height) mod height) * width) + (((c_col mod width) + width) mod width) )    ;
                    List.nth map ( ((((c_row - 1 mod height) + height) mod height) * width) + (((c_col + 1 mod width) + width) mod width) );
                    List.nth map ( ((((c_row mod height) + height) mod height)     * width) + (((c_col - 1 mod width) + width) mod width) )    ;
                    List.nth map ( ((((c_row mod height) + height) mod height)     * width) + (((c_col + 1 mod width) + width) mod width) )    ;
                    List.nth map ( ((((c_row + 1 mod height) + height) mod height) * width) + (((c_col - 1 mod width) + width) mod width) );
                    List.nth map ( ((((c_row + 1 mod height) + height) mod height) * width) + (((c_col mod width) + width) mod width) )    ;
                    List.nth map ( ((((c_row + 1 mod height) + height) mod height) * width) + (((c_col + 1 mod width) + width) mod width) );
                ];
                pop rest
    in
    pop map

let init_world = append_neighbours map

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



(* Launch window *)

let title  = "OGaml - Conway's Game of Life in OCaml"
let cell_size = 6 ;; (* puixels *)
let scaled_width  = cell_size ;;
let scaled_height = cell_size ;;
let cell_width  = cell_size ;;
let cell_height = cell_size ;;
let size_to_string w h = " " ^ (string_of_int (w-scaled_width)) ^ "x" ^ (string_of_int (h-scaled_height));;
open_graph (size_to_string (width * cell_size) (height * cell_size));;
set_window_title title;;
set_color black;
fill_rect 0 0 (width * cell_size) (height * cell_size);; (* black background "hack" *)

let alive_color = 1
let dead_color  = 0

let draw_point x y state size_w size_h =
  if state = 0 then set_color black else set_color white;
  fill_rect ((x-1) * scaled_width) ((y-1) * scaled_height) size_w size_h;;
  (* fill_circle ((x-1) * scaled_width) ((y-1) * scaled_height) (size_w/2);; *)

let print (world: cell list): unit =
  set_color black;
  fill_rect 0 0 (width * cell_size) (height * cell_size);
  let rec aux = function
    | [] -> ()
    | cell :: tl ->
        draw_point cell.row cell.col alive_color (scaled_width) (scaled_height);
        aux tl
  in
  aux (List.filter (fun c -> c.s = 1) world)

let rec bigbang w =
    print w;
    compute_next_state map;
    refresh_state map;
    bigbang w

let () = bigbang map
