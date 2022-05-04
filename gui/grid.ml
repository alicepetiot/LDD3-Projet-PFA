open Raylib ;;
open Lib.Map ;;
open Lib.Game ;;
open Lib.Player ;;
open Lib.Action ;;

let font_size = (Scale.f 24) ;;
let square_size = (Scale.f 38) ;;

type t = {
  map : map;
  cur_player : player;
  players : player array;
  accessible_enemies : int list;
  accessible_positions : (pos, int) Hashtbl.t;
  mutable mouse_pos : int * int;
  mutable el : Layout.element option;
  infos : Label.t;
  stats : Label.t;
  action : action option ref;
  mutable tmp_action : action option;
} ;;

let create map num players infos stats action = {
  map = map;
  cur_player = players.(num);
  players = players;
  accessible_enemies = accessible_enemies map players.(num) players;
  accessible_positions = accessible_positions map players.(num);
  mouse_pos = (-1, -1);
  el = None;
  infos = infos;
  stats = stats;
  action = action;
  tmp_action = None;
} ;;

let square_pos x =
  int_of_float (Float.floor ((float_of_int x) /. (float_of_int square_size)))
;;

let accessible_position g x y =
  Hashtbl.mem g.accessible_positions (create_pos x y)
;;

let accessible_enemy g id =
  (List.mem id g.accessible_enemies) && (player_can_attack g.cur_player)
;;

let update g =

  let el = Option.get g.el in
  let mx = square_pos (get_mouse_x () - el.computed_x) in
  let my = square_pos (get_mouse_y () - el.computed_y) in
  let mouse_pos = (mx, my) in

  if mouse_pos <> g.mouse_pos then begin

    if on_map g.map mx my then begin
      
      g.tmp_action <- None;

      let id = id_of_char (map_get g.map mx my) in

      if accessible_position g mx my then begin
        Label.set g.infos (Printf.sprintf "Se déplacer en (%d, %d)" mx my);
        g.tmp_action <- Some (MoveAction (mx, my, move_distance g.accessible_positions mx my));
        set_mouse_cursor MouseCursor.Pointing_hand;
      end else if accessible_enemy g id then begin
        Label.set g.infos (Printf.sprintf "Attaquer le joueur n°%d en (%d, %d)" id mx my);
        g.tmp_action <- Some (AttackAction (mx, my));
        set_mouse_cursor MouseCursor.Pointing_hand;
      end else if g.cur_player.id = id then begin
        Label.set g.infos "Finir le tour";
        g.tmp_action <- Some InvalidAction;
        set_mouse_cursor MouseCursor.Pointing_hand;
      end else begin
        Label.set g.infos (Printf.sprintf "(%d, %d)" mx my);
        set_mouse_cursor MouseCursor.Default;
      end;

      if id >= 0 && id <= 5 then begin
        Label.set g.stats (player_data g.players.(id));
      end else
        Label.set g.stats "";

    end else begin
      Label.set g.infos "";
      set_mouse_cursor MouseCursor.Default;
    end;

    g.mouse_pos <- mouse_pos;
  end
;;

let player_color g id =
  if g.cur_player.id = id then
    Color.green
  else if id mod 2 = 0 then
    Color.blue
  else
    Color.red
;;

let draw_player g p rx ry =
  let text = String.make 1 p in
  let id = id_of_char p in
  let cx = (float_of_int rx) +. (float_of_int square_size) /. 2.0 in
  let cy = (float_of_int ry) +. (float_of_int square_size) /. 2.0 in
  let x = cx -. (float_of_int (measure_text text font_size)) /. 2.0 in
  let y = cy -. (float_of_int font_size) /. 2.0 in
  if accessible_enemy g id then
    draw_rectangle (rx + 2) (ry + 2) (square_size - 4) (square_size - 4) Color.darkgray;
  draw_circle (int_of_float cx) (int_of_float cy) ((float_of_int square_size) /. 2.0 -. 2.0) (player_color g id);
  draw_text text (int_of_float x) (int_of_float y) font_size Color.black;
;;

let draw g x y =
  for i=0 to g.map.width-1 do 
    for j=0 to g.map.height-1 do
      let rx = x + i * square_size in
      let ry = y + j * square_size in
      if g.mouse_pos = (i, j) then
        draw_rectangle rx ry square_size square_size Color.lightgray;
      begin match map_get g.map i j with
      | '#' ->
        draw_rectangle rx ry square_size square_size Color.gray;
        draw_rectangle_lines rx ry square_size square_size Color.lightgray;
      | '0'..'5' as p ->
        draw_player g p rx ry;
      | ' ' when accessible_position g i j ->
        draw_rectangle (rx + 2) (ry + 2) (square_size - 4) (square_size - 4) Color.darkgray;
      | _ -> ()
      end;
    done;
  done; 
;;

let create_element map num players infos stats action =
  let g = create map num players infos stats action in
  let el = Layout.create_element
    (fun () -> update g)
    (draw g)
    (fun () -> square_size * map.height)
    (fun () -> square_size * map.width)
    (fun a -> if a then g.action := g.tmp_action)
    (ignore)
  in
  g.el <- Some el;
  el
;;
