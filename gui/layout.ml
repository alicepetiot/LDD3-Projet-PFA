type align = Left | Right | Center ;;

type element = {
  mutable align : align;
  mutable mouse_on : bool;
  mutable computed_x : int;
  mutable computed_y : int;
  mutable computed_width : int;
  mutable computed_height : int;
  update : unit -> unit;
  draw : int -> int -> unit;
  get_height : unit -> int;
  get_width : unit -> int;
  set_active : bool -> unit;
  set_hover : bool -> unit;
} ;;

let create_element update draw get_height get_width set_active set_hover = {
  align = Center;
  mouse_on = false;
  computed_x = 0;
  computed_y = 0;
  computed_width = get_width ();
  computed_height = get_height ();
  update = update;
  draw = draw;
  get_height = get_height;
  get_width = get_width;
  set_active = set_active;
  set_hover = set_hover
} ;;

let elements_space = (Scale.f 10) ;;

let default_click_action = ref (fun () -> ()) ;;

type t = {
  mutable elements : element list;
  mutable height : int;
  mutable width : int;
  mutable x : int;
  mutable y : int;
  mutable active_element : element option;
  mutable mouse_down : bool;
} ;;

let create () = {
  elements = [];
  height = -elements_space;
  width = 0;
  x = 0;
  y = 0;
  active_element = None;
  mouse_down = Raylib.is_mouse_button_down Raylib.MouseButton.Left;
} ;;

let add l el =
  l.elements <- el :: l.elements;
  l.height <- l.height + el.get_height () + elements_space;
  l.width <- max l.width (el.get_width ());
;;

let compute_element_x l el =
  match el.align with
  | Left -> 0
  | Right -> l.width - el.computed_height
  | Center -> l.width / 2 - (el.computed_width) / 2
;;

let mouse_on_element el =
  let mx = Raylib.get_mouse_x () in
  let my = Raylib.get_mouse_y () in
  mx >= el.computed_x && mx <= el.computed_x + el.computed_width && my >= el.computed_y && my <= el.computed_y + el.computed_height
;;

let mouse_pressed l =
  if not l.mouse_down && Raylib.is_mouse_button_down Raylib.MouseButton.Left then begin
    (!default_click_action) ();
    l.mouse_down <- true;
    true
  end else begin
    if l.mouse_down && not (Raylib.is_mouse_button_down Raylib.MouseButton.Left) then
      l.mouse_down <- false;
    false
  end
;;

let update l =
  let mouse_pressed = mouse_pressed l in
  if mouse_pressed && l.active_element <> None then begin
    (Option.get l.active_element).set_active false;
    l.active_element <- None
  end;
  let rec update_elements elements x y =
    match elements with
    | [] -> ()
    | el :: elements ->
      el.update ();
      el.computed_width <- el.get_width ();
      el.computed_height <- el.get_height ();
      el.computed_x <- x + compute_element_x l el;
      el.computed_y <- y;
      if mouse_on_element el then begin
        if not el.mouse_on then begin
          el.mouse_on <- true;
          el.set_hover true;
        end;
        if mouse_pressed && (Some el) <> l.active_element then begin
          l.active_element <- Some el;
          el.set_active true;
        end;
      end else if el.mouse_on then begin
        el.mouse_on <- false;
        el.set_hover false;
      end;
      update_elements elements x (el.computed_y + el.computed_height + elements_space)
  in update_elements l.elements l.x l.y
;;

let draw_content l =
  List.iter (fun el -> 
    el.draw el.computed_x el.computed_y
  ) l.elements
;;

let set_center l x y =
  l.x <- x - l.width / 2;
  l.y <- y - l.height / 2;
;;

let draw_once l =
  let open Raylib in
  set_center l ((get_screen_width ()) / 2) ((get_screen_height ()) / 2);
  update l;
  begin_drawing ();
  clear_background Color.black;
  draw_content l;
  end_drawing ();
;;

let rec loop_cond l cond =
  let open Raylib in
  if window_should_close () then
    close_window ()
  else if cond () then begin
    draw_once l;
    loop_cond l cond;
  end else
    set_mouse_cursor MouseCursor.Default
;;

let loop l =
  loop_cond l (fun () -> true)
;;
