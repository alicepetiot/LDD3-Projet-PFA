open Raylib ;;

let font_size = Scale.f 30 ;;
let border_width = Scale.f (font_size * 10) ;;

type t = {
  label : string;
  width : int;
  on_click : unit -> unit;
} ;;

let create label on_click = {
  label = label;
  width = measure_text label font_size;
  on_click = on_click;
} ;;

let draw b x y =
  draw_text b.label (x + (Scale.f 10)) (y + (Scale.f 5)) font_size Color.raywhite;
  draw_rectangle_lines x y (b.width + (Scale.f 20)) (font_size + (Scale.f 10)) Color.raywhite;
;;

let button_cursor on =
  if on then
    set_mouse_cursor MouseCursor.Pointing_hand
  else
    set_mouse_cursor MouseCursor.Default
;;

let create_element label on_click =
  let b = create label on_click in
  Layout.create_element 
    (ignore)
    (draw b)
    (fun () -> font_size + (Scale.f 10))
    (fun () -> b.width + (Scale.f 20))
    (fun a -> if a then b.on_click ())
    (button_cursor)
;;
