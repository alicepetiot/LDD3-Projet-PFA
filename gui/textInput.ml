open Raylib ;;

let font_size = (Scale.f 30) ;;
let border_width = (Scale.f (font_size * 10)) ;;

type t = {
  label : string;
  label_width : int;
  mutable content : string;
  mutable content_width : int;
  mutable cur : int;
  mutable active : bool
} ;;

let create label = 
  let label = label ^ " : " in 
  {
    label = label;
    label_width = measure_text label font_size;
    content = "";
    content_width = 0;
    cur = 0;
    active = false
  }
;;

let move_cur ti d =
  let cur = ti.cur + d in
  if cur >= 0 && cur <= String.length ti.content then
    ti.cur <- cur
;;

let remove_all ti =
  ti.content <- "";
  ti.cur <- 0
;;

let remove_prev ti =
  if ti.cur > 0 then begin
    let len = String.length ti.content in
    ti.content <- (String.sub ti.content 0 (ti.cur-1)) ^ (String.sub ti.content ti.cur (len - ti.cur));
    ti.cur <- ti.cur - 1
  end
;;

let remove_next ti =
  let len = String.length ti.content in
  if ti.cur < len then begin
    let len = String.length ti.content in
    ti.content <- (String.sub ti.content 0 ti.cur) ^ (String.sub ti.content (ti.cur+1) (len - ti.cur - 1))
  end
;;

let append ti chr =
  let code = Char.code chr in
  if code >= 32 && code <= 126 then begin
    ti.content <- ti.content ^ (String.make 1 chr);
    ti.cur <- ti.cur + 1
  end
;;

let update ti =
  if ti.active then begin
    begin match get_key_pressed () with
    | Key.Backspace when (is_key_down Key.Left_control) || (is_key_down Key.Right_control) -> remove_all ti
    | Key.Backspace -> remove_prev ti
    | Key.Delete -> remove_next ti
    | Key.Left -> move_cur ti (-1)
    | Key.Right -> move_cur ti 1
    | _ -> append ti (get_char_pressed ()) 
    end;
    ti.content_width <- measure_text ti.content font_size
  end
;;

let draw ti x y =
  draw_text ti.label x y font_size Color.raywhite;
  let x = x + ti.label_width + (Scale.f 5) in
  let border_width = (max border_width ti.content_width) + (Scale.f 10) in
  draw_text ti.content x y font_size Color.raywhite;
  draw_rectangle_lines (x - (Scale.f 5)) y border_width font_size Color.raywhite;
  if ti.active then
    let line_x = x + (measure_text (String.sub ti.content 0 ti.cur) font_size) + 1 in
    draw_line line_x (y + (Scale.f 3)) line_x (y + font_size - (Scale.f 3)) Color.raywhite
;;

let create_element_from ti =
  Layout.create_element 
    (fun () -> update ti)
    (draw ti)
    (fun () -> font_size)
    (fun () -> ti.label_width + border_width)
    (fun a -> ti.active <- a)
    (fun on -> ignore on)
;;

let create_element label =
  let ti = create label in
  create_element_from ti
;;
