open Raylib ;;

let font_size = (Scale.f 30) ;;

type t = {
  mutable label : string;
  mutable width : int;
} ;;

let create label = {
  label = label;
  width = measure_text label font_size;
} ;;

let set l label =
  l.label <- label;
  l.width <- measure_text label font_size;
;;

let draw l x y =
  draw_text l.label x y font_size Color.raywhite;
;;

let create_element_from l =
  Layout.create_element 
    (ignore)
    (draw l)
    (fun () -> font_size)
    (fun () -> l.width)
    (ignore)
    (ignore)
;;

let create_element label =
  let l = create label in
  create_element_from l
;;
