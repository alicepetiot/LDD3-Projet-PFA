let simple content =
  let layout = Layout.create () in
  Layout.add layout (Label.create_element content);
  Layout.draw_once layout
;;

let with_button content =
  let ok = ref false in
  let layout = Layout.create () in
  Layout.add layout (Button.create_element "OK" (fun () -> ok := true));
  Layout.add layout (Label.create_element content);
  Layout.loop_cond layout (fun () -> not !ok)
;;