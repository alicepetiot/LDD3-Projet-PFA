let create_element height =
  Layout.create_element
    (ignore)
    (fun x y -> ignore (x, y))
    (fun () -> (Scale.f height))
    (fun () -> 0)
    (ignore)
    (ignore)
;;
