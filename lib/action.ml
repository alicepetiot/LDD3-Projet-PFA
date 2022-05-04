type action =
  | AttackAction of (int * int)
  | MoveAction of (int * int * int)
  | InvalidAction
;;

let create_action t y x =
  match t with
  | 1 -> AttackAction (x, y)
  | 2 -> MoveAction (x, y, 0)
  | _ -> InvalidAction
;;

let action_of_string s =
  Scanf.sscanf s "%d|%d|%d" create_action
;;

let action_list_of_string s =
  List.map action_of_string (String.split_on_char ';' s)
;;

let string_of_action a =
  match a with
  | AttackAction (x, y) -> Printf.sprintf "1|%d|%d" y x
  | MoveAction (x, y, _) -> Printf.sprintf "2|%d|%d" y x
  | _ -> "invalid"
;;

let string_of_action_list al =
  let rec _string_of_action_list al res =
    match al with
    | [] -> res
    | a :: [] -> res ^ (string_of_action a)
    | a :: al -> _string_of_action_list al (res ^ (string_of_action a) ^ ";")
  in _string_of_action_list al ""
;;