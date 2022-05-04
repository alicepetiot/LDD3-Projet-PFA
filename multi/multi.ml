open Lib.Network ;;
open Lib.Player ;;
open Lib.Map ;;

Printf.printf "IP du serveur, 127.0.0.1 par défaut : " ;;
let ip = read_line () ;;
let c = create_client (if ip = "" then "127.0.0.1" else ip) 6566 ;;

let recv_num c =
  int_of_string (recv_string c)
;;

let recv_map c =
  map_of_string (recv_string c)
;;

let recv_players c =
  player_array_of_string_serv (recv_string c)
;;

let print_players players =
  Array.iter print_player_color players
;;

let ask s c = 
    Printf.printf "\n %s: " s;
    let r = read_line () in 
    send_string c r
;;

ask "Pseudo" c;;
ask "Perso1" c;;
ask "Perso2" c;;
ask "Perso3" c;;

let rec tour c = 
  let num = recv_num c in
  if num <> 6 && num <> 7 then
    begin
        let map = recv_map c in
        let personnage = recv_players c in 
        Printf.printf "num: %d\n" num;
        print_players personnage;
        print_map map;
        ask "Envoyer une liste d'actions" c;
        tour c;
    end
  else
    close_out c.output
;;

tour c;;
