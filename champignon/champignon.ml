open Gui ;;
open Lib ;;

let player_templates = [
    ("Assassin", "60|40|9|6|8|5|3");
    ("Tank", "35|65|5|10|14|2|5");
    ("Equilibre", "50|50|10|5|10|5|5")
] ;;

let init_window () =
  Raylib.set_config_flags [Window_resizable];
  Raylib.init_window 1200 900 "Designed by Champignons & Juroupi";
  Raylib.set_target_fps 60;
  Raylib.init_audio_device ();
  (*Raylib.maximize_window ()*)
;;

(* Créer un personnage à partir des champs personnalisés *)
let player_of_stats name id stats =
  try 
    let stats = Array.map (fun (stat : TextInput.t) -> int_of_string stat.content) stats in
    let player = Player.create_player name id stats.(6) stats.(5) stats.(4) stats.(3)
        (Player.create_attack stats.(2) stats.(1) stats.(0)) in
    if Player.player_is_valid player then
      Some player
    else begin
      Message.with_button "Invalide";
      None
    end
  with _ -> 
    Message.with_button "Invalide";
    None
;;

(* Choix d'un personnage *)
let player_choice n =

  let choice = ref None in
  let layout = Layout.create () in

  Layout.add layout (Button.create_element "Annuler" (fun () -> choice := Some Player.empty_player));

  Layout.add layout (Space.create_element 40);

  let custom = Array.map (TextInput.create) [| "PA Attaque"; "Portée"; "Dégats"; "PM"; "PA"; "Vie"; "Force" |] in
  Layout.add layout (Button.create_element "Valider" (fun () -> choice := player_of_stats "" (n-1) custom));
  Array.iter (fun ti -> Layout.add layout (TextInput.create_element_from ti)) custom;
  Layout.add layout (Label.create_element "Personnalisé :");

  Layout.add layout (Space.create_element 20);

  List.iter (fun (name, stats) ->
    Layout.add layout (Button.create_element (name ^ " : " ^ stats) (fun () -> choice := Some (Player.player_of_string "" stats (n-1))))
  ) player_templates;
  Layout.add layout (Label.create_element (Printf.sprintf "Choix personnage %d :" n));

  Layout.loop_cond layout (fun () -> !choice = None);
  
  Option.get !choice
;;

(* Choix des personnages *)
let players_choice () =
  let rec players_choice_rec n players =
    if n > 3 then
      players
    else begin
      let choice = player_choice n in
      if choice = Player.empty_player then
        if n = 1 then
          []
        else
          players_choice_rec (n-1) (List.tl players)
      else
        players_choice_rec (n+1) (choice :: players)
    end
  in players_choice_rec 1 []
;;

(* Recevoir le numéro du personnage à jouer *)
let recv_num client =
  int_of_string (Network.recv_string client)
;;

(* Recevoir la carte *)
let recv_map client =
  Map.map_of_string (Network.recv_string client)
;;

(* Recevoir la liste des personnages *)
let recv_players client =
  Player.player_array_of_string_serv (Network.recv_string client)
;;

(* Envoyer la liste d'actions *)
let send_actions client actions =
  Network.send_string client (Action.string_of_action_list actions)
;;

(* On attend que le joueur choisisse une action *)
let wait_for_action map players num =

  let layout = Layout.create () in

  let infos = Label.create "" in
  let stats = Label.create "" in

  let action = ref None in

  Layout.add layout (Label.create_element_from stats);
  Layout.add layout (Label.create_element_from infos);
  Layout.add layout (Grid.create_element map num players infos stats action);
  Layout.add layout (Label.create_element "Choisir une action :");

  Layout.loop_cond layout (fun () -> !action = None);

  Option.get !action
;;

(* On attend que le joueur choisisse toutes ses actions *)
let rec wait_for_actions map players num =

  let action = wait_for_action map players num in
    
  if action = InvalidAction then
    []
  else begin
    Game.play_action action map players num;
    action :: (wait_for_actions map players num)
  end
;;

(* Tour de jeu *)
let rec game_loop client =

  let num = recv_num client in

  if num < 6 then begin

    let map = recv_map client in
    let players = recv_players client in

    let actions = wait_for_actions map players num in

    send_actions client actions;

    Message.simple "En attente...";

    game_loop client;
  end else if num = 6 then
    Message.with_button "Le joueur pair a gagné"
  else
    Message.with_button "Le joueur impair a gagné"
;;

(* On commence la partie *)
let game client username =

  let players = players_choice () in
  Network.send_string client username;
  List.iter (fun p -> Network.send_string client (Player.string_of_player_serv p)) players;

  Message.simple "En attente...";

  game_loop client
;;

(* On se connecte au serveur *)
let connect ip username =
  let layout = Layout.create () in
  Layout.add layout (Label.create_element "Connexion...");
  Layout.draw_once layout;
  try
    let client = Network.create_client ip 6566 in
    game client username
  with _ -> Message.with_button "Erreur de connexion"
;;

(* On demande d'ip du serveur et le pseudo *)
let menu () =
  let quit = ref false in
  let layout = Layout.create () in
  let ip = TextInput.create "IP du serveur" in
  let username = TextInput.create "Pseudo" in
  ip.content <- "127.0.0.1";
  Layout.add layout (Button.create_element "Quitter" (fun () -> quit := true));
  Layout.add layout (Space.create_element 20);
  Layout.add layout (Button.create_element "Valider" (fun () -> connect ip.content username.content));
  Layout.add layout (TextInput.create_element_from username);
  Layout.add layout (TextInput.create_element_from ip);
  Layout.loop_cond layout (fun () -> not !quit)
;;

let init_sounds () = 
  let sounds = Array.map (fun s -> Raylib.load_sound (Printf.sprintf "sounds/%s" s)) (Sys.readdir "sounds") in
  Layout.default_click_action := (fun () ->
    Raylib.play_sound sounds.(Random.int (Array.length sounds))
  )
;;


let test () =
  let map = Map.create_map 10 10 0.1 in
  let players = [| 
    Player.empty_player; Player.empty_player; Player.empty_player; 
    Player.empty_player; Player.empty_player; Player.empty_player 
  |] in
  let layout = Layout.create () in
  let infos = Label.create "" in
  let stats = Label.create "" in
  Map.map_set map 2 1 '0';
  Map.map_set map 2 2 '3';
  Layout.add layout (Label.create_element_from stats);
  Layout.add layout (Label.create_element_from infos);
  Layout.add layout (Grid.create_element map 0 players infos stats (ref None));
  Layout.add layout (Label.create_element "Choisir une action :");
  Layout.loop layout
;;

init_window () ;;
init_sounds () ;;
menu () ;;

























