open Lib.Player
open Lib.Game
open Lib.Map

let players = [|
    empty_player;
    empty_player;
    empty_player;
    empty_player;
    empty_player;
    empty_player 
|] ;;


let player_templates = [|
    ("Assassin", "60|40|9|6|8|5|3");
    ("Tank", "35|65|5|10|14|2|5");
    ("Equilibre", "50|50|10|5|10|5|5")
|] ;;

Printf.printf "Liste des personnages :\n" ;;
for i=0 to (Array.length player_templates) - 1 do
    Printf.printf " %d. %-11s %s\n" (i+1) ((fst player_templates.(i)) ^ " :") (player_data_color (player_of_string "" (snd player_templates.(i)) 0))
done;;

(* Lire un entier dans la console, recommence tant que l'entrée est invalide *)
let read_int () =
    let rec _read_int () =
        try
            read_int ()
        with _ -> (Printf.printf "Invalide\n" ; _read_int ())
    in _read_int ()
;;

(* Lire un entier borné dans la console, recommence tant que l'entrée est invalide *)
let rec read_int_min_max min max =
    let v = read_int () in
    if v >= min && v <= max then 
        v 
    else begin
        Printf.printf "Invalide\n" ;
        read_int_min_max min max
    end
;;


for i=0 to 1 do 
    Printf.printf "Equipe %d\n" (i+1) ;
    for j=0 to 2 do
        Printf.printf "Choisir le personnage %d\n" (j+1) ;
        let choice = (read_int_min_max 1 (Array.length player_templates)) - 1 in
        players.(i * 3 + j) <- player_of_string 
            (fst player_templates.(choice)) 
            (snd player_templates.(choice)) 
            (i * 3 + j)
    done;
done;;


let map = create_map 30 30 0.06 ;;
Array.iter (add_player map) players ;;


let rec action_attack m player players_around =
    Printf.printf "\x1b[1;95mJoueur à attaquer\x1b[0m: ";
    let number = read_int_min_max 0 5 in
    if List.mem number players_around then 
        attack_player m player players.(number)
    else begin
        if (player.id < 3) = (number < 3) then
            Printf.printf "Le joueur sélectionné n'est pas un ennemi\n"
        else
            Printf.printf "Le joueur sélectionné n'est pas dans la portée\n";
        action_attack m player players_around
    end
;;

let rec action_move m player accessible_positions =
    Printf.printf "\x1b[1;95mX\x1b[0m: ";
    let x = player.pos.x + read_int () in
    Printf.printf "\x1b[1;95mY\x1b[0m: "; 
    let y = player.pos.y + read_int () in
    if not (move_player m player x y accessible_positions) then begin
        Printf.printf "Coordonnées invalides\n"; 
        action_move m player accessible_positions
    end
;;

let action_pass player =
    player.point_action <- 0;
    player.point_mvt <- 0
;;

let rec choose_action map player players_around accessible_positions =
    match read_int_min_max 1 3 with
    | 1 when can_move player -> action_move map player accessible_positions
    | 2 when can_attack player players_around -> action_attack map player players_around
    | 3 -> action_pass player
    | _ -> (Printf.printf "Impossible\n"; choose_action map player players_around accessible_positions)
;; 


while not (game_is_over players) do
    for i=0 to 5 do

        let players_around = accessible_enemies map players.(i) players in

        while can_play players.(i) players_around do

            let accessible_positions = accessible_positions map players.(i) in

            Printf.printf "\x1b%c" 'c' ;
            Printf.printf "BONNE SAINT A LICE :)\n (Vous pouvez dire que Coline est pas drôle) \n";
            Printf.printf "=== Equipe %d joueur %d ===\n" ((i/3)+1) (i mod 3);
            Array.iter print_player_color players ;

            print_map_color map accessible_positions players_around players.(i);

            Printf.printf "Choisir une action :\n" ;
            Printf.printf " 1. Deplacement%s\n" (if can_move players.(i) then "" else " (impossible)");
            Printf.printf " 2. Attaque%s\n"  (if can_attack players.(i) players_around then "" else " (impossible)");
            Printf.printf " 3. Passer son tour\n" ;

            choose_action map players.(i) players_around accessible_positions
        done
    done;

    Array.iter reset_points players

done ;;

Printf.printf "Fini\n" ;;
