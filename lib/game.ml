open Player ;;
open Map ;;
open Action ;;

let accessibles map player range visit_pos = 
    let positions = Hashtbl.create 100 in
    let rec accessibles_rec pos dmax =
        let cond = match Hashtbl.find_opt positions pos with
        | None -> true
        | Some (d) -> d < dmax
        in if cond && dmax >= 0 then begin
        	visit_pos pos;
            if (map_get map pos.x pos.y) = ' ' || pos = player.pos then begin
	            Hashtbl.add positions pos dmax;
	            accessibles_rec { pos with x = pos.x + 1 } (dmax - 1);
	            accessibles_rec { pos with x = pos.x - 1 } (dmax - 1);
	            accessibles_rec { pos with y = pos.y + 1 } (dmax - 1);
	            accessibles_rec { pos with y = pos.y - 1 } (dmax - 1) 
        	end
        end
    in accessibles_rec player.pos range;
    positions
;;

let accessible_positions map player =
    let positions = accessibles map player player.point_mvt (fun _ -> ()) in
    Hashtbl.remove positions player.pos;
    positions
;;

let accessible_enemies map player players =
	let enemies = ref [] in
	let _ = accessibles map player player.attack.range (fun pos ->
		match map_get map pos.x pos.y with
		| '0'..'5' as c -> 
			let id = id_of_char c in
			if (id mod 2) != (player.id mod 2) && is_alive players.(id) && not (List.mem id !enemies) then
				enemies := id :: !enemies
		| _ -> ()
	) in !enemies
;;

let rec add_player map player = 
    let x = (if player.id mod 2 = 0 then map.width / 4 else map.width * 3 / 4) + Random.int (map.width / 8) in
    let y = 1 + Random.int (map.height - 2) in
    let c = map_get map x y in
    if c = ' ' then
        (player.pos.x <- x; player.pos.y <- y; map_set map x y (string_of_int player.id).[0])
    else
        add_player map player
;;

let attack_player map p1 p2 =
    p2.life <- p2.life - p1.strength / 10 - p1.attack.dmg;
    p1.point_action <- p1.point_action - p1.attack.point_action;
    if p2.life <= 0 then
        map_set map p2.pos.x p2.pos.y ' '
;;

let move_player_unsafe map player x y d =
	map_set map player.pos.x player.pos.y ' '; 
    map_set map x y (string_of_int player.id).[0];
    player.pos.x <- x;
    player.pos.y <- y;
    player.point_mvt <- d;
;;

let move_player map player x y accessible_positions =
	match Hashtbl.find_opt accessible_positions { x = x; y = y } with
	| None -> false
	| Some (d) -> begin
        move_player_unsafe map player x y d;
	    true
	end
;;

let move_distance accessible_positions x y =
    match Hashtbl.find_opt accessible_positions { x = x; y = y } with
	| None -> 0
	| Some (d) -> d
;;

let print_map_color map accessible_positions players_around player =
    for y=0 to map.height-1 do
        for x=0 to map.width-1 do
            let c = map_get map x y in match c with
            | '0' | '2' | '4' -> Printf.printf "\x1b[1;35%sm%c\x1b[0m " 
                (if (List.mem (id_of_char c) players_around) && player.id mod 2 = 1 then ";107" else "") c
            | '1' | '3' | '5' -> Printf.printf "\x1b[1;36%sm%c\x1b[0m " 
                (if (List.mem (id_of_char c) players_around) && player.id mod 2 = 0 then ";107" else "") c
            | '#' -> 
                Printf.printf "\x1b[1;93m☻\x1b[0m "
            | ' ' when Hashtbl.mem accessible_positions (create_pos x y) -> 
                Printf.printf "\x1b[91m♥\x1b[0m "
            | _ -> Printf.printf "  "
        done;
        print_newline ();
    done;
;;

let game_is_over players =
    players.(0).life + players.(2).life + players.(4).life = 0
    || players.(1).life + players.(3).life + players.(5).life = 0
;;

let play_action action map players num =
    match action with
    | AttackAction (x, y) ->
        attack_player map players.(num) players.(id_of_char (map_get map x y))
    | MoveAction (x, y, d) ->
        move_player_unsafe map players.(num) x y d
    | _ ->
        ()
;;