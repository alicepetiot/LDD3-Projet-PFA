type attack = {
	dmg: int;
	range: int;
	point_action: int
} ;;

type pos = {
	mutable x: int;
	mutable y: int
} ;;

type player = {
	name: string;
	id: int;
	strength: int;
	mutable life: int;
	max_point_action: int;
	mutable point_action: int;
	max_point_mvt: int;
	mutable point_mvt: int;
	attack: attack;
	pos: pos;
} ;;


let create_pos x y =  { x = x; y = y } ;;

let create_attack dmg range pa = {
	dmg = dmg;
	range = range;
	point_action = pa
} ;;

let create_player name id strength life pa pm att = {
	name = name; 
	id = id;
	strength = strength;
	life = life;
	max_point_action = pa;
	point_action = pa;
	max_point_mvt = pm;
	point_mvt = pm;
	attack = att;
	pos = { x = 0; y = 0 }
} ;;

let create_player_serv name id strength life pa pm y x att = {
	name = name; 
	id = id;
	strength = strength;
	life = life;
	max_point_action = pa;
	point_action = pa;
	max_point_mvt = pm;
	point_mvt = pm;
	attack = att;
	pos = { x = x; y = y }
} ;;

let empty_player =
	create_player "" 0 0 0 0 0 (create_attack 0 0 0)
;;

let can_move p =
	p.point_mvt > 0
;;

let player_can_move p =
	p.point_mvt > 0
;;

let can_attack p players_around =
	p.point_action >= p.attack.point_action && players_around != []
;;

let player_can_attack p =
	p.point_action >= p.attack.point_action
;;

let is_alive p =
	p.life > 0
;;

let can_play p players_around =
	(is_alive p) && ((can_move p) || (can_attack p players_around))
;;

let reset_points p =
	p.point_action <- p.max_point_action;
	p.point_mvt <- p.max_point_mvt
;;

let player_is_valid p =
	p.life + p.strength = 100 
	&& p.life >= 1
	&& p.strength >= 1
	&& p.strength >= 1
	&& p.point_action + p.point_mvt = 15
	&& ((p.attack.range * 10) + p.attack.dmg) - ((p.point_action + 1) * 10) <= 0
;;

let string_of_player p =
	Printf.sprintf "%d|%d|%d|%d|%d|%d|%d" p.strength p.life p.point_action p.point_mvt p.attack.dmg p.attack.range p.attack.point_action
;;

let string_of_player_serv p =
	Printf.sprintf "%d|%d|%d|%d|%d*%d|%d|%d|%d" p.strength p.life p.point_action p.point_mvt p.pos.x p.pos.y p.attack.dmg p.attack.range p.attack.point_action
;;

let player_of_string name s id =
	Scanf.sscanf s "%d|%d|%d|%d|%r" (fun s -> Scanf.bscanf s "%d|%d|%d" create_attack) (create_player name id)
;;

let player_of_string_serv name s id =
	Scanf.sscanf s "%d|%d|%d|%d|%d*%d|%r" (fun s -> Scanf.bscanf s "%d|%d|%d" create_attack) (create_player_serv name id)
;;

let player_array_of_string s =
	Array.of_list (List.mapi (fun id s -> player_of_string (string_of_int id) s id) (String.split_on_char ';' s))
;;

let player_array_of_string_serv s =
	Array.of_list (List.mapi (fun id s -> player_of_string_serv (string_of_int id) s id) (String.split_on_char ';' s))
;;

let player_data p = 
	Printf.sprintf "{ Force: %2d; Vie: %2d;	PA: %2d; PM: %2d; Attaque: { Dmg: %2d; Range: %2d; PA: %2d }}"
	p.strength p.life p.point_action p.point_mvt p.attack.dmg p.attack.range p.attack.point_action
;;

let print_player p = 
    Printf.printf "%s %d(%d,%d) : " p.name p.id p.pos.x p.pos.y ;
    Printf.printf "{Force : %d; Vie : %d; PA : %d; PM : %d; Attaque : {Dmg : %d; Range : %d; PA : %d}}\n"
    p.strength p.life p.point_action p.point_mvt p.attack.dmg p.attack.range p.attack.point_action
;;

let player_data_color p =
	Printf.sprintf "{ \x1b[97mForce\x1b[0m: \x1b[92m%2d\x1b[0m; \x1b[97mVie\x1b[0m: \x1b[92m%2d\x1b[0m; \
	\x1b[97mPA\x1b[0m: \x1b[92m%2d\x1b[0m; \x1b[97mPM\x1b[0m: \x1b[92m%2d\x1b[0m; \x1b[97mAttaque\x1b[0m: \
	{ \x1b[97mDmg\x1b[0m: \x1b[92m%2d\x1b[0m; \x1b[97mRange\x1b[0m: \x1b[92m%2d\x1b[0m; \x1b[97mPA\x1b[0m: \x1b[92m%2d\x1b[0m }}"
	p.strength p.life p.point_action p.point_mvt p.attack.dmg p.attack.range p.attack.point_action
;;

let print_player_color p = 
    Printf.printf "%-9s \x1b[1;%sm%d\x1b[0m(\x1b[92m%2d\x1b[0m, \x1b[92m%2d\x1b[0m): %s\n" 
    p.name (if p.id mod 2 = 0 then "35" else "36") p.id p.pos.x p.pos.y (if is_alive p then player_data_color p else "\x1b[91mDes CDs\x1b[0m");
;;

let id_of_char c =
	(Char.code c) - (Char.code '0')
;;