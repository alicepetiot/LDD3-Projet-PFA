open Lib.Player
open Lib.Game
open Lib.Map

let init_sounds () = 
  let sounds = Sys.readdir "../../sounds" in
  print_string sounds.(0)
;;

init_sounds();;

(* Renvoie le meilleur mouvement *)
let best_moves map amis ennemies =
    let p1 = amis.(0) in 
    let p2 = amis.(1) in 
    let p3 = amis.(2) in 
    let p4 = ennemies.(0) in 
    let p5 = ennemies.(1) in 
    let p6 = ennemies.(2) in 
    let moves = [] in 
    Printf.printf "cc"
;;