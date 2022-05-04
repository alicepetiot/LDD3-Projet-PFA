open Lib.Player

let player1 = create_player "test" 564 70 90 10 20 (create_attack 10 10 10)

let%test _ = player1.id = 0