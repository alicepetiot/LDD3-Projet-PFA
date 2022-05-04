type map = {
    matrix: char array array;
    width: int;
    height: int
} ;;

let empty_map width height = {
    matrix = Array.make_matrix height width ' ';
    width = width;
    height = height
} ;;

let map_get m x y =
    m.matrix.(y).(x)
;;

let map_set m x y c =
    m.matrix.(y).(x) <- c
;;

let on_map m x y =
    x >= 0 && y >= 0 && x < m.width && y < m.height
;;

let build_walls m d =
    for x=0 to m.width-1 do 
        map_set m x 0 '#';
        map_set m x (m.height-1) '#';
    done;
    for y=1 to m.height-2 do 
        map_set m 0 y '#';
        map_set m (m.width-1) y '#';
    done;
    for x=1 to m.width-2 do 
        for y=1 to m.height-2 do 
            if (Random.float 1.) < d then map_set m x y '#';
        done;
    done;
;;

let create_map width height d = 
    let m = empty_map width height in
    build_walls m d;
    m
;;

let print_map m =
    for y=0 to m.height-1 do 
        for x=0 to m.width-1 do 
            Printf.printf "%c " (map_get m x y);
        done;
        print_newline ();
    done;
;;

let string_of_map m =
    let x = ref 0 in
    let y = ref 0 in
    String.init ((m.width + 1) * m.height - 1) (fun _ ->
        if !x = m.width then
            (x := 0; y := !y + 1; '|')
        else
            (x := !x + 1; map_get m (!x - 1) !y)
    )
;;

let map_of_string s =
    let width = String.index_from s 0 '|' in
    let height = ((String.length s) + 1) / (width + 1) in
    let m = empty_map width height in
    let x = ref 0 in
    let y = ref 0 in
    String.iter (fun c ->
        if !x = m.width then
            (x := 0; y := !y + 1)
        else
            (x := !x + 1; map_set m (!x - 1) !y c)
    ) s; m
;;
