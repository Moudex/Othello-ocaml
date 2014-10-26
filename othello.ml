(********************************
 ****** PARTIE CONTROLLEUR ******
 ********************************)
type config = {
    nbcols  : int;
    nbrows  : int
} ;;
type coord = {
    x   : int;
    y   : int
} ;;
type cell = White | Black | Empty ;;
type board = cell array array ;;

(* Configuration par défaut *)
let default_config = { nbcols=8; nbrows=8 } ;;
let cell_size = ref 25
let bg_r = ref 199
let bg_g = ref 222
let bg_b = ref 109

(* Directions possibles pour la capture *)
let directions = [
    (-1,-1); (-1, 0); (-1, 1);
    (0,-1); (0,1);
    (1,-1); (1,0); (1,1)
];;

(* Iterateur sur l'ensemble des cases du plateau *)
let iter_on_cell cf f =
    for i=0 to cf.nbcols-1 do for j=0 to cf.nbrows-1 do f (i,j) done done
;;

(* Construction du plateau *)
let make_board =
    Array.init default_config.nbcols (fun y -> Array.init default_config.nbrows
    (fun x -> Empty)
    )
;;

(* Placement du carré central *)
let init_board =
    let board = make_board in
    board.((Array.length board)/2-1).((Array.length board)/2-1) <- White;
    board.((Array.length board)/2).((Array.length board)/2) <- White;
    board.((Array.length board)/2).((Array.length board)/2-1) <- Black;
    board.((Array.length board)/2-1).((Array.length board)/2) <- Black;
    board
;;

(* verifie si la position est sur le plateau *)
let check_pos board x y =
    x >= 0 && y >= 0 && y < Array.length board && x < Array.length board.(0)
;;

(* Retourne la faction opposee *)
let get_opponent c =
    match c with
    | White -> Black
    | _ -> White
;;

(* Teste si on capture des pieces dans cette direction *)
let playable_dir board c (x,y) (dx, dy) =
    let rec playable_dir_rec (x,y) valid =
        if not (check_pos board x y) then
            false
        else (
            match board.(x).(y) with
            | Empty -> false
            | cell -> if cell = (get_opponent c) then
                    playable_dir_rec (x + dx, y + dy) true
                else
                    valid
        )
    in playable_dir_rec (x + dx, y + dy) false
;;

(* Teste si on peut jouer cette case *)
let playable_cell board c x y =
    if not (check_pos board x y) then
        false
    else
        match board.(x).(y) with
        | Empty -> (true && (List.fold_left (fun a b -> a || b) false (List.map
        (fun d -> playable_dir board c (x, y) d) directions)))
        | _ -> false
;;

(* Joue une case *)
let play_cell board c x y =
    (List.iter
        (fun (dx, dy) -> if (playable_dir board c (x, y) (dx, dy)) then
            let rec take (x, y) =
                if (check_pos board x y) then
                    if (board.(x).(y) = (get_opponent c)) then
                        (board.(x).(y) <- c; take (x + dx, y + dy))
                    in take (x + dx, y + dy)
        )
    directions);
    board.(x).(y) <- c
;;

(* La partie est elle finie *)
let is_finished board =
let finished = ref true in
    for i=0 to Array.length board-1 do
        for j=0 to Array.length board.(i)-1 do
            if board.(j).(i) = Empty then
                finished := false;
        done;
done;
!finished
;;


(******************************
 ****** PARTIE GRAPHIQUE ******
 ******************************)
(* Affiche une case *)
let display_cell board x y =
    Graphics.set_color (Graphics.rgb !bg_r !bg_g !bg_b);
    Graphics.fill_rect
        (y * !cell_size + 1)
        (x * !cell_size + 1)
        (!cell_size - 2)
        (!cell_size - 2);
    Graphics.set_color Graphics.black;
    Graphics.draw_rect
        (y * !cell_size)
        (x * !cell_size)
        (y + !cell_size - y)
        (x + !cell_size - x);
    Graphics.set_color (match board.(y).(x) with
                        | Black -> Graphics.black
                        | _ -> Graphics.white);
        if ( not ( board.(y).(x) = Empty)) then
        (
            Graphics.fill_circle
                (y * !cell_size + !cell_size/2)
                (x * !cell_size + !cell_size/2)
                (!cell_size / 2 - 2);
            Graphics.set_color Graphics.black;
            Graphics.draw_circle
                (y * !cell_size + !cell_size/2)
                (x * !cell_size + !cell_size/2)
                (!cell_size / 2 -2);
        )
;;

(* Affiche le plateau *)
let display_board board =
    Graphics.open_graph
    (Printf.sprintf
    " %dx%d"
    (!cell_size * Array.length board.(0)+1)
    (21 + !cell_size * Array.length board.(0)));
    for i=0 to Array.length board-1 do
        for j=0 to Array.length board.(i)-1 do
            display_cell board i j;
        done;
    done;
;;

(* Affichage des massages *)
let display_message message =
    Graphics.moveto 2 (Graphics.size_y()-18);
    Graphics.set_color Graphics.black;
    Graphics.draw_string message
;;


(***********************
 ****** PARTIE IA ******
 ***********************)


(***********************
 ****** LANCEMENT ******
 ***********************)
let game() =
    let board = ref make_board in
    display_board !board;

    Graphics.close_graph
;;

let main () =
    game()
;;

main();
