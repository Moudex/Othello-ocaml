type config = {
    nbcols  : int;
    nbrows  : int
} ;;
type coord = {
    x   : int;
    y   : int
} ;;
type cell = White | Black | Empty
type board = cell array array

(* Configuration par défaut *)
let default_config = { nbcols=8; nbrows=8 } ;;

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
