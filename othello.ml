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
