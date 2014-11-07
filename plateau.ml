(* Definition d'une case *)
type case = Blanc | Noir | Vide

(* Configuration du plateau *)
type pconf = {
    nbcols  : int;
    nbligs  : int
}

(*definition du plateau *)
type plateau = case array array

(* position et liste de positions *)
type pos = (int * int)
type liste_pos = pos list

(* Directions possibles pour la capture *)
let directions = [
    (-1,-1); (-1,0); (-1,1);
    (0,-1); (0,1);
    (1,-1); (1,0); (1,1)
];;

(* Construction du plateau *)
let const_plateau conf =
    Array.init conf.nbcols
        (fun y -> Array.init conf.nbligs
            (fun x -> Vide)
        )
;;

(* Initialisation du carré central *)
let init_plateau conf =
    let plateau = const_plateau conf in
    plateau.((Array.length plateau)/2-1).((Array.length plateau)/2-1) <- Blanc;
    plateau.((Array.length plateau)/2).((Array.length plateau)/2) <- Blanc;
    plateau.((Array.length plateau)/2).((Array.length plateau)/2-1) <- Noir;
    plateau.((Array.length plateau)/2-1).((Array.length plateau)/2) <- Noir;
    plateau
;;

(* Iterateur sur l'ensemble des cases du plateau *)
let iter plateau f =
    for i=0 to ((Array.length plateau)-1) do for j=0 to ((Array.length
    plateau.(0))-1) do f (i,j) done done
;;

(* Copie le plateau *)
let clone plateau =
    Array.init (Array.length plateau) (fun y ->
        Array.init (Array.length plateau.(0)) (fun x -> plateau.(y).(x)))
;;

(* Faction opposée *)
let adversaire c =
    match c with
    | Blanc -> Noir
    | Noir -> Blanc
    | _ -> Vide
;;

(* La position est elle sur le plateau *)
let valid_pos plateau x y =
    x >= 0 && y >= 0 &&
    y < Array.length plateau &&
    x < Array.length plateau.(0)
;;

(* Peut on capturer des pieces dans cette direction *)
let cap_dir plateau c (x,y) (dx, dy) =
    let rec cap_dir_rec (x,y) valid =
        if not (valid_pos plateau x y) then
            false
        else (
            match plateau.(x).(y) with
            | Vide -> false
            | case -> if case = (adversaire c) then
                    cap_dir_rec (x + dx, y + dy) true
                else
                    valid
        )
    in cap_dir_rec (x + dx, y + dy) false
;;

(* Peut on capturer des pieces avec cette case *)
let cap_case plateau c x y =
    if not (valid_pos plateau x y) then
        false
    else
        match plateau.(x).(y) with
        | Vide -> (true && (List.fold_left (fun a b -> a || b) false (List.map
        (fun d -> cap_dir plateau c (x, y) d) directions)))
        | _ -> false
;;

(* Liste les coups possibles *)
let coups_possibles p c =
    let coups = ref [] in
    for i = 0 to (Array.length p) -1 do
        for j = 0 to (Array.length p.(0)) -1 do
            if (cap_case p c i j) then
                (coups := (i, j)::!coups)
        done;
    done;
    !coups
;;


(* Joue une couleur *)
let joue_c plateau c x y =
    (List.iter
        (fun (dx, dy) -> if (cap_dir plateau c (x, y) (dx, dy)) then
            let rec retourne (x, y) =
                if (valid_pos plateau x y) then
                    if (plateau.(x).(y) = (adversaire c)) then
                        (plateau.(x).(y) <- c; retourne (x + dx, y + dy))
                    in retourne (x + dx, y + dy)
        )
    directions);
    plateau.(x).(y) <- c
;;

(* Compte le nombre de pieces d'une couleur donnée *)
let nombre plateau couleur =
    let res = ref 0 in
    for i = 0 to (Array.length plateau) -1 do
        for j = 0 to (Array.length plateau.(0)) -1 do
            if plateau.(i).(j) = couleur then res := succ !res;
        done;
    done;
    !res
;;

(* Determine le vainqueur *)
let vainqueur plateau =
    if (nombre plateau Blanc) > 32 then
        Blanc
    else if (nombre plateau Noir) > 32 then
        Noir
    else
        Vide
;;

(* La partie est elle finie *)
let fin plateau =
let fini = ref true in
    for i=0 to Array.length plateau-1 do
        for j=0 to Array.length plateau.(0)-1 do
            if plateau.(j).(i) = Vide then
                fini := false
        done;
done;
!fini
;;

(* La couleur peut elle jouer *)
let peut_jouer plateau c =
    let jouer = ref false in
        for i=0 to Array.length plateau-1 do
            for j=0 to Array.length plateau.(0)-1 do
                    if (cap_case plateau c i j) then
                        jouer := true
            done;
        done;
    !jouer
;;

(* Joue et retourne la couleur qui doit jouer *)
let joue plateau c x y =
    joue_c plateau c x y;
    if fin plateau then
        Vide
    else 
        match peut_jouer plateau (adversaire c) with
        | false -> c
        | _ -> adversaire c
;;
