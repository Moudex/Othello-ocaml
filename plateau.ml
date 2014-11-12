(* Definition d'une case *)
type case = Blanc | Noir | Vide

(* Configuration du plateau *)
type pconf = {
    nbcols  : int;
    nbligs  : int
}

(*definition du plateau de jeu *)
type p_jeu = case array array

(* definition du plateau d'evaluation *)
type p_eval = int array array

(* plateau de jeux et evaluation *)
type plateaux = {
    mutable jeu : p_jeu;
    mutable e_noir  : p_eval;
    mutable e_blanc : p_eval
}

(* position et liste de positions *)
type pos = (int * int)
type liste_pos = pos list

(* Directions possibles pour la capture *)
let directions = [
    (-1,-1); (-1,0); (-1,1);
    (0,-1); (0,1);
    (1,-1); (1,0); (1,1)
];;

(* Construction des plateaux *)
let const_plateau conf =
    let plats = {
        jeu = Array.init conf.nbcols (fun y->Array.init conf.nbligs (fun x->Vide));
        e_noir = Array.init conf.nbcols (fun y->Array.init conf.nbligs (fun x->0));
        e_blanc = Array.init conf.nbcols (fun y->Array.init conf.nbligs (fun x->0))
    }
;;

(* Initialisation du carré central et des plateau eval*)
let init_plateau conf =
    let plats = const_plateau conf in
    let ligs = conf.nbligs in
    let cols = conf.nbcols in
    (* Carre central *)
    plats.jeu.(cols/2-1).(ligs/2-1) <- Blanc;
    plats.jeu.(cols/2).(ligs/2) <- Blanc;
    plats.jeu.(cols/2).(ligs/2-1) <- Noir;
    plats.jeu.(cols/2-1).(ligs/2) <- Noir;
    (* plateau d'evaluation *)
    plats.e_noir.(cols/2-1).(ligs/2-1) <- 16;
    plats.e_noir.(cols/2).(ligs/2) <- 16;
    plats.e_noir.(cols/2).(ligs/2-1) <- 16;
    plats.e_noir.(cols/2-1).(ligs/2) <- 16;
    plats.e_noir.(cols/2-2).(ligs/2-1) <- 2;
    plats.e_noir.(cols/2-2).(ligs/2) <- 2;
    plats.e_noir.(cols/2+1).(ligs/2-1) <- 2;
    plats.e_noir.(cols/2+1).(ligs/2) <- 2;
    plats.e_noir.(cols/2-1).(ligs/2-2) <- 2;
    plats.e_noir.(cols/2).(ligs/2-2) <- 2;
    plats.e_noir.(cols/2-1).(ligs/2+1) <- 2;
    plats.e_noir.(cols/2).(ligs/2+1) <- 2;
    plats.e_noir.(cols/2-2).(ligs/2-2) <- 1;
    plats.e_noir.(cols/2-2).(ligs/2+1) <- 1;
    plats.e_noir.(cols/2+1).(ligs/2-2) <- 1;
    plats.e_noir.(cols/2+1).(ligs/2+1) <- 1;
    plats.e_noir.(cols-1).(ligs-1) <- 500;
    plats.e_noir.(cols-1).(0) <- 500;
    plats.e_noir.(0).(ligs-1) <- 500;
    plats.e_noir.(0).(0) <- 500;
    plats.e_noir.(cols-2).(ligs-1) <- -150;
    plats.e_noir.(cols-3).(ligs-1) <- 30;
    plats.e_noir.(cols-1).(ligs-2) <- -150;
    plats.e_noir.(cols-1).(ligs-3) <- 30;
    plats.e_noir.(cols-2).(ligs-2) <- -250;
    plats.e_noir.(cols-2).(0) <- -150;
    plats.e_noir.(cols-3).(0) <- 30;
    plats.e_noir.(cols-1).(1) <- -150;
    plats.e_noir.(cols-1).(2) <- 30;
    plats.e_noir.(cols-2).(1) <- -250;
    plats.e_noir.(0).(ligs-2) <- -150;
    plats.e_noir.(0).(ligs-3) <- 30;
    plats.e_noir.(1).(ligs-1) <- -150;
    plats.e_noir.(2).(ligs-1) <- 30;
    plats.e_noir.(1).(ligs-2) <- -250;
    plats.e_noir.(0).(1) <- -150;
    plats.e_noir.(0).(2) <- 30;
    plats.e_noir.(1).(0) <- -150;
    plats.e_noir.(2).(0) <- 30;
    plats.e_noir.(1).(1) <- -250;
    for i=3 to ligs-4 do
        for j=3 to cols-4 do
            plats.e_noir.(0).(i) <- 10;
            plats.e_noir.(cols-1).(i) <- 10;
            plats.e_noir.(j).(0) <- 10;
            plats.e_noir.(j).(ligs-1) <- 10;
        done;
    done;
    (* copie eval blanc *)
    for i=0 to ligs-1 do
        for j=0 to cols-1 do
            plats.e_blanc.(j).(i) <- plats.e_noir.(j).(i);
        done;
    done;
    plats
;;

(* Copie les plateaux *)
let clone plateaux =
    let cols = Array.length plateaux.jeu in
    let ligs = Array.length plateaux.jeu.(0) in
    let plats = {
        jeu = Array.init cols (fun y->Array.init ligs (fun x->plateaux.jeu.(y).(x)));
        e_noir = Array.init cols (fun y->Array.init ligs (fun x->plateaux.e_noir.(y).(x)));
        e_blanc = Array.init cols (fun y->Array.init ligs (fun x->plateaux.e_blanc.(y).(x)))
    }
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

(* nb coups possible *)
let nbcoups_possibles p c=
    let nb = ref 0 in
    for i = 0 to (Array.length p) -1 do
        for j = 0 to (Array.length p.(0)) -1 do
            if (cap_case p c i j) then
                nb := 1 + !nb
        done;
    done;
    !nb
;;

(* Met a jour le plateau de force de la couleur *)
let maj_force plateaux c x y =
    
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
