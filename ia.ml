(*********************************************************
 *                                                       *
 *  Ce fichier implémente toutes le fonctions relatives  *
 *  au jeu de l'intelligence artificielle                *
 *                                                       *
 *********************************************************)

(* configuration plateau *)
type plateau_conf = Plateau.pconf

type pos = Plateau.pos
type liste_pos = Plateau.liste_pos

let init conf_plat =
   () 
;;

(* Type de noeud *)
type noeud = Min | Max

let humain couleur plateau taille_case =
    let rec humain_rec () =
        let etat = Graphics.wait_next_event[Graphics.Button_down]in
        let x = (etat.Graphics.mouse_x / taille_case)
        and y = (etat.Graphics.mouse_y / taille_case) in
        if (Plateau.cap_case plateau couleur x y) then
            Plateau.joue plateau couleur x y
        else
            humain_rec ()
    in humain_rec ()
;;

let rec aleatoire couleur plateau =
    let x = Random.int (Array.length plateau) in
    let y = Random.int (Array.length plateau.(0)) in
    match plateau.(x).(y) with
    | Plateau.Vide -> if (Plateau.cap_case plateau couleur x y) then
                        Plateau.joue plateau couleur x y
                    else
                        aleatoire couleur plateau
    | _ -> aleatoire couleur plateau
;;

let naif couleur plateau =
    Plateau.Vide 
;;

(* Algorithme minmax recursif *)
let rec minmax_rec p c nc r eval =
    match r with
    | 0 -> eval c
    | _ ->
        let cp = Plateau.coups_possibles p c in
        match cp with
        | [] -> eval c
        | _ ->
            match nc with
            | Max ->
                List.fold_left (fun tmp suiv -> max tmp (minmax_rec (Plateau.joue p
                (fst suiv) (snd suiv)) (Plateau.adversaire c) Min (pred r) eval )) 
                -1000000 cp
            | Min ->
                List.fold_left (fun tmp suiv -> min tmp (minmax_rec (Plateau.joue p
                (fst suiv) (snd suiv)) (Plateau.adversaire c) Max (pred r) eval )) 
                1000000 cp
;;

let minmax p c r eval =
    if (r < 0) then
        failwith "Rang incorrect"
    let plat = ref (Plateau.clone p) in
    let cp = Plateau.coup_possibles !plat c in
    let valeurs = (List.map (fun x -> minmax_rec (Plateau.joue !plat (fst x) (snd x))
    (Plateau.adversaire c) Min (pred r) eval) cp) in
    let coup = ref List.head cp in
    let max = ref List.head valeurs in
        for i = 0 to List.length cp do
            if ((List.nth valeurs i) > !max) then
                !max := (List.nth valeurs i); !coup := (List.nth cp i)
        done;
    Plateau.joue p c (fst !coup) (snd !coup)
;;

let alphabeta couleur plateau =
    Plateau.Vide
;;
