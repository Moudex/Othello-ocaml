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



let rec minmax_rec p c nc r eval =
    match r with
    | 0 -> eval p c
    | _ ->
        let cp = Plateau.coups_possibles p c in
        match cp with
        | [] -> eval p c
        | _ ->
            match nc with
            | Max ->
                let bsc = ref (-1000000) in
                List.iter (fun coup ->
                    let plat = ref (Plateau.clone p) in
                    let coul = Plateau.joue !plat c (fst coup) (snd coup) in
                    let sc = ref (minmax_rec !plat coul (match coul with | c -> Max | _ -> Min) (pred r) eval) in
                    if sc > bsc then
                        bsc := !sc
                ) cp;
                !bsc
            | Min ->
                let bsc = ref 1000000 in
                List.iter (fun coup ->
                    let plat = ref (Plateau.clone p) in
                    let coul = Plateau.joue !plat c (fst coup) (snd coup) in
                    let sc = ref (minmax_rec !plat coul (match coul with | c -> Min | _ -> Max) (pred r) eval) in
                    if sc < bsc then
                        bsc := !sc
                ) cp;
                !bsc
;;

let minmax p c r eval =
    let cp = Plateau.coups_possibles p c in
    let bsc = ref (-1000000) in
    let bc = ref (List.hd cp) in
    List.iter (fun coup ->
        let plat = ref (Plateau.clone p) in
        let coul = Plateau.joue !plat c (fst coup) (snd coup) in
        let sc = ref (minmax_rec !plat coul (match coul with | c -> Max | _ -> Min) (pred r) eval) in
        if sc > bsc then
            bsc :=  !sc; bc := coup
    ) cp;
    Plateau.joue p c (fst !bc) (snd !bc)
;;

let alphabeta couleur plateau =
    Plateau.Vide
;;
