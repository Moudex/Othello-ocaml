(*********************************************************
 *                                                       *
 *  Ce fichier implémente toutes le fonctions relatives  *
 *  au jeu de l'intelligence artificielle                *
 *                                                       *
 *********************************************************)

(* configuration plateau *)
type plateau_conf = Plateau.pconf
type plateaux = Plateau.plateaux

type pos = Plateau.pos
type liste_pos = Plateau.liste_pos
exception Elagage

let init conf_plat =
   () 
;;

(* Type de noeud *)
type noeud = Min | Max

let humain couleur plateaux taille_case =
    let rec humain_rec () =
        let etat = Graphics.wait_next_event[Graphics.Button_down]in
        let x = (etat.Graphics.mouse_x / taille_case)
        and y = (etat.Graphics.mouse_y / taille_case) in
        if (Plateau.cap_case plateaux.Plateau.jeu couleur x y) then
            Plateau.joue plateaux couleur x y
        else
            humain_rec ()
    in humain_rec ()
;;

let rec aleatoire couleur plateaux =
    let x = Random.int (Array.length plateaux.Plateau.jeu) in
    let y = Random.int (Array.length plateaux.Plateau.jeu.(0)) in
    match plateaux.Plateau.jeu.(x).(y) with
    | Plateau.Vide -> if (Plateau.cap_case plateaux.Plateau.jeu couleur x y) then
                        Plateau.joue plateaux couleur x y
                    else
                        aleatoire couleur plateaux
    | _ -> aleatoire couleur plateaux
;;

let rec minmax_rec p c nc r eval =
    match r with
    | 0 -> eval p c
    | _ ->
        let cp = Plateau.coups_possibles p.Plateau.jeu c in
        match cp with
        | [] -> eval p c
        | _ ->
            match nc with
            | Max ->
                let bsc = ref (-1000000) in
                List.iter (fun coup ->
                    let plats = ref (Plateau.clone p) in
                    let coul = Plateau.joue !plats c (fst coup) (snd coup) in
                    let sc = ref (minmax_rec !plats coul (match coul with | c -> Max | _ -> Min) (pred r) eval) in
                    if sc > bsc then
                        bsc := !sc
                ) cp;
                !bsc
            | Min ->
                let bsc = ref 1000000 in
                List.iter (fun coup ->
                    let plats = ref (Plateau.clone p) in
                    let coul = Plateau.joue !plats c (fst coup) (snd coup) in
                    let sc = ref (minmax_rec !plats coul (match coul with | c -> Min | _ -> Max) (pred r) eval) in
                    if sc < bsc then
                        bsc := !sc
                ) cp;
                !bsc
;;

let minmax p c r eval =
    let cp = Plateau.coups_possibles p.Plateau.jeu c in
    let bsc = ref (-1000000) in
    let bc = ref (0, 0) in
    List.iter (fun coup ->
        let plat = ref (Plateau.clone p) in
        let coul = Plateau.joue !plat c (fst coup) (snd coup) in
        let sc = ref (minmax_rec !plat coul (match coul with | c -> Max | _ -> Min) (pred r) eval) in
        if sc > bsc then
            bsc :=  !sc; bc := coup
    ) cp;
    Plateau.joue p c (fst !bc) (snd !bc)
;;


let rec alphabeta_rec p c nc r eval alpha beta =
    match r with
    | 0 -> eval p c
    | _ ->
        let cp = Plateau.coups_possibles p.Plateau.jeu c in
        match cp with
        | [] -> eval p c
        | _ ->
            match nc with
            | Max ->
                let al = ref alpha in
                (try List.iter (fun coup ->
                    let plat = ref (Plateau.clone p) in
                    let coul = Plateau.joue !plat c (fst coup) (snd coup) in
                    let sc = ref (alphabeta_rec !plat coul (match coul with | c -> Max | _ -> Min) (pred r) eval !al beta) in
                    if !sc > !al then
                        al := !sc;
                        if !al >= beta then
                            raise Elagage
                ) cp with _ -> ());
                !al
            | Min ->
                let be = ref beta in
                (try List.iter (fun coup ->
                    let plat = ref (Plateau.clone p) in
                    let coul = Plateau.joue !plat c (fst coup) (snd coup) in
                    let sc = ref (alphabeta_rec !plat coul (match coul with | c -> Min | _ -> Max) (pred r) eval alpha !be) in
                    if !sc < !be then
                        be := !sc;
                        if alpha >= !be then
                            raise Elagage
                ) cp with _ -> ());
                !be
;;

let alphabeta p c r eval =
    let cp = Plateau.coups_possibles p.Plateau.jeu c in
    let alpha = ref (-1000000) in
    let beta = ref 1000000 in
    let bsc = ref (-1000000) in
    let bc = ref (0, 0) in
    List.iter (fun coup ->
        let plat = ref (Plateau.clone p) in
        let coul = Plateau.joue !plat c (fst coup) (snd coup) in
        let sc = ref (alphabeta_rec !plat coul (match coul with | c -> Max | _ -> Min) (pred r) eval !alpha !beta) in
        if !sc > !bsc then
            bsc := !sc; bc := coup
    ) cp;
    Plateau.joue p c (fst !bc) (snd !bc)
;;
