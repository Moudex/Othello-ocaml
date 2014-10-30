(*********************************************************
 *                                                       *
 *  Ce fichier implémente toutes le fonctions relatives  *
 *  au jeu de l'intelligence artificielle                *
 *                                                       *
 *********************************************************)

(* configuration plateau *)
type plateau_conf = Plateau.pconf;;

let init conf_plat =
   () 
;;

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

let minmax couleur plateau =
   Plateau.Vide
;;

let alphabeta couleur plateau =
    Plateau.Vide
;;
