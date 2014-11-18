(* configuration plateau *)
type plateau_conf = Plateau.pconf;;

(* configuration affichage *)
type affichage_conf = Ograph.aconf;; 

(* configuration jeu *)
type jeu_conf = Jeu.jconf;;

let confj = {
    Jeu.noir = Jeu.Humain;
    Jeu.eval_noir = Jeu.Materiel;
    Jeu.lvl_noir = 3;
    Jeu.blanc = Jeu.Humain;
    Jeu.eval_blanc = Jeu.Materiel;
    Jeu.lvl_blanc = 3
}

let confp= {
    Plateau.nbligs=8;
    Plateau.nbcols=8
};;

let confa = {
    Ograph.bg_r=199;
    Ograph.bg_g=222;
    Ograph.bg_b=109;
    Ograph.taille_case=25
};;

exception Fin_partie;;

let get_eval c =
    let mode =
        match c with
        | Plateau.Blanc -> confj.Jeu.eval_blanc
        | Plateau.Noir -> confj.Jeu.eval_noir
    in
    match mode with
    | Jeu.Materiel -> Plateau.nb_pieces
    | Jeu.Mobilitee -> Plateau.nbcoups_possibles
    | Jeu.Force -> Plateau.force
;;

let get_lvl c =
    match c with
    | Plateau.Blanc -> confj.Jeu.lvl_blanc
    | Plateau.Noir -> confj.Jeu.lvl_noir
;;

let get_joueur c =
    match c with
    | Plateau.Blanc -> confj.Jeu.blanc
    | Plateau.Noir -> confj.Jeu.noir
;;

let jeu () =

    Menu.menu confp confa confj;
    let plats = ref (Plateau.init_plateau confp) in
    let tour = ref Plateau.Noir in
    Ia.init confp;
    while not (!tour = Plateau.Vide) do
        Ograph.aff_plateau confa !plats.Plateau.jeu;
        Ograph.aff_scores
            ("Blanc : " ^ (string_of_int (Plateau.nb_pieces !plats
            Plateau.Blanc))^"ma "^(string_of_int (Plateau.nbcoups_possibles !plats
            Plateau.Blanc))^"mo "^(string_of_int (Plateau.force !plats
            Plateau.Blanc))^"fo")
            ("Noir : " ^ (string_of_int (Plateau.nb_pieces !plats
            Plateau.Noir))^"ma "^(string_of_int (Plateau.nbcoups_possibles !plats
            Plateau.Noir))^"mo "^(string_of_int (Plateau.force !plats
            Plateau.Noir))^"fo");
        (match !tour with
        | Plateau.Blanc -> Ograph.aff_message "Au tour des blancs..."
        | Plateau.Noir -> Ograph.aff_message "Au tour des noirs...");
        tour := match (get_joueur !tour) with
        | Jeu.Humain -> Ia.humain !tour !plats confa.Ograph.taille_case
        | Jeu.Aleatoire -> Ia.aleatoire !tour !plats
        | Jeu.MinMax -> Ia.minmax !plats !tour (get_lvl !tour) (get_eval !tour)
        | Jeu.AlphaBeta -> Ia.alphabeta !plats !tour (get_lvl !tour) (get_eval !tour)
    done;
    Ograph.aff_plateau confa !plats.Plateau.jeu;
    Ograph.aff_message "Pressez une touche pour quiter";
    Graphics.wait_next_event[Graphics.Key_pressed];
    Graphics.close_graph
;;

let main () =
    jeu()
;;

main();
