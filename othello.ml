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

let jeu () =

    Menu.menu confp confa confj;
    let plats = ref (Plateau.init_plateau confp) in
    let tour = ref Plateau.Noir in
    Ia.init confp;
    while not (!tour = Plateau.Vide) do
        Ograph.aff_plateau confa !plats.Plateau.jeu;
        Ograph.aff_scores
            ("Blanc : " ^ (string_of_int (Plateau.nombre !plats
            Plateau.Blanc))^"ma "^(string_of_int (Plateau.nbcoups_possibles !plats
            Plateau.Blanc)^"mo"))
            ("Noir : " ^ (string_of_int (Plateau.nombre !plats
            Plateau.Noir))^"ma "^(string_of_int (Plateau.nbcoups_possibles !plats
            Plateau.Noir)^"mo"));
        tour := match (match !tour with
                | Plateau.Noir -> Ograph.aff_message "Au tour des noirs..."; confj.Jeu.noir
                | Plateau.Blanc -> Ograph.aff_message "Au tour des blancs..."; confj.Jeu.blanc
                | _ -> raise Fin_partie)
        with
        | Jeu.Humain -> Ia.humain !tour !plats confa.Ograph.taille_case
        | Jeu.Aleatoire -> Ia.aleatoire !tour !plats
        | Jeu.MinMax -> Ia.minmax !plats !tour (match !tour with 
                                                            | Plateau.Blanc -> confj.Jeu.lvl_blanc
                                                            | Plateau.Noir -> confj.Jeu.lvl_noir)
        Plateau.nombre
        | Jeu.AlphaBeta -> Ia.alphabeta !plats !tour (match !tour with 
                                                                    | Plateau.Blanc -> confj.Jeu.lvl_blanc
                                                                    | Plateau.Noir -> confj.Jeu.lvl_noir)
        Plateau.nbcoups_possibles
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
