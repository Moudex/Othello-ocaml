(* configuration plateau *)
type plateau_conf = Plateau.pconf;;

(* configuration affichage *)
type affichage_conf = Ograph.aconf;; 

(* configuration du jeu *)
type jeu_conf = Jeu.jconf;;

(* couleur des joueurs *)
type couleur = Plateau.case;;

let confj= {
    Jeu.noir = Jeu.Humain;
    Jeu.blanc = Jeu.Humain
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
    let plat = ref (Plateau.init_plateau confp) in
    let tour = ref Plateau.Noir in
    Ia.init confp;
    while not (!tour = Plateau.Vide) do
        Ograph.aff_plateau confa !plat;
        tour := match (match !tour with
                | Plateau.Noir -> Ograph.aff_message "Au tour des noirs..."; confj.Jeu.noir
                | Plateau.Blanc -> Ograph.aff_message "Au tour des blancs..."; confj.Jeu.blanc
                | _ -> raise Fin_partie)
        with
        | Jeu.Humain -> Ia.humain !tour !plat confa.Ograph.taille_case
        | Jeu.Aleatoire -> Ia.aleatoire !tour !plat
        | Jeu.Naif -> Ia.naif !tour !plat
        | Jeu.MinMax -> Ia.minmax !plat !tour 3 Jeu.score_naif
        | Jeu.AlphaBeta -> Ia.alphabeta !tour !plat;
    done;
    Ograph.aff_plateau confa !plat;
    Ograph.aff_message "Pressez un touche pour quiter";
    Graphics.wait_next_event[Graphics.Key_pressed];
    Graphics.close_graph
;;

let main () =
    jeu()
;;

main();
