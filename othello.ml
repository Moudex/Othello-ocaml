(* configuration plateau *)
type plateau_conf = Plateau.pconf;;

(* configuration affichage *)
type affichage_conf = Ograph.aconf;; 

(* configuration du jeu *)
type jeu_conf = Jeu.jconf;;

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

let jeu () =

    Menu.menu confp confa confj;
    let plat = ref (Plateau.init_plateau confp) in
    Ograph.aff_plateau confa !plat
;;

let main () =
    jeu()
;;

main();
