(* Configuration de l'affichage *)
type aconf = {
    bg_r    : int;
    bg_g    : int;
    bg_b    : int;
    taille_case : int
}

(* Affiche une case *)
let aff_case conf plateau x y =
    Graphics.set_color (Graphics.rgb conf.bg_r conf.bg_g conf.bg_b);
    Graphics.fill_rect
        (y * conf.taille_case + 1)
        (x * conf.taille_case + 1)
        (conf.taille_case - 2)
        (conf.taille_case - 2);
    Graphics.set_color Graphics.black;
    Graphics.draw_rect
        (y * conf.taille_case)
        (x * conf.taille_case)
        (y + conf.taille_case - y)
        (x + conf.taille_case - x);
    Graphics.set_color (match plateau.(y).(x) with
                        | Plateau.Noir -> Graphics.black
                        | _ -> Graphics.white);
        if ( not ( plateau.(y).(x) = Plateau.Vide)) then
        (
            Graphics.fill_circle
                (y * conf.taille_case + conf.taille_case/2)
                (x * conf.taille_case + conf.taille_case/2)
                (conf.taille_case / 2 - 2);
            Graphics.set_color Graphics.black;
            Graphics.draw_circle
                (y * conf.taille_case + conf.taille_case/2)
                (x * conf.taille_case + conf.taille_case/2)
                (conf.taille_case / 2 -2);
        )
;;

(* Affiche le plateau *)
let aff_plateau conf plateau =
    Graphics.open_graph
    (Printf.sprintf
    " %dx%d"
    (conf.taille_case * Array.length plateau.(0)+1)
    (21 + conf.taille_case * Array.length plateau.(0)));
    for i=0 to Array.length plateau-1 do
        for j=0 to Array.length plateau.(i)-1 do
            aff_case conf plateau i j;
        done;
    done;
;;

(* Affichage des massages *)
let aff_message message =
    Graphics.moveto 2 (Graphics.size_y()-18);
    Graphics.set_color Graphics.black;
    Graphics.draw_string message
;;
