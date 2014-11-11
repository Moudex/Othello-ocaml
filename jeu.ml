(*
 * Fichier relatif au jeux en général :
 * # types des joueurs
 * # Calculs de scores
 * # Fonctions d'évaluation
 *
 *)

(* Types de joueurs *)
type player = Humain | Aleatoire | MinMax | AlphaBeta
type eval = Materiel | Mobilitee | Force | Hybride1 | Hybride2 | Evolutif

(* configuration du jeu *)
type jconf = {
    mutable noir    : player;
    mutable eval_noir : eval;
    mutable lvl_noir : int;
    mutable blanc   : player;
    mutable eval_blanc : eval;
    mutable lvl_blanc : int;
}

type plateau = Plateau.plateau
type t_force = int array array

type t_forces = {
    mutable f_noir : t_force;
    mutable f_blanc : t_force
}

let f = {
    f_noir = Array.init 8 (fun y ->Array.init 8 (fun x -> 0));
    f_blanc = Array.init 8 (fun y ->Array.init 8 (fun x -> 0))
}

(* Score materiel, nb pions oté du nb de pions adverse *)
let score_materiel p c =
    (Plateau.nombre p c) - (Plateau.nombre p
    (Plateau.adversaire c))
;;

(* Score de mobilitee, nb de coups jouables oté de ceux adverse *)
let score_mobilitee p c =
    (Plateau.nbcoups_possibles p c) - (Plateau.nbcoups_possibles p
    (Plateau.adversaire c))
;;

let const_force conf =
    Array.init conf.Plateau.nbcols
    (fun y -> Array.init conf.Plateau.nbligs
        (fun x -> 0)
    )
;;

let init_force conf =
    let force = const_force conf in
    (* Carre central *)
    force.((Array.length force)/2-1).((Array.length force)/2-1) <- 16;
    force.((Array.length force)/2).((Array.length force)/2) <- 16;
    force.((Array.length force)/2).((Array.length force)/2-1) <- 16;
    force.((Array.length force)/2-1).((Array.length force)/2) <- 16;

    (* Bords du carre central *)
    force.((Array.length force)/2-2).((Array.length force)/2-1) <- 2;
    force.((Array.length force)/2-2).((Array.length force)/2) <- 2;
    force.((Array.length force)/2+1).((Array.length force)/2-1) <- 2;
    force.((Array.length force)/2+1).((Array.length force)/2) <- 2;
    force.((Array.length force)/2-1).((Array.length force)/2-2) <- 2;
    force.((Array.length force)/2).((Array.length force)/2-2) <- 2;
    force.((Array.length force)/2-1).((Array.length force)/2+1) <- 2;
    force.((Array.length force)/2).((Array.length force)/2+1) <- 2;
    force.((Array.length force)/2-2).((Array.length force)/2-2) <- 1;
    force.((Array.length force)/2-2).((Array.length force)/2+1) <- 1;
    force.((Array.length force)/2+1).((Array.length force)/2-2) <- 1;
    force.((Array.length force)/2+1).((Array.length force)/2+1) <- 1;

    (* coins *)
    force.((Array.length force)-1).((Array.length force)-1) <- 500;
    force.((Array.length force)-1).(0) <- 500;
    force.(0).((Array.length force)-1) <- 500;
    force.(0).(0) <- 500;

   (* Voisins des coins *)
    force.((Array.length force)-2).((Array.length force)-1) <- -150;
    force.((Array.length force)-3).((Array.length force)-1) <- 30;
    force.((Array.length force)-1).((Array.length force)-2) <- -150;
    force.((Array.length force)-1).((Array.length force)-3) <- 30;
    force.((Array.length force)-2).((Array.length force)-2) <- -250;
    force.((Array.length force)-2).(0) <- -150;
    force.((Array.length force)-3).(0) <- 30;
    force.((Array.length force)-1).(1) <- -150;
    force.((Array.length force)-1).(2) <- 30;
    force.((Array.length force)-2).(1) <- -250;
    force.(0).((Array.length force)-2) <- -150;
    force.(0).((Array.length force)-3) <- 30;
    force.(1).((Array.length force)-1) <- -150;
    force.(2).((Array.length force)-1) <- 30;
    force.(1).((Array.length force)-2) <- -250;
    force.(0).(1) <- -150;
    force.(0).(2) <- 30;
    force.(1).(0) <- -150;
    force.(2).(0) <- 30;
    force.(1).(1) <- -250;

    (* Le reste des bords *)
    for i=3 to ((Array.length force)-4) do
        for j=3 to ((Array.length force.(0))-4) do
            force.(0).(i) <- 10;
            force.((Array.length force)-1).(i) <- 10;
            force.(j).(0) <- 10;
            force.(j).((Array.length force)-1) <- 10;
        done;
    done;
    force
;;

let score_force p c =
    let score = ref 0 in
    for i=0 to ((Array.length p)-1) do
        for j=0 to ((Array.length p.(0))-1) do
            match p.(j).(i) with
            | c -> score := !score + (match c with | Plateau.Blanc -> f.f_blanc.(j).(i) | Plateau.Noir -> f.f_noir.(j).(i) | _ -> 0)
            | _ -> ()
        done;
    done;
    !score
;;

let init conf =
    f.f_noir <- init_force conf;
    f.f_blanc <- init_force conf;
;;
