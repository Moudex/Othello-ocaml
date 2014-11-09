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
    mutable lvl_blanc : int
}

type plateau = Plateau.plateau

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
