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

(* Score naif, nb pions oté du nb de pions adverse *)
let score_naif plateau couleur =
    (Plateau.nombre plateau couleur) - (Plateau.nombre plateau
    (Plateau.adversaire couleur))
;;
