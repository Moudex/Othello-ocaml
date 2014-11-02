(*
 * Fichier relatif au jeux en général :
 * # types des joueurs
 * # Calculs de scores
 * # Fonctions d'évaluation
 *
 *)

(* Types de joueurs *)
type player = Humain | Aleatoire | Naif | MinMax | AlphaBeta

(* configuration du jeu *)
type jconf = {
    mutable noir    : player;
    mutable blanc   : player
}

type plateau = Plateau.plateau

(* Score naif, nb pions oté du nb de pions adverse *)
let score_naif plateau couleur =
    (Plateau.nombre plateau couleur) - (Plateau.nombre plateau
    (Plateau.adversaire couleur))
;;
