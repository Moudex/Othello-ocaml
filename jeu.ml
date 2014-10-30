(* Types de joueurs *)
type player = Humain | Aleatoire | Naif | MinMax | AlphaBeta

(* configuration du jeu *)
type jconf = {
    mutable noir    : player;
    mutable blanc   : player
}

