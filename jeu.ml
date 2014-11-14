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
