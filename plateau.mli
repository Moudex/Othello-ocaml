(* Definition d'une case *)
type case = Blanc | Noir | Vide

(* Configuration du plateau *)
type pconf = {
    nbcols  : int;
    nbligs  : int
}

(*definition du plateau *)
type plateau = case array array

(* Initialisation du plateau *)
val init_plateau : pconf -> case array array = <fun>

(* Couleur de l'adversaire *)
val adversaire : case -> case = <fun>

(* Fin de la partie *)
val fin : case array array -> bool = <fun>

(* Jouer *)
val joue : case array array -> case -> int -> int -> case = <fun>
