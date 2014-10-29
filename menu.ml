(* configuration plateau *)
type plateau_conf = Plateau.pconf;;

(* configuration affichage *)
type affichage_conf = Ograph.aconf;; 

(* configuration jeu *)
type jeu_conf = Jeu.jconf;;

(* Ouverture d'une fenetre *)
open Tk ;;
let menu confp confa confj =
let top = openTk () in
Wm.title_set top "Othello by Dupin and Escoute" ;

(* Frame titre *)
let frame_title = Frame.create top in
pack ~side:`Top ~padx:8 ~pady:4 [coe(Label.create ~text:"Othello" frame_title)];

(* Frame centrale *)
let frame_content = Frame.create top in

(* Noir *)
let frame_noir = Frame.create ~relief:`Sunken ~borderwidth:2 frame_content in
let v_noir = Textvariable.create () in
Textvariable.set v_noir "?";
let rb_noir txt ret = Radiobutton.create
    ~text:txt ~variable:v_noir ~value:txt
    ~command:(fun () -> confj.Jeu.noir <- ret)
    frame_noir
in
let diff_noir = Scale.create
    ~min:0. ~max:10.
    ~resolution:1. ~tickinterval:5.
    ~label:"Difficulté" ~orient:`Horizontal
    frame_noir
in
pack [coe (Label.create ~text:"Noir" frame_noir)];
pack ~anchor:`W[coe (rb_noir "Humain" Jeu.Humain);
    coe (rb_noir "Aleatoire" Jeu.Aleatoire);
    coe (rb_noir "Naif" Jeu.Naif);
    coe (rb_noir "Min Max" Jeu.MinMax);
    coe (rb_noir "Alpha Beta" Jeu.AlphaBeta);
];
pack [coe diff_noir];

(* Blanc *)
let frame_blanc = Frame.create ~relief:`Sunken ~borderwidth:2 frame_content in
let v_blanc = Textvariable.create ()in
Textvariable.set v_blanc "?";
let rb_blanc txt ret = Radiobutton.create
    ~text:txt ~variable:v_blanc ~value:txt
    ~command:(fun () -> confj.Jeu.blanc <- ret)
    frame_blanc
in
let diff_blanc = Scale.create
    ~min:0. ~max:10.
    ~resolution:1. ~tickinterval:5.
    ~label:"Difficulté" ~orient:`Horizontal
    frame_blanc
in
pack [coe (Label.create ~text:"Blanc" frame_blanc)];
pack ~anchor:`W[coe (rb_blanc "Humain" Jeu.Humain);
    coe (rb_blanc "Aleatoire" Jeu.Aleatoire);
    coe (rb_blanc "Naif" Jeu.Naif);
    coe (rb_blanc "Min Max" Jeu.MinMax);
    coe (rb_blanc "Alpha Beta" Jeu.AlphaBeta);
];
pack [coe diff_blanc];

(* Taille *)
(*
let frame_taille = Frame.create frame_content;
let w = Entry.create ~width:4 ~relief:`Sunken frame_taille;
let h = Entry.create ~width:4 ~relief:`Sunken frame_taille;
pack ~expand:true ~side:`Top [coe (Label.create ~text:"Taille du plateau : "
frame_taille)];
pack ~side:`Bottom [coe w;
    coe (Label.create ~text:" x " frame_taille);
    coe h];

pack ~side:`Bottom [coe frame_taille];
*)
pack ~side:`Left ~anchor:`N [coe frame_noir];
pack ~side:`Right ~anchor:`N [coe frame_blanc];

let btn_jouer = Button.create
    ~text:"Jouer"
    ~command:(fun () ->
        closeTk()
    )
    top
in

pack [coe frame_title];
pack [coe frame_content];
pack ~side:`Bottom ~anchor:`E [coe btn_jouer];
let _ = Printexc.print mainLoop () in ()
;;
