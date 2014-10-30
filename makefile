all: othello

othello: plateau.cmo ograph.cmo jeu.cmo ia.cmo menu.cmo othello.cmo
	ocamlc -o othello -I +labltk labltk.cma graphics.cma $^

othello.cmo: othello.ml plateau.cmo ograph.cmo jeu.cmo ia.cmo menu.cmo
	ocamlc -c $<

menu.cmo: menu.ml jeu.cmo plateau.cmo ograph.cmo
	ocamlc -I +labltk labltk.cma -c $<

%.cmo: %.ml
	ocamlc -c $<

.PHONY: clean mrproper

clean:
	rm -rf *.cmo

mrproper: clean
	rm -rf othello
