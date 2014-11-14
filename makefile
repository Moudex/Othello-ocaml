all: othello

othello: plateau.cmo ograph.cmo ia.cmo menu.cmo othello.cmo jeu.cmo
	ocamlc -o othello -I +labltk labltk.cma graphics.cma $^

othello.cmo: othello.ml plateau.cmo ograph.cmo ia.cmo jeu.cmo
	ocamlc -c $<

menu.cmo: menu.ml plateau.cmo ograph.cmo jeu.cmo
	ocamlc -I +labltk labltk.cma -c $<

%.cmo: %.ml
	ocamlc -c $<

.PHONY: clean mrproper

clean:
	rm -rf *.cmo

mrproper: clean
	rm -rf othello
