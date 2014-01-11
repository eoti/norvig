all:
	ocamlopt norvig.ml -o test
	
edit:
	nedit norvig.ml input.scm input.txt &	
	
tst:
	./test input.scm output.s
	as output.s -o output.o
	
