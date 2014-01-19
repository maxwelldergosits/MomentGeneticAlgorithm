
all:
	ocamlc -g -I +lablGL unix.cma lablglut.cma lablgl.cma polygon.mli polygon.ml ga.mli ga.ml model.mli model.ml graphics_phys.mli graphics_phys.ml -o main

opt:
	ocamlopt -g -I +lablGL unix.cmxa lablglut.cmxa lablgl.cmxa polygon.mli polygon.ml ga.mli ga.ml model.mli model.ml graphics_phys.mli graphics_phys.ml -o main

