default: opt

FLAGS = -I +lablGL

OLIBS = unix.cmxa lablglut.cmxa lablgl.cmxa
LIBS = unix.cma lablglut.cma lablgl.cma


clean:
	rm *.cmi *.cmx *.o *.cmo main

all:
	ocamlc $(FLAGS) $(LIBS) polygon.mli polygon.ml ga.mli ga.ml model.mli model.ml graphics_phys.mli graphics_phys.ml -o main

opt:
	ocamlopt $(FLAGS) $(OLIBS) polygon.mli polygon.ml ga.mli ga.ml model.mli model.ml graphics_phys.mli graphics_phys.ml -o main

