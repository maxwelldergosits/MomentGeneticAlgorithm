
type color = (float*float*float)
type point = float * float

type gobject =color *point list

val gen_poly : int -> point list

val translate : point list -> point -> point list

val scale : point list -> float -> point list

val centroid : point list -> point

val area : point list -> float

val height_of_poly : point list -> float

val triangles : 'a list -> ('a *'a*'a) list

val random_points : int -> point list 

val valid_poly :
  (float * float) list -> bool

val map2 : ('a -> 'a -> 'b) -> 'a list -> 'b list

