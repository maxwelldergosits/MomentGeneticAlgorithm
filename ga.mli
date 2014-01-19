open Polygon

type 'a ga_pop

val rpoints : unit -> point list

val initial_population : int -> 'a list -> (unit -> 'a) -> 'a list

val sort_best : ('a -> 'b) -> 'a list -> 'a list

val best : ('a -> 'b) -> 'a list -> 'a

val create : (unit -> 'a) -> ('a -> float) -> ('a -> 'a) -> ('a -> 'a -> 'a) -> int -> 'a ga_pop

val run : ('a ga_pop -> int -> 'a ga_pop)

val get_pop : ('a ga_pop -> 'a list)
