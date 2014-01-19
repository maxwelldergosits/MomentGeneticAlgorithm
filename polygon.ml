type color = (float*float*float)
type point = float * float

type gobject =color *point list

let gen_poly n =
  let pi = 4.0 *. (atan 1.0) in
  let rec gph i =
    if i < 0 then []
    else
      let theta = (float_of_int i) /. (float_of_int n) in
      let y = sin (2. *. pi *. theta) in
      let x = cos (2. *. pi *. theta) in
      (y,x)::(gph (i-1)) in
  gph (n-1)


let trans_p = (fun (y,x) (dy,dx) -> dy+.y ,dx+.x)

let translate ps (dy,dx) =
  List.map (fun a -> trans_p a (dy,dx)) ps

let map2 (f : ('a -> 'a-> 'b)) (l: 'a list) : ('b list) =
  let l = List.rev l in
  let first,t = match l with h::t -> h,t | _ -> failwith "List.length = 0" in
  let rec map2_helper f l =
    match l with
    |a::b::rest -> f a b :: map2_helper f (b::rest)
    |[a] -> [f a first]
    |[] -> [] in
    map2_helper f l

let scale_point =(fun (y,x) m -> y *. m , x *. m)

let scale ps m =
  List.map (fun a -> scale_point a m) ps

let centroid ps =
  let l = float_of_int (List.length ps) in
  let (y,x) = List.fold_left (fun (yi,xi) (ya,xa) -> (yi +. ya),(xi +. xa)) (0.0,0.0) ps in
  (y/.l,x/.l)

let height_of_poly p=
  match p with
  |(a,_)::t ->
  let min_y = List.fold_left (fun ya (y,_) -> min y ya) a t in
  let max_y = List.fold_left (fun ya (y,_) -> max y ya) a t in
  max_y -. min_y
  |[] -> 0.0

let area ps =
  let cps = map2 (fun (yi,xi) (yj,xj) -> (xi *. yj) -. (xj *. yi)) ps in
  0.5 *. (List.fold_left (+.) 0.0 cps)

let eq (x,y,z) (a,b,c) =
  let first = List.sort (compare) [x;y;z] in
  let second = List.sort (compare) [a;b;c] in
  List.for_all2 (=) first second

let remove_elt f e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when f e x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

let remove_duplicates f l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt f x xs) (x::acc)
  in go l []

let triangles ps =
  let pis = List.mapi (fun i x -> x,i) ps in
  let pllll = List.map (fun (x,i) ->
    List.map (fun (y,j) ->
      List.map (fun (z,k) ->
        if i <> j && i <> k && j <> k then [(x,y,z)] else []) pis) pis) pis in
  let dupes = List.flatten (List.flatten (List.flatten pllll)) in
  remove_duplicates (eq) dupes

let random_points n =

  let rand_point() =
    let y = (float_of_int ((Random.int 2000) - 1000)) /. 1000. in
    let x = (float_of_int ((Random.int 2000) - 1000)) /. 1000. in
    (y,x) in
  let rec loop i =
    if i = n then [(0.2,0.2);(0.2,0.8);(0.8,0.2);(0.8,0.8)]
    else rand_point()::loop (i+1) in
  loop 0


let slope (y,x) (a,b) =
  (y -. a) /. (x -. b)

let line_intersect ((y1,x1),(y2,x2)) ((y3,x3),(y4,x4)) =
  if (y1,x1) = (y3,x3) || (y1,x1) = (y4,x4) || (y2,x2) = (y3,x3) || (y2,x2) = (y4,x4) then false else

  let bx = x2 -. x1 in
  let by = y2 -. y1 in
  let dx = x4 -. x3 in
  let dy = y4 -. y3 in
  let b_dot_d_perp = bx *. dy -. by *. dx in
  if(b_dot_d_perp = 0.0) then false else
  let cx = x3 -. x1 in
  let cy = y3 -. y1 in
  let t = (cx *. dy -. cy *. dx) /. b_dot_d_perp in
  if(t < 0.0 || t > 1.0) then false else
  let u = (cx *. by -. cy *. bx) /. b_dot_d_perp in
  if(u < 0.0 || u > 1.0) then false else
  true


let line_pairs ps =
  let eqq = (fun (x,y) (a,b) ->
    let first = List.sort (compare) [x;y] in
    let second = List.sort (compare) [a;b] in
  List.for_all2 (=) first second) in
  let pis = List.mapi (fun i x -> x,i) ps in
  let pllll = List.map (fun (x,i) ->
    List.map (fun (y,j) ->
        if i <> j then [(x,y)] else []) pis) pis in
  let dupes = (List.flatten (List.flatten pllll)) in
  remove_duplicates (eqq) dupes

let valid_poly ps =
  let mapped = map2 (fun x y -> x,y) ps in
  let pairs = line_pairs mapped in
  List.for_all (fun (x,y) -> not (line_intersect x y)) pairs
