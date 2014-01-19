open Polygon
open Ga


let () = Random.init (int_of_float (Unix.time()))



type force = Uniform | Center | End






    let sq = fun x -> x *. x

    let x_moment = map2 (fun (yi,xi) (yj,xj) -> (xj -. xi) *. (yj +. yi) *. ((sq yj) +. (sq yi)))
    let y_moment = map2 (fun (yi,xi) (yj,xj) -> (yj -. yi) *. (xj +. xi) *. ((sq xj) +. (sq xi)))
    let xy_moment = map2 (fun (yi,xi) (yj,xj) -> (xi -. xj) *. ((3. *. xj *. (sq yj)) +. (xi *. (sq yj)) +. (2. *. xi *. yi *. yj) +. (2. *. xj *. yi *. yj) +. (xj *. (sq yi)) +. (3. *. (sq xi) *. (sq yi))))


    let iy = fun a -> (List.fold_left ( +. ) 0.0 (y_moment a)) /. 12.
    let ix = fun a -> (List.fold_left ( +. ) 0.0 (x_moment a)) /. 12.
    let ixy = fun a -> (List.fold_left ( +. ) 0.0 (xy_moment a)) /. 12.


let fitness a = ((iy a)) /. (area a)

let population = ref None

let time_step (time: float) =

  let force = time *. 500. in

  let cross_section =

    let non_centered = gen_poly 100 in
(*    let non_centered = [(y,neg x); (y,x) ; (neg y, x); neg y, neg x] in*)

    let yc,xc = centroid non_centered in

    translate non_centered (0.0 -. yc , 0.0 -. xc) in


  let i = iy cross_section in

  let e = 200. in

  let length = 1. in

  let height = 0.1 in

  let deflection dx =
    let abs = fun x -> if x > 0.0 then x else 0.0 -. x in
    let x = (length /. 2.) -. (abs dx) in
    let num = force *. x in
    let den = 48.0 *. e *. i in
    let c = (3. *. (sq length)) -. (4. *. (sq x)) in
    let def = (num /. den) *. c in
    def in

  let res = 1000 in

  let rec make_points i =
    if i = res then [] else
    let y = height /. 2. in
    let x = (((float_of_int i) /. (float_of_int res)) *. length) -. (length /. 2.) in
    (y,x)::(make_points (i+ 1)) in


  let deflect_point (y,x) =
    let def = deflection x in
    ((y +. def,x),((def -. y),x)) in

  let dp = let t,b = List.split (List.map (deflect_point) (make_points 0)) in
    List.rev_append t b in

(*  ((0.3,0.5,0.6),dp)::*)
(*  (((0.4,0.3,0.1),(translate (scale cross_section 0.2) (0.7,0.7)))::*)

  let cross_over a b =
    List.map2 (fun (y1,x1) (y2,x2) -> (y1 +. y2 /. 2.),(x1 +. x2 /. 2.)) a b in

  let mutate a =
    List.map (
      fun (y,x) ->
        (if (Random.float 1.0) <= 0.02 then y +. (Random.float 2.0) else y),
        (if (Random.float 1.0) <= 0.02 then x +. (Random.float 2.0) else x)
    ) a in


  let pop = match !population with None -> let p = (create (rpoints) (fitness) (mutate) (cross_over) 250) in (population:=Some(p); p)  | Some(p) -> p in
  let best_poly = best fitness (get_pop (run pop 200)) in
  let (ybc,xbc) = centroid best_poly in
  let centered = translate best_poly (-.ybc,-.xbc) in


  [((0.4,0.6,0.8),centered)]
