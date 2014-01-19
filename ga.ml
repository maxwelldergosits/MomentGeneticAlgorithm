open Polygon

type 'a ga_pop = (('a list) * ('a -> float) * ('a -> 'a) * ('a -> 'a -> 'a))


let rec permutations l =
   let n = List.length l in
   if n = 1 then [l] else
   let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t in
   let rec aux k =
      let e = List.nth l k in
      let subperms = permutations (sub e l) in
      let t = List.rev_map (fun a -> e::a) subperms in
      if k < n-1 then List.rev_append t (aux (k+1)) else t in
   aux 0

let rpoints() =
  let rec h () =
    let ps = random_points 2 in
    let rec iter f l =
      match l with | e::t ->
      if f e then Some(e) else iter f t
      | [] -> None in
    match iter (valid_poly) (permutations ps) with Some(v) -> v | None -> h()
  in h()

let rec initial_population n acc f =
  if n = 0 then acc else
  initial_population (pred n) ((f())::acc) f

let sort_best f l =
  let mapped = List.map (fun a -> a,f a) l in
  List.map (fun (x,_) -> x) (List.sort (fun (_,x) (_,y) -> compare y x) mapped)

let best f = function
  h :: t ->
  List.fold_left (fun a b -> if f a < f b then b else a) h t
  | [] -> failwith ":("

let create (gen:unit-> 'a) (fit: 'a -> float) (mut : 'a -> 'a) (crs: 'a -> 'a -> 'a) (n:int) : 'a ga_pop =
  let rec loop i acc = if i = 0 then acc else loop (pred i) (gen()::acc) in
  (loop n [],fit,mut,crs)

let get_pop (ps,a,b,c) = ps

let run ((ps, f, m, crs): 'a ga_pop) n =
  let pop_size = List.length ps in

  let evolvePopulation pop =

    let create_tournament n =
      let rec loop i = if i < 0 then [] else (List.nth ps (Random.int pop_size))::(loop (pred i)) in
      best f (loop 5) in

      let rec loop i =
        match i with | 0 -> [] | _ ->
        let indiv1 = create_tournament pop in
        let indiv2 = create_tournament pop in
        let newIndiv = crs indiv1 indiv2 in
        newIndiv :: loop (pred i) in

      let crossed = loop pop_size in

      List.map (m) crossed in

  let rec rh i pop =
    match i with 0 -> pop | _ -> rh (pred i) (evolvePopulation pop) in
  ((rh pop_size ps), f, m, crs)

