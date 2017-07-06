type 'a t = (Syntax.id * 'a) list

exception Not_bound

let empty = []
let extend x v env = (x,v)::env
let append env ls = env @ ls

let rec lookup x env = 
  try List.assoc x env with Not_found -> raise Not_bound

let rec map f = function
    [] -> []
  | (id, v)::rest -> (id, f v) :: map f rest

let rec map_by_id f = function
    [] -> []
  | (id, v)::rest -> 
  (id, f (id, v))::(map_by_id f rest)

let rec get_list = function
    [] -> []
  | (id, v)::rest -> (id, v) :: get_list rest

let rec fold_right f env a = 
  match env with
      [] -> a
    | (_, v)::rest -> f v (fold_right f rest a)

let length env = List.length env

let update env id value = map_by_id (fun (id', value') -> if id' = id then value else value') env