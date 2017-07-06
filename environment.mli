type 'a t

exception Not_bound

val empty : 'a t
val extend : Syntax.id -> 'a -> 'a t -> 'a t
val append : 'a t -> (Syntax.id * 'a) list -> 'a t
val lookup : Syntax.id -> 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val map_full : ((Syntax.id * 'a) -> 'b) -> 'a t -> 'b t
val get_list : 'a t -> (Syntax.id * 'a) list
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val length : 'a t -> int
val update : 'a t -> Syntax.id -> 'a -> 'a t