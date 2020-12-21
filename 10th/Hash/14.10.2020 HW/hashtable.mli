type ('a,'b) t

val create: int -> ('a -> int -> int) -> ('a,'b) t 
val add: ('a,'b) t -> 'a -> 'b -> 'b option
val mem: ('a,'b) t -> 'a -> bool
val find: ('a,'b) t -> 'a -> 'b option
val delete: ('a,'b) t -> 'a -> 'b option

val iter: ('a,'b) t -> (('a*'b) -> unit) -> unit
val fold: ('a,'b) t -> ('c -> ('a*'b) -> 'c) -> 'c -> 'c