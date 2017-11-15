type zero
type 'a succ = S

type ('a, 'b) clist =
  |  Nil : ('a, zero) clist
  | Cons : ('a * (('a, 'b) clist)) -> (('a, 'b succ) clist)

(*let foldr f z = function
    Nil -> z
  | Cons(x, xs) -> f x (foldr f z xs)*)

let rec to_list : 'b. ('a,'b) clist -> 'a list = function
    Nil -> []
  | Cons (x, xs) -> x :: to_list xs


type 'a clist  =
  |  Nil : zero clist
  | Cons : int * ('a clist) -> 'a succ clist

let rec foldr : type a. (int -> 'b -> 'b) -> 'b -> a clist -> 'b = fun f z -> function
    Nil  -> z
  | Cons(x, xs) -> f x (foldr f z xs)

let rec to_list : type a. a clist -> int list = function
    Nil -> []
  | Cons (x, xs) -> x :: to_list xs

let l1 = Nil
let l2 = Cons( 2, Nil)

let f a b = Cons(a,b)
