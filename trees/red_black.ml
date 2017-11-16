open Types

module RedBlackTree (Key : ORDERED_TYPE) (Value : TYPE) =
  struct
    type color = Black | Red

    type t = Node of node | Nil
     and node =
       {
         key : Key.t;
         value : Value.t;
         color : color;
         left : t;
         right : t
       }

    let empty () = Nil

    let find x =
      let rec aux = function
        | Nil -> None
        | Node {key; value; color; left; right} ->
           match Key.compare x key with
           | 0 -> Some value
           | n -> if n < 0 then
                    aux left
                  else
                    aux right
      in
      aux

    let insert key value =
      let rec aux = function
        | Nil -> Node {key; value; color = Red; left = Nil; right = Nil}
        | Node {key = k; value = v; color; left; right} ->
           if Key.compare key k <= 0 then
             Node {key = k; value = v; color; left = aux left; right}
           else
             Node {key = k; value = v; color; left; right = aux right}
      in aux

  end


(**** TEST ****)

module IntTree = RedBlackTree(Int)(Int)
open IntTree
let test () =
  let tree = Nil in
  let tree = insert 1 0 tree in

  tree
