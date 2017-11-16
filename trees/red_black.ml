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

    let color = function
      | Node {color} -> color
      | Nil -> Black

    let get_node = function
        Nil -> failwith "This node is empty"
      | Node node -> node

    let insert key value =
      let rec aux = function
        | Nil -> assert false
        | Node ({key = k; value = v; color; left = left1; right = right1} as grandpa) ->
           let current = Node {key; value; color = Red; left = Nil; right = Nil} in
           if Key.compare key k <= 0 then
             begin
               match left1 with
               | Nil -> assert false
               | Node ({key = k; value = v; color; left = left2; right = right2} as dad)->
                  if Key.compare key k <= 0 then
                    match left2 with
                    | Nil -> Node {grandpa with left = Node {dad with left = current}}
                    | _ -> aux (Node dad)
                  else
                    match right2 with
                    | Nil -> Node {grandpa with left = Node {dad with right = current}}
                    | _ -> aux (Node dad)
             end
           else
             match right1 with
               | Nil -> assert false
               | Node ({key = k; value = v; color; left = left2; right = right2} as dad)->
                  if Key.compare key k <= 0 then
                    match left2 with
                    | Nil -> Node {grandpa with right = Node {dad with left = current}}
                    | _ -> aux (Node dad)
                  else
                    match right2 with
                    | Nil -> Node {grandpa with right = Node {dad with right = current}}
                    | _ -> aux (Node dad)


      in
      function
      | Nil -> Node {key; value; color = Red; left = Nil; right = Nil}
      | Node {key = k; value = v; color; left; right} as gp ->
         if Key.compare key k <= 0 then
           match left with
           | Nil ->
              let current = Node {key; value; color = Red; left = Nil; right = Nil} in
              Node {key = k; value = v; color; left = current; right}
           | _ -> aux gp
         else
           match right with
           | Nil ->
              let current = Node {key; value; color = Red; left = Nil; right = Nil} in
              Node {key = k; value = v; color; left; right = current}
           | _ -> aux gp

  end

(**** TEST ****)

module IntTree = RedBlackTree(Int)(Int)
open IntTree
let test () =
  let tree = Nil in
  let tree = insert 1 0 tree in

  tree
