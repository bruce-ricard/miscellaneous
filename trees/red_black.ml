module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end

module type TYPE =
  sig
    type t
    val to_string : t -> string
  end


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
           if Key.compare key k <=0 then
             Node {key = k; value = v; color; left = aux left; right}
           else
             Node {key = k; value = v; color; left; right = aux right}
      in aux

  end

(********** TEST **********)

module Int = struct type t = int let compare = compare let to_string = string_of_int end

module IntRBTree = RedBlackTree(Int)(Int)

let test () =
  let t = IntRBTree.empty () in
  IntRBTree.to_string t;
  print_endline "====================";
  let t = IntRBTree.insert 1 1 t in
  IntRBTree.to_string t;
  print_endline "====================";
  let t = IntRBTree.insert 2 2 t in
  IntRBTree.to_string t;
  print_endline "====================";
  let t = IntRBTree.insert 5 5 t in
  IntRBTree.to_string t;
  print_endline "====================";
  let t = IntRBTree.insert 3 3 t in
  IntRBTree.to_string t;
  print_endline "====================";
  let t = IntRBTree.insert 7 7 t in
  IntRBTree.to_string t;
  print_endline "====================";
  let t = IntRBTree.insert 0 0 t in
  IntRBTree.to_string t;
  print_endline "====================";
  let t = IntRBTree.insert 4 0 t in
  IntRBTree.to_string t;
  print_endline "===================="


let () = test ()
