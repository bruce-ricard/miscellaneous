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
    type t =
      | Nil
      | BlackNode of black_node
      | RedNode of red_node

     and black_node =
       {
         black_key : Key.t;
         black_value : Value.t;
         black_left : t;
         black_right : t
       }

     and red_node =
       {
         red_key : Key.t;
         red_value : Value.t;

         red_left : black_node option;
         red_right : black_node option;
       }

    let empty () = Nil

    let find x =
      let rec aux = function
        | Nil -> None
        | BlackNode node -> find_in_black_node node
        | RedNode node -> find_in_red_node node
        and find_in_black_node {black_key; black_value; black_left; black_right} =
          match Key.compare x black_key with
          | 0 -> Some black_value
          | n when n < 0 -> aux black_left
          | n when n > 0 -> aux black_right
          | _ -> assert false

        and find_in_red_node {red_key; red_value; red_left; red_right} =
          match Key.compare x red_key, red_left, red_right with
          | 0,_,_ -> Some red_value
          | n,None,_ when n < 0 -> None
          | n,(Some black_node), _ when n < 0 -> find_in_black_node black_node
          | n,_,None when n > 0 -> None
          | n,_,(Some black_node) when n > 0 -> find_in_black_node black_node
          | _ -> assert false


      in
      aux

    let rec insert key value = function
        Nil -> BlackNode {
                   black_key = key;
                   black_value = value;
                   black_left = Nil;
                   black_right = Nil
                 }
      | BlackNode {black_key; black_value; black_left; black_right} ->
         if Key.compare key black_key <= 0 then
           BlackNode {
               black_key;
               black_value;
               black_left = insert key value black_left;
               black_right
             }
         else
           BlackNode {
               black_key;
               black_value;
               black_left;
               black_right = insert key value black_right
             }
      | RedNode {red_key; red_value; red_left; red_right} ->
         Nil

    let to_string tree =
      let rec combine_children pad l1 l2 =
        match l1,l2 with
        | x :: xs, (y :: ys) -> (x ^ (String.make pad '.') ^ y) :: combine_children pad xs ys
        | l,[] -> l
        | [],l -> List.map (fun s -> (String.make (pad + 1) ':') ^ s) l
      in
      let rec aux pad = function
          Nil -> [(String.make pad ' ' ^ "()")], pad + 2
        | BlackNode {black_key; black_value; black_left; black_right} ->
           let left, left_pad = aux pad black_left
           and right, right_pad = aux pad black_right in
           let pad = max left_pad right_pad in
           (String.make pad '_' ^ Key.to_string black_key) :: (combine_children pad left right), (pad + 2)
        | RedNode {red_key; red_value; red_left; red_right} ->
           [""],0
      in
      List.iter print_endline (fst (aux 0 tree))

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
