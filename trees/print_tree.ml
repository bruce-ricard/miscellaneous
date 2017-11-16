module type TREE =
  sig
    type tree
    type value
    type key
    type t = Nil | Node of node
     and node = {key : key; value : value; left : t; right : t}

    val convert : tree -> t
    val key_to_string : key -> string
    val value_to_string : value -> string
  end

module Print (Tree : TREE) =
  struct
    open Tree

    let to_string tree =
      let rec combine_children pad l1 l2 =
        match l1,l2 with
        | x :: xs, (y :: ys) -> (x ^ (String.make pad '.') ^ y) :: combine_children pad xs ys
        | l,[] -> l
        | [],l -> List.map (fun s -> (String.make (pad + 1) ':') ^ s) l
      in
      let rec aux pad = function
          Nil -> [(String.make pad ' ' ^ "()")], pad + 2
        | Node {key; value; left; right} ->
           let left, left_pad = aux pad left
           and right, right_pad = aux pad right in
           let pad = max left_pad right_pad in
           (String.make pad '_' ^ key_to_string key) :: (combine_children pad left right), (pad + 2)
      in
      List.iter print_endline (fst (aux 0 tree))

  end
