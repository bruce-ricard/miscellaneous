
type dictionary = node list
 and node = Node of char * bool * dictionary

let string_to_list s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


let rec dictionary_contains_character dictionary char =
  match dictionary with
    [] -> false
  | Node(c,_,_) :: nodes -> c = char ||
                              dictionary_contains_character nodes char


let rec add_char_list_to_node word (Node(char, b, l) as node)=
  match word with
  |  [] -> node
  | c :: cs ->
     if c = char then
       match cs with
       | [] -> Node(char, true, l)
       | cs -> Node(char,b, (add_char_list_to_dictionary cs l))
     else
       node

and add_char_list_to_dictionary word dictionary =
  match word with
  | [] -> dictionary
  | c :: cs ->
     let dictionary =
       if dictionary_contains_character dictionary c then
         dictionary
       else
         (Node(c,false,[]) :: dictionary)
     in
     List.map (add_char_list_to_node word) dictionary

let rec get_node char = function
  | [] -> None
  | Node(c,_,_) as node :: nodes -> if c = char then
                                      Some node
                                    else
                                      get_node char nodes

let add_word word dictionary =
  let char_list = string_to_list word in
  add_char_list_to_dictionary char_list dictionary

let is_word_in_dictionary word dictionary =
  let rec aux char_list dictionary =
    match char_list with
    | [] -> false
    | c :: cs ->
       match get_node c dictionary with
       | None -> false
       | Some(Node(_,b, l)) ->
          begin
            match cs with
            | [] -> b
            | cs -> aux cs l
          end
  in
  let char_list = string_to_list word in
  aux char_list dictionary

let create_dictionary word_list =
  List.fold_left (fun dic word -> add_word word dic) [] word_list

let rec to_n n =
  let n = n - 1 in
  if n < 0 then
    []
  else
    n :: (to_n n)

let rec all_coordinates_size_n n =
  let xs = to_n n in
  List.fold_left (@) [] @@
    List.map (function x -> (List.map (function y -> (x,y)) xs)) xs

let find_words_in_grid grid dictionary =

  let rec aux i j forbidden dictionary =
    let should_go =
      try
        ignore grid.(i).(j);
        not (List.mem (i,j) forbidden)
      with
        _ -> false
    in

    if should_go then
      let current_char = grid.(i).(j) in
      let current_word = String.make 1 current_char in
      match get_node current_char dictionary with
      | None -> []
      | Some(Node(_,b,l)) ->
         let new_forbidden = (i,j) :: forbidden in
         let reccall =
           (aux (i + 1) j new_forbidden l)
           @ (aux (i) (j + 1) new_forbidden l)
           @ (aux (i - 1) (j) new_forbidden l)
           @ (aux (i) (j - 1) new_forbidden l)
         in
         let found_words =
           if b then
             "" :: reccall
           else
             reccall
         in
         List.map ((^) current_word) found_words
    else
      []
  in
  let start_points = all_coordinates_size_n (Array.length grid) in

  List.fold_left (@) [] @@
    List.map (function (i,j) -> aux i j [] dictionary) start_points

let make_grid words =
  let rec aux = function
      [] -> []
    | word :: words ->  string_to_list word :: (aux words)
  in
  let list_grid = aux words in

  let grid = Array.of_list (List.map Array.of_list list_grid) in
  let length = Array.length grid in
  Array.iter (fun a -> if Array.length a != length then
                         failwith "all words must have the same number of characters as the number of words in the list") grid;
  grid

let main () =
  let grid = make_grid ["abx"; "xcx"; "edx"]
  and dic = create_dictionary
              ["b"; "ab"; "abcde"; "aba"; "aa"; "bbb"; "acb"]
  in
  let matches = find_words_in_grid grid dic in
  print_endline "The matching words are:";
  List.iter print_endline matches


let () = main ()
