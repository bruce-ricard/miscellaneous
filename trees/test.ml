open Red_black


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
