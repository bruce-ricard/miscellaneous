let () = print_endline "0"

module F =
  functor (X : sig end) ->
  struct
    let () = print_endline "2"
  end

let () = print_endline "3"

module X =
  struct
    let () = print_endline "1"
  end

let () = print_endline "4"

module M = F(X)

let () = print_endline "5"
