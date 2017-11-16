module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

module type TYPE =
  sig
    type t
  end

module Int =
  struct
    type t = int
    let compare = compare
    let to_string = string_of_int
  end
