class point (x : int) =
object(self : 'a)
  val mutable x = x
  method get_x = x
  method move_to y = x <- (x + y)
  method equals (y : 'a) = self#get_x == y#get_x
end

class colored_point x (color : string) =
object(self : 'a)
  inherit point x as super
  val mutable color = color
  method get_color = color
  method equals (y : 'a) = super#equals y && self#get_color = y#get_color
end

let pc1 = new colored_point 3 "red";;

let p1 = (pc1 :> point);;
let p2 = new point 5;;

p1#equals p2x
