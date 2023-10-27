let ( >> ) f g x = g (f x)

let invert_compare a b = compare b a

let tee f a =
  f a;
  a  
