let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec fold f l a =
  match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)

let string_of_strlist first sep last l =
  first ^ fold (
    fun x a -> x ^ (if a = last then "" else sep) ^ a
  ) l last