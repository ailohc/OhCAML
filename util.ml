let rec map l f =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map tl f)

let rec fold l f r=
  match l with
  | [] -> r
  | hd::tl -> f hd (fold tl f r)