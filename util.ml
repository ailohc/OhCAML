let rec map l f =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map tl f)