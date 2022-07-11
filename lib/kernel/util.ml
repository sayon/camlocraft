let foreach arr f =
  for i = 0 to Array.length arr - 1 do f arr.(i) done

let print_line s = print_string s; print_string "\n"

let rec do_all f lst =
  match lst with
  | [] -> ()
  | x :: xs -> f x; do_all f xs
