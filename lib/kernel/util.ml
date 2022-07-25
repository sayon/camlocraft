let print_line s = print_string s; print_string "\n"

let uncurry f = fun (x,y) -> f x y
