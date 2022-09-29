
let rec reduce f l = 
  match l with
  | [] -> []
  | h::t  -> if f h then h :: reduce f t else reduce f t;;


let rec print_list l = 
  match l with
  | [] -> Printf.printf "\n"
  | h::t -> Printf.printf "%d " h; print_list t;;
  

let res = reduce (fun n -> n mod 2 = 0) [-2;-1;0;1;2;3;4;5;6] ;;
print_list res;;
