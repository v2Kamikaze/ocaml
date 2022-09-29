let rec reduce f l = 
  match l with
  | [] -> []
  | h::t  -> if f h then h :: reduce f t else reduce f t;;

let rec print_list l = 
  match l with
  | [] -> Printf.printf "\n"
  | h::t -> Printf.printf "%d " h; print_list t;;
  

let remove_from_list v l = 
  reduce (fun x -> v != x) l;;


print_list (remove_from_list 1 [1; 2; 3; 1; 2; 1]);;