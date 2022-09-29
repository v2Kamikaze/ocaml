let l = [ (1, "Um"); (2, "Dois"); (3, "Três") ];;

let rec map l f = 
  match l with
  | [] -> []
  | h :: t ->  f h :: map t f;;


let separete_into_lists l =  (map l (fun (a,_) -> a), map l (fun (_,b) -> b));;
let res = separete_into_lists l;;

print_string (if res = ( [1; 2; 3], ["Um"; "Dois"; "Três"] ) then "OK\n" else "Errado\n");;
