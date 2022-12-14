

let rec mapear lista f = 
  match lista with
  | [] -> []
  | h :: t -> f h :: mapear t f;;


let l1 = [ 1; 2; 3; 4 ];;

let l2 = mapear l1 (fun i -> i*i) ;;

let l3 = mapear l2 string_of_int ;;

let l4 = mapear l3 (fun s -> s^s) ;;

let l5 = mapear l4 String.length ;;

print_endline
  begin
  if l2 = [1; 4; 9; 16] &&
      l3 = ["1"; "4"; "9"; "16"] &&
      l4 = ["11"; "44"; "99"; "1616"] &&
      l5 = [2; 2; 2; 4]
  then "Conforme esperado."
  else "Algo errado!"
end ;;