type imprimivel = Imp: ('a * ('a -> unit)) -> imprimivel ;;

let l = [ Imp (3, print_int); Imp (3.14, print_float); Imp ("Oi", print_string) ] ;;

let imprimir list = 
  let rec aux l =
    match l with
    | [] -> print_endline " []"
    | Imp(v, f)::t -> begin
      f v;
      print_string " :: ";
      aux t
    end
  in aux list
;;

imprimir l;;