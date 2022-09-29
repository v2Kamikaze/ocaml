let inverse l = 
  let rec _inverse l aux =  
    match l with
    | [] -> aux
    | h::t -> _inverse t (h::aux)
  in
    _inverse l []
;;

List.map print_int (inverse [1;2;3;4;5]);;