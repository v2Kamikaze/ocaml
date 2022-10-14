type 'a data_pointer = Null | Value of 'a;;

type 'a pointer = {mutable v : 'a ref data_pointer};;

exception NullPointer of string

let create_null () = {v = Null};;

let ( !& ) v = {v = Value (ref v)};;

let ( ?. ) p = 
  match p.v with
  | Null -> true
  | Value _ -> false
;;

let equals p1 p2 = 
  match p1.v, p2.v with
  | Value v1, Value v2 -> v1 == v2
  | _, _ -> false
;;

let ( =& ) p v = p.v <- (Value (ref v));;

let ( !* ) p = 
  match p.v with
  | Value r -> (!r)
  | Null -> raise (NullPointer "Exceção de Ponteiro Nulo!")
;;

let ( =~ ) p1 p2 = p1.v <- p2.v;;

let main () =
  let cont = ref 0 in
  let imp p nome = Printf.printf "   %s -> %s" nome (if ?. p then "nulo" else (Printf.sprintf "%4d" (!* p))) in
  let p = create_null () in
  let q = !& 0 in
  let situacao () =
    cont := !cont + 1;
    Printf.printf "%2d:   " !cont;
    print_string (
      if   equals p q
      then "p igual a q"
      else "p dif. de q"
    );

    imp p "p";
    imp q "q";

    print_char '\n'
  in
  (*  1 *)  situacao ();
  (*  2 *)  p =& 0;                  situacao ();
  (*  3 *)  p =~ q;                  situacao ();
  (*  4 *)  q =& int_of_string "1";  situacao ();
  (*  5 *)  p =& int_of_string "1";  situacao ();
  (*  6 *)  q =~ p;                  situacao ();
  (*  7 *)  q =& 1;                  situacao ();
  (*  8 *)  p =& 1;                  situacao ();
  (*  9 *)  p =~ create_null ();     situacao ();
  (* 10 *)  q =~ create_null ();     situacao ();
;;

main ();;


(*
   
1:   p dif. de q   p -> nulo   q ->    0
2:   p dif. de q   p ->    0   q ->    0
3:   p igual a q   p ->    0   q ->    0
4:   p dif. de q   p ->    0   q ->    1
5:   p dif. de q   p ->    1   q ->    1
6:   p igual a q   p ->    1   q ->    1
7:   p dif. de q   p ->    1   q ->    1
8:   p igual a q   p ->    1   q ->    1
9:   p dif. de q   p -> nulo   q ->    1
10:   p igual a q   p -> nulo   q -> nulo


*)
