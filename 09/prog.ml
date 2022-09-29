Aux.create_file "fuen.txt" 50;;

let res = Aux.read_file "fuen.txt";;           


match res with
| None -> print_endline "Lista vazia!"
| Some l -> Aux.for_each print_endline l; ()
;;

