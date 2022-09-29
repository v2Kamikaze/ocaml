type birth = {
  name: string; 
  date_birth: string;
};;

type choice = L | I | S;;

let rec for_each fn list =
  match list with
  | [] -> ()
  | h::t -> fn h; for_each fn t;;

let add_reg reg list = reg::list;;

let show_menu = print_string "L: Listar o cadastro inteiro\nI: Inserir um registro no cadastro\nS: Sair\n";;

let contains name list = (List.filter (fun r -> r.name = name) list) != [];;

let list_all_reg list = 
  if List.length list = 0 then print_string "---------------------------\n"
  else (for_each (fun r -> print_string ("Nome: " ^ r.name ^ "\n" ^ "Nascimento: " ^ r.date_birth ^ "\n" ^ "---------------------------\n")) list);
  list;;

let reg_exists name date_birth list = print_string "Registro não inserido, nome já existente!\n"; add_reg {name; date_birth} list;;

let cadaster_reg list = 
  print_string "Leitura de novo registro: \nNome: ";
  let name = read_line () in
    print_string "\nNascimento (DD/MM/AAAA): ";
    let date_birth = read_line () in
      if not (contains name list) then 
        add_reg {name; date_birth} list
      else
        reg_exists name date_birth list;;


let input_choice () = 
  print_string "Digite a opção escolhida (L/I/S): ";
  let choice = read_line () in 
    if (String.compare choice "L") = 0 then Some L
    else if (String.compare choice "I") = 0 then Some I
    else if (String.compare choice "S") = 0 then Some S
    else None;;

let rec main list  = 
  show_menu;
  let c = input_choice () in 
    match c with
    | Some L -> main (list_all_reg list)
    | Some I -> main (cadaster_reg list)
    | Some S -> ()
    | None -> main list;;

main [];;
