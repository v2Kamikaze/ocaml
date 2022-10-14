class virtual tDesenhavel =
  object
  method virtual desenhar: unit
  end 
;;

class virtual ['t] tPilha =
  object
  method virtual vazia:       bool
  method virtual empilhar:    't -> unit
  method virtual topo:        't option
  method virtual desempilhar: 't option
  end 
;;


class ['t] tPilhaDesenhavel impr_elem = 
  object(self)

  inherit ['t] tPilha
  inherit tDesenhavel

  val mutable pilha: 't list = []
 
  method desenhar = 
    print_string "Pilha: ";
    List.iter (fun (e: 't) -> 
     impr_elem e;
     print_string " ";
    ) pilha;
    print_newline ()

  method vazia = 
    match pilha with
    | [] -> true
    | _ -> false

  method empilhar v = pilha <- v::pilha

  method topo = 
    match pilha with 
    | [] -> None
    | h::t -> Some h

  method desempilhar = 
    match pilha with
    | [] -> None
    | h::t -> pilha <- t; Some h

  end 
;;


let imprimir_na_tela (d: tDesenhavel) =
  let delim () = print_endline "----------"
  in
  print_char '\n';
  delim ();
  d#desenhar;
  delim ();;

let manipular_pilha (p: int tPilha) op : string =
  match op with
  | "d" -> begin
           match p#desempilhar with
           | None -> "Impossível desempilhar, pilha vazia"
           | Some e -> "Desempilhado: " ^ (string_of_int e)
           end
  | "v" -> if p#vazia
           then "A pilha está vazia"
           else "A pilha não está vazia"
  | "t" -> begin
           match p#topo with
           | None -> "Não há topo, pilha vazia"
           | Some e -> "Topo: " ^ (string_of_int e)
           end
  | _ -> begin
         match int_of_string_opt op with
         | None -> "Operação inválida!"
         | Some i -> p#empilhar i; "Empilhado: " ^ (string_of_int i)
         end
  ;;


let p: int tPilhaDesenhavel = new tPilhaDesenhavel (print_int) in
let rec menu () =
  imprimir_na_tela (p :> tDesenhavel);
  print_string "\n#: Emp; d: Desemp; v: Vazia; t: Topo; s: Sair: ";
  let op = read_line () in
  print_char '\n';
  match op with
  | "s" -> ()
  | _ -> print_endline (manipular_pilha (p :> int tPilha) op);
         menu ()
in
menu () ;;
