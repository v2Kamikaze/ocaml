
type tEstudante = <nome: string>;;

type tEstudanteComp = <linguagem : string; nome : string >;;

let criar_estudante (nome: string): tEstudante = 
  object
    method nome = nome
  end
;;

let criar_estudante_comp (nome: string) (ling: string): tEstudanteComp = 
  object
    method nome = nome
    method linguagem = ling
  end
;;

let imprimir_lista_est_comp (func: (tEstudanteComp -> unit)) lista_estudantes_comp =
  let rec _each l = 
    match l with
    | [] -> ()
    | h::t -> func h; _each t 
  in _each lista_estudantes_comp
;;

let imprimir_estudante estudante = print_endline estudante#nome;; 


let lista = [ 
  criar_estudante_comp "Guido van Rossum"  "Python";
  criar_estudante_comp "Dennis Ritchie"    "C";
  criar_estudante_comp "Bjarne Stroustrup" "C++";
  criar_estudante_comp "Xavier Leroy"      "OCaml" 
];;

print_endline "Lista de Estudantes de Computação:";;
imprimir_lista_est_comp (fun e -> print_endline e#nome) lista;;

print_endline "====================================";;

print_endline "Lista de Estudantes de Computação:";;
imprimir_lista_est_comp imprimir_estudante lista;;