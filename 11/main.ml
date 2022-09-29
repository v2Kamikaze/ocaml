type reg = {linha: string; mutable contador: int};;


let ler_arquivo nome_arq =
  let rec _ler_arquivo arq l =
    try
      let linha = input_line arq in 
        _ler_arquivo arq (linha::l)
    with
    | End_of_file -> l 
  in 
    try
      let arq = open_in nome_arq in 
      let res = _ler_arquivo arq [] in 
         close_in arq;
         Some res
    with
    | Sys_error _ -> None;;

let contar_linhas linhas_arq =
  let rec _contar_linhas linhas_arq regs = 
    match linhas_arq with
    |[] -> regs
    |h::t -> 
      try
        let linha = List.find (fun l -> l.linha = h) regs in 
          linha.contador <- linha.contador + 1;
          _contar_linhas t regs
      with
      | Not_found -> _contar_linhas t ({linha = h; contador = 1}::regs)
  in _contar_linhas linhas_arq [];;
;;
    

let rec for_each f = function
  |[] -> ()
  |h::t -> f h; for_each f t;;
  
let linhas = match (ler_arquivo "main.ml") with
  | None -> []
  | Some l -> l |> contar_linhas;;

for_each (fun r -> Printf.printf "[%s] -> %d\n" r.linha r.contador) linhas;;