let rec for_each f = function
  |[] -> ()
  |h::t -> f h; for_each f t;;
  

let redim vetor_antigo tamanho_do_vetor novo_tamanho valor_inicial = 
  let novo_array = Array.make novo_tamanho valor_inicial in 
    for i = 0 to tamanho_do_vetor - 1 do 
      novo_array.(i) <- vetor_antigo.(i)
    done;
    novo_array
;;

let print_vetor vetor =  for i = 0 to Array.length vetor - 1 do 
  Printf.printf "v[%d] = %d\n" i vetor.(i)
done;;

let main () = 
  let rec _main vetor tamanho i = 
    print_string "Número a inserir (ou outra coisa para parar): ";
    let linha = read_line () in 
      if linha = "sair" then print_vetor vetor
      else if i = tamanho - 1 then 
        let novo_vetor = redim vetor tamanho (tamanho * 2) 0 in 
          novo_vetor.(i) <- int_of_string linha;
          _main novo_vetor (Array.length novo_vetor) (i + 1)
      else
        begin
          vetor.(i) <- int_of_string linha;
          _main vetor tamanho (i + 1)
        end
  in _main [|0|] 1 0;;

main () ;;
