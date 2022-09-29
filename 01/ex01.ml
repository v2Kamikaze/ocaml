let rec pegar_segundo_operando () =
  print_string "Digite o segundo operando: ";
  let entrada = read_float() in 
    if entrada >= 0.0 then 
      entrada
  else 
    pegar_segundo_operando ();;


let rec pegar_operador () = 
  print_string "Digite a operação (+ - * / **): ";
  let operador = read_line() in
    let op = String.trim operador in
      if op <> "+" && op <> "-" && op <> "*" &&  op <> "/" && op <> "**" then 
          pegar_operador ()
      else
        op;;


let calcular_escolha n1 op n2 = 
  if op = "+" then n1 +. n2
  else if op = "-" then n1 -. n2
  else if op = "*" then n1 *. n2
  else if op = "/" then n1 /. n2
  else
    n1 ** n2;;


print_string "Digite o primeiro operando: ";
let primeiro_operando = read_float() in
  let operador = pegar_operador () in
  let segundo_operando = pegar_segundo_operando () in
    Printf.printf "Resultado: %.3f\n\n" (calcular_escolha primeiro_operando operador segundo_operando);;
