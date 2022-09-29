let rec mapear lista f = 
  match lista with
  | [] -> []
  | h :: t -> f h :: mapear t f;;

let trocar n de para = if n = de then para else n;;

let substituir lista de para = mapear lista (fun x -> trocar x de para);;

print_endline
  begin
  if (substituir [1;2;3;1;2;3;3;3;2;2;1;1] 2 0) = [1;0;3;1;0;3;3;3;0;0;1;1] then "Ok"
  else "Algo errado"
end;;