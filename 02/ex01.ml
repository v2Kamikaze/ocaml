let rec	substituir lista de para	=	
  match	lista	with
  | []	-> []
  |	h :: t	->	
      if h = de then para :: substituir t de para 
      else h :: substituir t de para;;

print_endline
  begin
  if (substituir [1;2;3;1;2;3;3;3;2;2;1;1] 2 0) = [1;0;3;1;0;3;3;3;0;0;1;1] then "Ok"
  else "Algo errado"
end;;