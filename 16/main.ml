class virtual ['t] tConteinerSimples = 
  object (self)

  method virtual inserir: 't -> unit
  method virtual remover: 't option
  method virtual vazio: bool

  method esvaziar = 
    let sep () = 
      print_endline "-----------------"
    in
    sep();

    while self#vazio = false do 
      match self#remover with
      | None -> print_endline "Absurdo!"
      | Some e -> Printf.printf "Removido:%d\n" e
    done;
    sep()
  end
;;

class ['t] tFilaVetor valor_inicial = 
  object(self)
  inherit ['t] tConteinerSimples

  val mutable vetor: 't array = Array.make 1 valor_inicial
  val mutable pos = -1
  val mutable tamanho = 0
  val mutable pos_ini = 0
 
  method inserir v = 
    tamanho <- tamanho + 1;
    pos <- pos + 1;
    let capacidade = (Array.length vetor) in
    let _red () = 
      let novo_array = Array.make (capacidade * 2) valor_inicial in 
      for i = 0 to capacidade - 1 do 
        novo_array.(i) <- vetor.(i)
      done;
      vetor <- novo_array
    in

    if capacidade <= pos
    then 
      begin
        _red ();
        vetor.(pos) <- v
      end
    else 
      vetor.(pos) <- v

  method vazio = tamanho = 0

  method remover = 
    if self#vazio
    then None
    else begin 
      tamanho <- tamanho - 1;
      let v = Some(vetor.(pos_ini)) in 
        pos_ini <- pos_ini + 1;
        v
    end
  end
;;

let f : int tConteinerSimples  = new tFilaVetor 0 in
let remover () =
  match f#remover with
  | None   -> print_endline "Absurdo!"
  | Some e -> Printf.printf "Removido: %d\n" e
in
f#inserir 1;
f#inserir 2;
f#inserir 3;
f#inserir 4;
remover ();
remover ();
print_endline "----------";
f#inserir 5;
f#inserir 6;
remover ();
f#inserir 7;
f#inserir 8;
f#esvaziar ;;