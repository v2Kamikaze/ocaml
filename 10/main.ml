let bissexto ano =  ((ano mod 4) = 0 || (ano mod 100) = 0) && (ano mod 400) != 400;;

module type Data =
  sig 
  type t
  val dia: t -> int
  val mes: t -> int
  val ano: t -> int
  val criar: int -> int -> int -> t
  val string_of_data: t -> string
  val data_of_string: string -> t option
  val dia_seguinte: t -> t
  end

module DataTupla : Data = 
  struct
  type t = int * int * int
  let dia (d,m,a) = d
  let mes (d,m,a) = m
  let ano (d,m,a) = a
  let criar d m a = (d, m, a)
  let string_of_data (d, m, a) = (string_of_int d) ^ "/" ^ (string_of_int m) ^ "/" ^ (string_of_int a)
  let data_of_string s = 
    let ls = (String.split_on_char '/' s) in
    if List.length ls != 3 then None
    else
      match ls with 
      | [] -> None
      | d::ma -> match ma with
        | [] -> None
        | m::a -> match a with
          | [] -> None
          | a::t -> Some ((int_of_string d), (int_of_string m), (int_of_string a))


  let dia_seguinte (dia, mes, ano) = 
    if bissexto ano && (dia + 1) > 29  && mes = 12 then (0, 1, ano + 1)
    else if bissexto ano && (dia + 1) > 29 then (0, mes + 1, ano)
    else if bissexto ano then (dia +1, mes, ano)
    else if mes = 2 && (dia + 1) > 28 then (0, mes + 1, ano)
    else if (dia + 1) > 31 && mes = 12 then (0, 1, ano + 1)
    else if (dia + 1) > 31 then (0, mes + 1, ano) 
    else (dia + 1, mes, ano)
  end


module DataRegistro : Data = 
  struct
  type t = {dia: int; mes: int; ano: int}
  let dia {dia = d; mes = m; ano = a} = d (* Notação  padrão *)
  let mes {dia; mes; ano} = mes           (* Notação abreviada *)
  let ano {dia; mes; ano} = ano
  let criar dia mes ano = {dia; mes; ano} (* {dia=d;mes=m;ano=a} *)
  let string_of_data {dia; mes; ano} = (string_of_int dia) ^ "/" ^ (string_of_int mes) ^ "/" ^ (string_of_int ano)
  let data_of_string s = 
    let ls = (String.split_on_char '/' s) in
    if List.length ls != 3 then None
    else
      match ls with 
      | [] -> None
      | d::ma -> match ma with
        | [] -> None
        | m::a -> match a with
          | [] -> None
          | a::t -> Some {dia = (int_of_string d); mes = (int_of_string m); ano = (int_of_string a)} 
  let dia_seguinte {dia; mes; ano} = 
    if bissexto ano && (dia + 1) > 29  && mes = 12 then {dia = 0; mes = 1; ano = ano + 1}
    else if bissexto ano && (dia + 1) > 29 then {dia = 0; mes = mes + 1; ano = ano}
    else if bissexto ano then {dia = dia +1; mes = mes; ano = ano}
    else if mes = 2 && (dia + 1) > 28 then {dia = 0; mes = mes + 1; ano}
    else if (dia + 1) > 31 && mes = 12 then {dia= 0; mes = 1; ano = ano + 1}
    else if (dia + 1) > 31 then {dia = 0; mes = mes + 1; ano = ano} 
    else {dia = dia + 1; mes = mes; ano = ano}
  end

module type UtilData =
  sig 
  type t 
  val anterior: t -> t -> bool
  end

module CriarUtilData (D: Data):
  UtilData with type t := D.t
  =
  struct
    open D 
    let anterior a b = 
      ano a < ano b ||
      (ano a = ano b && (mes a < mes b || (mes a = mes b && dia a < dia b)));;
  end ;;

module UT (* UtilTupla *) = CriarUtilData(DataTupla) ;;
module UR (* UtilReg   *) = CriarUtilData(DataRegistro);;

let main_data_reg () =
  print_string "Digite uma data no formato d/m/a: ";
  let data = read_line () in
    match DataRegistro.data_of_string data with
    | None -> print_endline ("Você digitou um formato de data inválido! d/m/a != " ^ data)
    | Some d -> DataRegistro.dia_seguinte d |> DataRegistro.string_of_data |> print_endline;;


let main_data_tuple () =
  print_string "Digite uma data no formato d/m/a: ";
  let data = read_line () in
    match DataTupla.data_of_string data with
    | None -> print_endline ("Você digitou um formato de data inválido! d/m/a != " ^ data)
    | Some d -> DataTupla.dia_seguinte d |> DataTupla.string_of_data |> print_endline;;
    

main_data_reg ();;
main_data_tuple ();;