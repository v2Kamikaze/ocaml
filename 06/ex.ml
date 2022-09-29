let file_name = "alunos.csv";;
let file_name_out = "media.csv";;

exception	EmptyList of string;;


let open_csv file_name =
  let rec _open_file file l = 
    try _open_file file ((input_line file)::l);
    with 
    | End_of_file -> l;
  in 
  let file = open_in file_name in 
    let csv_content = (_open_file file []) in
      close_in file;
      csv_content
;;

let average l = 
  let rec _average l acc =
    match l with
    | [] -> acc
    | h::t -> _average t (acc +. (float_of_string h))
  in
   match l with
   | [] ->  raise @@ EmptyList "Lista vazia!"
   | _::t -> (_average t 0.0) /. (float_of_int (List.length t))
;;


let to_pair_list a b =
  let rec _to_pair_list a b l = 
    match a, b with
    | [],[] ->  l
    | name::t1, avrg::t2 -> _to_pair_list t1 t2 ((name, avrg)::l)
    | _ -> l
  in
  _to_pair_list a b []
;;

let rec each fn l = 
  match l with
  | [] -> ()
  | h::t -> fn h; each fn t
;;

let save_csv l file_name_out =  
  let file = open_out file_name_out in
    each (fun (n, a) -> output_string file (n ^ "," ^ (string_of_float a) ^ "\n")) l;
    close_out file;
;;

let csv_splited = List.map (fun s -> String.split_on_char ',' s)  (open_csv file_name);;
let avrg = List.map average csv_splited;;
let names = List.map (function [] -> "" | h::_ -> h) csv_splited;;
let pairs = to_pair_list names avrg;;
save_csv pairs file_name_out;;