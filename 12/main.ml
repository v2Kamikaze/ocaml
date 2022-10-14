type 'a node = {
  value: 'a;
  mutable prev: 'a node ref option;
  mutable next: 'a node ref option;
};;

type 'a linked_list = {
  mutable head: 'a node ref option
};;

let make () = {head = None};;

(* Adiciona um novo elemento na cabeça da lista *)
let insert llist e = 
  let new_e = { value = e; prev = None; next = llist.head } in
  begin
    match llist.head with
    | None -> ()
    | Some r -> (!r).prev <- Some (ref new_e)
  end;
  llist.head <- Some (ref new_e)
;;

let find llist e = 
  let rec _find node = 
    match node with
    | None -> None
    | Some r -> if (!r).value = e 
      then Some (ref {value = (!r).value; prev = None; next = None}) 
      else _find (!r).next 
  in _find llist.head
;;

let each llist f = 
  let rec _each node = 
    match node with
    | None -> ()
    | Some r -> f ((!r).value); _each ((!r).next)
  in _each (llist.head)
;;


let print_llist llist = each llist (Printf.printf "-> %d\n");;

let main_ex1 () = 
  let llist = make () in 
  insert llist 1;
  insert llist 2;
  insert llist 3;
  print_llist llist;

  match find llist 2 with
  | None -> print_string "Não encontrado.\n"
  | Some r -> Printf.printf "Encontrado o valor: %d\n" ((!r).value);

    let new_e = { value = 4; prev = None; next = None } in 
    r := new_e ;
    print_llist llist;
;;

let remove llist node = 
  let rec _remove n = 
    match n with
    | None -> ()
    | Some r ->
      if (!r).value = node.value
      then
        begin
          (* rl -> reg left  rr -> reg right *)
          match (!r).prev, (!r.next) with
          | None, None -> llist.head <- None
          | None, Some rr ->
            (!rr).prev <- None;
            llist.head <- Some rr
          | Some rl, None -> 
            (!rl).next <- None;
          | Some rl, Some rr ->
            (!rl).next <- Some (ref (!rr)); 
            (!rr).prev <- Some (ref (!rl));
        end
      else _remove (!r).next
  in _remove llist.head
;;


(* let remove llist node = 
  match node.prev, node.next with
  | None, None -> llist.head <- None
  | None, Some rr ->
    (!rr).prev <- None;
    llist.head <- Some rr
  | Some rl, None -> 
    (!rl).next <- None;
  | Some rl, Some rr ->
    (!rl).next <- Some (ref (!rr)); 
    (!rr).prev <- Some (ref (!rl));
;; *)


let main_ex2 () =
  let llist = make () in
  insert llist 1;
  insert llist 2;
  insert llist 3;
  print_llist llist;
  match find llist 2 with
  | None   -> print_endline "2 não encontrado!"
  | Some r ->
    begin
      Printf.printf "Encontrado: %d\n" (!r).value;
      remove llist (!r);
      print_llist llist;
      match find llist 3 with
      | None   -> print_endline "3 não encontrado"
      | Some r ->
        begin
          Printf.printf "Encontrado: %d\n" (!r).value;
          remove llist (!r);
          print_llist llist;
          match find llist 1 with
          | None   -> print_endline "1 não encontrado"
          | Some r ->
            begin
              Printf.printf "Encontrado: %d\n" (!r).value;
              remove llist (!r);
              print_llist llist;
            end
        end
    end 
;;


print_endline "============== EX 1 ==============";;
main_ex1 ();;
print_endline "============== EX 2 ==============";;
main_ex2 ();;



