type 'a bin_tree = Leaf | Node of 'a bin_tree * 'a * 'a bin_tree;;

let rec bin_search tree value = 
  match tree with
  | Leaf -> None
  | Node(t1, v, t2) when v = value -> Some tree
  | Node(t1, v, _) when v < value -> bin_search t1 value
  | Node(_, v, t2) -> bin_search t2 value;;


let t = Node(Node( Leaf, 5, Leaf), 1, Node( Leaf, 7, Leaf));;

let res = match bin_search t 7 with
| Some(v) -> v
| None -> Leaf;;

match res with
| Node(_, v, _) -> print_int v
| Leaf -> print_string "Não possui esse valor\n";;


