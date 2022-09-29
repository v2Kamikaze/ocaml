type 'a bin_tree = Leaf | Node of 'a bin_tree * 'a * 'a bin_tree;;


let min_value t = 
  let rec min_v t min =
    match t with
    | Leaf -> min
    | Node(left, v, right) -> min_v left min ; min_v right min 
  in
  min_v t Float.max_float;;

let rec delete_node tree value = 
  match tree with 
  | Leaf -> Leaf
  | Node (left, node, right) when value < node ->
    Node((delete_node left value), node, right)
  | Node (left, node, right) when value > node ->
    Node(left, node, (delete_node right value))
  | Node (Leaf, node, Leaf) -> 
    Leaf
  | Node (Leaf, node, right) -> 
    right
  | Node (left, node, Leaf) -> 
    left
  | Node (left, node, right) -> 
    let new_value = min_value right in
    Node(left, new_value, (delete_node right new_value))


let rec bin_search tree value = 
  match tree with
  | Leaf -> None
  | Node(t1, v, t2) when v = value -> Some tree
  | Node(t1, v, _) when v < value -> bin_search t1 value
  | Node(_, v, t2) -> bin_search t2 value;;

let t = Node(Node( Leaf, 5.0, Leaf), 1.0, Node( Leaf, -7.0, Leaf));;

let t = delete_node t 1.0;;

let res = match bin_search t (-7.0) with
  | Some(v) -> v
  | None -> Leaf;;

match res with
| Node(_, v, _) -> print_float v
| Leaf -> print_string "Não possui esse valor\n";;