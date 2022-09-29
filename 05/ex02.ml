type ('a, 'b) bin_tree = Leaf of 'b | Node of ('a, 'b) bin_tree * 'a * ('a, 'b) bin_tree;;

let t = Node(
          Leaf 3.0, (+.), 
          Node( 
            Leaf 1.0, (-.), Leaf 10.0))

let rec sum t =
  match t with
  | Leaf x -> x
  | Node(l, op, r) ->  op (sum l) (sum r) ;;

Printf.printf "%f\n" (sum t);;