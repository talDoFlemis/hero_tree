type 'a binary_tree = Leaf | Node of 'a * 'a binary_tree * 'a binary_tree

let rec insert value tree =
  match tree with
  | Leaf -> Node (value, Leaf, Leaf)
  | Node (v, left, right) ->
      if value < v then Node (v, insert value left, right)
      else if value > v then Node (v, left, insert value right)
      else tree

let rec search value tree =
  match tree with
  | Leaf -> false
  | Node (v, left, right) ->
      if value = v then true
      else if value < v then search value left
      else search value right

let rec find_min tree =
  match tree with
  | Leaf -> None
  | Node (v, Leaf, _) -> Some v
  | Node (_, left, _) -> find_min left

let rec remove value tree =
  match tree with
  | Leaf -> Leaf
  | Node (v, left, right) -> (
      if value < v then Node (v, remove value left, right)
      else if value > v then Node (v, left, remove value right)
      else
        match (left, right) with
        | Leaf, _ -> right
        | _, Leaf -> left
        | _, _ -> (
            let min_right = find_min right in
            match min_right with
            | None -> tree
            | Some min_val -> Node (min_val, left, remove min_val right)))

let rec inorder tree =
  match tree with
  | Leaf -> []
  | Node (v, left, right) -> inorder left @ [ v ] @ inorder right
