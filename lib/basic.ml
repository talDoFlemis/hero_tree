type 'a basic_coord_tree =
  | Leaf
  | Node of 'a * float * float * 'a basic_coord_tree * 'a basic_coord_tree
[@@deriving show { with_path = false }, sexp]

let scale = 30.0

let rec calculate_coords tree level left_lim =
  match tree with
  | Tree.Leaf -> (Leaf, 0.0, 0.0)
  | Node (value, Leaf, Leaf) ->
      let new_y = level *. scale in
      let new_x = left_lim in
      (Node (value, new_x, new_y, Leaf, Leaf), new_x, new_x)
  | Node (value, left, Leaf) ->
      let new_y = level *. scale in
      let new_left, root_x, right_lim =
        calculate_coords left (level +. 1.0) left_lim
      in
      (Node (value, root_x, new_y, new_left, Leaf), root_x, right_lim)
  | Node (value, Leaf, right) ->
      let new_y = level *. scale in
      let new_right, root_x, right_lim =
        calculate_coords right (level +. 1.0) left_lim
      in
      (Node (value, root_x, new_y, Leaf, new_right), root_x, right_lim)
  | Node (value, left, right) ->
      let new_y = level *. scale in
      let new_left, left_root_x, left_right_lim =
        calculate_coords left (level +. 1.0) left_lim
      in
      let right_left_lim = left_right_lim +. scale in
      let new_right, right_root_x, right_lim =
        calculate_coords right (level +. 1.0) right_left_lim
      in
      let root_x = (left_root_x +. right_root_x) /. 2.0 in
      (Node (value, root_x, new_y, new_left, new_right), root_x, right_lim)

let convert_to_basic tree =
  let tree, _, _ = calculate_coords tree 0.0 0.0 in
  tree

let rec inorder_print (tree: int basic_coord_tree) =
  match tree with
  | Leaf -> ()
  | Node (value, x, y, left, right) ->
      inorder_print left;
      print_endline (Printf.sprintf "value: %i x:%f y:%f" value x y);
      inorder_print right
