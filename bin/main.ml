open Hero_tree.Tree
open Hero_tree

let example_tree = Leaf
let example_tree = insert 3 example_tree
let example_tree = insert 1 example_tree
let example_tree = insert 2 example_tree
let example_tree = insert 4 example_tree
let example_tree = insert 5 example_tree

let basic_tree = Basic.convert_to_basic example_tree
let () = Basic.inorder_print basic_tree
