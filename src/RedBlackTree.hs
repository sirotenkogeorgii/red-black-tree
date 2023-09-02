import DisplayTree
import Helpers


-- check if a function contains a key.
contains NIL _ = False
contains (Node left _ val right) key
    | val < key = contains right key
    | val > key = contains left key
    | otherwise = True
   

-- insert the key into the tree.
insert tree val = recolor (insert_helper tree val) BLACK


-- delete the key from the tree.
delete tree val
    | get_val tree == val && not (has2children tree) = recolor (get_child tree) BLACK -- if the desired value is the root.
    | otherwise = usual_delete_result
    where usual_delete_result = case (delete_helper tree val) of
                                    (Node left color root_val right) -> (Node left color root_val right)
                                    (DoubleBlackNode tree)           -> tree -- we don't want the root to be a double black.
                                    _                                -> NIL


-- delete multiple values from the tree.
multiple_delete tree [] = tree
multiple_delete tree (x:xs) = multiple_delete (delete tree x) xs


-- insert multiple values into the tree.
multiple_insert tree [] = tree
multiple_insert tree (x:xs) = multiple_insert (insert tree x) xs


-- create a tree from a list of values.
create_tree xs = multiple_insert NIL xs
