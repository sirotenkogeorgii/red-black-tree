import DisplayTree
import Helpers


contains NIL _ = False
contains (Node left _ val right) key
    | val < key = contains right key
    | val > key = contains left key
    | otherwise = True
   
insert tree val = recolor (insert_helper tree val) BLACK

delete tree val
    | get_val tree == val && not (has2children tree) = recolor (get_child tree) BLACK
    | otherwise = usual_delete_result
    where usual_delete_result = case (delete_helper tree val) of
                                    (Node left color root_val right) -> (Node left color root_val right)
                                    (DoubleBlackNode tree)           -> tree
                                    _                                -> NIL

multiple_delete tree [] = tree
multiple_delete tree (x:xs) = multiple_delete (delete tree x) xs

multiple_insert tree [] = tree
multiple_insert tree (x:xs) = multiple_insert (insert tree x) xs

create_tree xs = multiple_insert NIL xs
