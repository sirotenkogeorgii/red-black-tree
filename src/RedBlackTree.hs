import DisplayTree
import Helpers

resore_insert (Node NIL color val NIL) = (Node NIL color val NIL) 

restore_insert (Node (Node (Node left_x RED x right_x) RED p right_p) BLACK g (Node left_u RED u right_u)) = (Node (Node (Node left_x RED x right_x) BLACK p right_p) RED g (Node left_u BLACK u right_u))
restore_insert (Node (Node left_p RED p (Node left_x RED x right_x)) BLACK g (Node left_u RED u right_u)) = (Node (Node left_p BLACK p (Node left_x RED x right_x)) RED g (Node left_u BLACK u right_u))
restore_insert (Node (Node left_p RED p right_p) BLACK g (Node (Node left_x RED x right_x) RED u right_u)) = (Node (Node left_p BLACK p right_p) RED g (Node (Node left_x RED x right_x) BLACK u right_u))
restore_insert (Node (Node left_p RED p right_p) BLACK g (Node left_u RED u (Node left_x RED x right_x))) = (Node (Node left_p BLACK p right_p) RED g (Node left_u BLACK u (Node left_x RED x right_x)))

restore_insert (Node (Node (Node left_x RED x right_x) RED p right_p) BLACK g uncle) = (Node (Node left_x RED x right_x) BLACK p (Node right_p RED g uncle))
restore_insert (Node (Node left_p RED p (Node left_x RED x right_x)) BLACK g uncle) = (Node (Node left_p RED p left_x) BLACK x (Node right_x RED g uncle))
restore_insert (Node uncle BLACK g (Node left_p RED p (Node left_x RED x right_x))) = (Node (Node uncle RED g left_p) BLACK p (Node left_x RED x right_x))
restore_insert (Node uncle BLACK g (Node (Node left_x RED x right_x) RED p right_p)) = (Node (Node uncle RED g left_x) BLACK x (Node right_x RED p right_p))

restore_insert tree = tree

insert_helper NIL val = (Node NIL RED val NIL)
insert_helper tree@(Node left color parent_val right) val
    | parent_val < val = restore_insert (Node left color parent_val (insert_helper right val))
    | parent_val > val = restore_insert (Node (insert_helper left val) color parent_val right)
    | otherwise = tree

insert tree val = recolor (insert_helper tree val) BLACK


contains NIL _ = False
contains (Node left _ val right) key
    | val < key = contains right key
    | val > key = contains left key
    | otherwise = True

-- NOTE: maybe we should restore insert, becouse parent color can create sequative red nodes
restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node slib_left BLACK slib_val (Node r_left RED r_val r_right))) = -- 3.2) (a) (right right case)
        Node (Node replacing_node BLACK parent_val slib_left) parent_color slib_val (Node r_left BLACK r_val r_right)

restore_delete (Node (Node (Node r_left RED r_val r_right) BLACK slib_val slib_right) parent_color parent_val (DoubleBlackNode replacing_node)) = -- 3.2) (a) (left left case)
        Node (Node r_left BLACK r_val r_right) parent_color slib_val (Node slib_right BLACK parent_val replacing_node)

        
restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node (Node r_left RED r_val r_right) BLACK slib_val slib_right)) = -- 3.2) (a) (righ left case)
        Node (Node replacing_node BLACK parent_val r_left) parent_color r_val (Node r_right BLACK slib_val slib_right)

restore_delete (Node (Node slib_left BLACK slib_val (Node r_left RED r_val r_right)) parent_color parent_val (DoubleBlackNode replacing_node)) = -- 3.2) (a) (left right case)
        Node (Node slib_left BLACK slib_val r_left) parent_color r_val (Node r_right BLACK parent_val replacing_node)


restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node slib_left slib_color slib_val slib_right))
        | parent_color == BLACK && slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            DoubleBlackNode (Node replacing_node BLACK parent_val (Node slib_left RED slib_val slib_right))
        | parent_color == RED && slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            (Node replacing_node BLACK parent_val (Node slib_left RED slib_val slib_right))         
        | otherwise = restore_delete (Node (Node replacing_node BLACK parent_val (recolor slib_left RED)) BLACK slib_val (recolor slib_right BLACK))

restore_delete (Node (Node slib_left slib_color slib_val slib_right) parent_color parent_val (DoubleBlackNode replacing_node))
        | parent_color == BLACK && slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            DoubleBlackNode (Node (Node slib_left RED slib_val slib_right) BLACK parent_val replacing_node)
        | parent_color == RED && slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            (Node (Node slib_left RED slib_val slib_right) BLACK parent_val replacing_node)
        | otherwise = (Node (recolor slib_left BLACK) BLACK slib_val (Node (recolor slib_right RED) BLACK parent_val replacing_node))

restore_delete tree = tree

delete_helper NIL _ = NIL
delete_helper tree@(Node left color node_val right) val
    | right /= NIL && get_val right == val && not (has2children right) && different_color (get_child right) right = (Node left color node_val (recolor (get_child right) BLACK)) -- 2)
    | left /= NIL && get_val left == val && not (has2children left) && different_color (get_child left) left = (Node (recolor (get_child left) BLACK) color node_val right) -- 2)
    | right /= NIL && get_val right == val && not (has2children right) && get_color right == BLACK && get_color (get_child right) == BLACK = restore_delete (Node left color node_val (DoubleBlackNode (get_child right))) -- 3.1,2) 
    | left /= NIL && get_val left == val && not (has2children left) && get_color left == BLACK && get_color (get_child left) == BLACK = restore_delete (Node (DoubleBlackNode (get_child left)) color node_val right) -- 3.1,2)                                                                                                           
    | node_val < val = restore_delete (Node left color node_val (delete_helper right val))                                                                                        
    | node_val > val = restore_delete (Node (delete_helper left val) color node_val right)                                                                                        
    | otherwise = restore_delete (set_val deleted_min_tree min_val)
    where min_val = get_min right
          deleted_min_tree = delete_helper tree min_val
       
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