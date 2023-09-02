module Helpers where


-- vertex colors. can be only two colors.
data Color = RED | BLACK deriving (Show, Eq)

-- the tree can be represented as an empty value, or as a regular node, or as a double black node, 
-- which only matters for restoring invariants after deleting some value.
data Tree a = NIL | Node (Tree a) Color a (Tree a) | DoubleBlackNode (Tree a) deriving (Show,Eq)


-- get the color of the node.
get_color NIL = BLACK
get_color (Node _ color _ _) = color


-- get the value of the node.
get_val (Node _ _ val _) = val


-- get the min value of the tree.
get_min (Node left _ val _)
    | left /= NIL = get_min left
    | otherwise = val


-- check if the node has 2 children.
has2children NIL = False
has2children (Node left _ _ right)
    | left == NIL || right == NIL = False
    | otherwise = True


-- get the child of the node. Makes sense only for nodes with only one child.
get_child NIL = NIL
get_child (Node left _ _ right)
    | left == NIL = right
    | right == NIL = left
    | otherwise = NIL


-- check if two nodes has the same color.
different_color tree1 tree2
    | get_color tree1 == get_color tree2 = False
    | otherwise = True


-- recolor the node with the color as the second parameter.
recolor NIL _ = NIL
recolor (Node left _ val right) new_color = (Node left new_color val right)
recolor (DoubleBlackNode (Node left _ val right)) new_color = DoubleBlackNode (Node left new_color val right)


-- set a new value of the node.
set_val (Node left color _ right) val = (Node left color val right)
set_val (DoubleBlackNode (Node left color _ right)) val = DoubleBlackNode (Node left color val right)


-- if sibling s is black and at least one of sibling’s children is red, perform rotation(s). Let the red child of s be r.
-- s is right child of its parent and r is right child of s or both children of s are red.
restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node slib_left BLACK slib_val (Node r_left RED r_val r_right))) = -- (right right case)
        Node (Node replacing_node BLACK parent_val slib_left) parent_color slib_val (Node r_left BLACK r_val r_right)

-- if sibling s is black and at least one of sibling’s children is red, perform rotation(s). Let the red child of s be r. 
-- s is left child of its parent and r is left child of s or both children of s are red.
restore_delete (Node (Node (Node r_left RED r_val r_right) BLACK slib_val slib_right) parent_color parent_val (DoubleBlackNode replacing_node)) = -- (left left case)
        Node (Node r_left BLACK r_val r_right) parent_color slib_val (Node slib_right BLACK parent_val replacing_node)

-- if sibling s is black and at least one of sibling’s children is red, perform rotation(s). Let the red child of s be r.
-- s is right child of its parent and r is left child of s.
restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node (Node r_left RED r_val r_right) BLACK slib_val slib_right)) = -- (righ left case)
        Node (Node replacing_node BLACK parent_val r_left) parent_color r_val (Node r_right BLACK slib_val slib_right)

-- if sibling s is black and at least one of sibling’s children is red, perform rotation(s). Let the red child of s be r. 
-- s is left child of its parent and r is right child.
restore_delete (Node (Node slib_left BLACK slib_val (Node r_left RED r_val r_right)) parent_color parent_val (DoubleBlackNode replacing_node)) = -- (left right case)
        Node (Node slib_left BLACK slib_val r_left) parent_color r_val (Node r_right BLACK parent_val replacing_node)

-- other cases.
restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node slib_left slib_color slib_val slib_right))
        | parent_color == BLACK && slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            DoubleBlackNode (Node replacing_node BLACK parent_val (Node slib_left RED slib_val slib_right)) --  if sibling is black and its both children are black, perform recoloring, and recur for the parent if parent is black. 
        | parent_color == RED && slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            (Node replacing_node BLACK parent_val (Node slib_left RED slib_val slib_right)) --  case like one above, but with red parent and we don't recurse.
        | otherwise = restore_delete (Node (Node replacing_node BLACK parent_val (recolor slib_left RED)) BLACK slib_val (recolor slib_right BLACK)) -- if sibling is red, perform a rotation to move old sibling up, recolor the old sibling and parent. the new sibling is always black.

-- mirror case like the one above.
restore_delete (Node (Node slib_left slib_color slib_val slib_right) parent_color parent_val (DoubleBlackNode replacing_node))
        | parent_color == BLACK && slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            DoubleBlackNode (Node (Node slib_left RED slib_val slib_right) BLACK parent_val replacing_node) --  if sibling is black and its both children are black, perform recoloring, and recur for the parent if parent is black. 
        | parent_color == RED && slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            (Node (Node slib_left RED slib_val slib_right) BLACK parent_val replacing_node) --  case like one above, but with red parent and we don't recurse.
        | otherwise = (Node (recolor slib_left BLACK) BLACK slib_val (Node (recolor slib_right RED) BLACK parent_val replacing_node)) -- -- if sibling is red, perform a rotation to move old sibling up, recolor the old sibling and parent. the new sibling is always black.

-- othwewise don't do anything.
restore_delete tree = tree


-- we can't delete from an empty tree.
delete_helper NIL _ = NIL

-- helper delete function for the case, when the current node is not NIL.
delete_helper tree@(Node left color node_val right) val
    | right /= NIL && get_val right == val && not (has2children right) && different_color (get_child right) right = 
            (Node left color node_val (recolor (get_child right) BLACK)) -- simple case when we delete node with one children and the only child has the different color.
    | left /= NIL && get_val left == val && not (has2children left) && different_color (get_child left) left = 
            (Node (recolor (get_child left) BLACK) color node_val right) -- simple case when we delete node with one children and the only child has the different color.
    | right /= NIL && get_val right == val && not (has2children right) && get_color right == BLACK && get_color (get_child right) == BLACK = 
            restore_delete (Node left color node_val (DoubleBlackNode (get_child right))) -- delete the current node and the only child has the same black color: replace with child -> make it double black -> restore invariant.     
    | left /= NIL && get_val left == val && not (has2children left) && get_color left == BLACK && get_color (get_child left) == BLACK = 
            restore_delete (Node (DoubleBlackNode (get_child left)) color node_val right) -- delete the current node and the only child has the same black color: replace with child -> make it double black -> restore invariant.                                                                                                     
    | node_val < val = restore_delete (Node left color node_val (delete_helper right val)) -- if the current node value is less -> go right                                                                                    
    | node_val > val = restore_delete (Node (delete_helper left val) color node_val right) -- if the current node value is greater -> go left                                                                                      
    | otherwise = restore_delete (set_val deleted_min_tree min_val) -- if delete the current node: change current value by the minimum value in the right subtree and delete it in the right subtree.
    where min_val = get_min right
          deleted_min_tree = delete_helper tree min_val


resore_insert (Node NIL color val NIL) = (Node NIL color val NIL) 

restore_insert (Node (Node (Node left_x RED x right_x) RED p right_p) BLACK g (Node left_u RED u right_u)) = 
    (Node (Node (Node left_x RED x right_x) BLACK p right_p) RED g (Node left_u BLACK u right_u))
restore_insert (Node (Node left_p RED p (Node left_x RED x right_x)) BLACK g (Node left_u RED u right_u)) = 
    (Node (Node left_p BLACK p (Node left_x RED x right_x)) RED g (Node left_u BLACK u right_u))
restore_insert (Node (Node left_p RED p right_p) BLACK g (Node (Node left_x RED x right_x) RED u right_u)) = 
    (Node (Node left_p BLACK p right_p) RED g (Node (Node left_x RED x right_x) BLACK u right_u))
restore_insert (Node (Node left_p RED p right_p) BLACK g (Node left_u RED u (Node left_x RED x right_x))) = 
    (Node (Node left_p BLACK p right_p) RED g (Node left_u BLACK u (Node left_x RED x right_x)))

restore_insert (Node (Node (Node left_x RED x right_x) RED p right_p) BLACK g uncle) = 
    (Node (Node left_x RED x right_x) BLACK p (Node right_p RED g uncle))
restore_insert (Node (Node left_p RED p (Node left_x RED x right_x)) BLACK g uncle) = 
    (Node (Node left_p RED p left_x) BLACK x (Node right_x RED g uncle))
restore_insert (Node uncle BLACK g (Node left_p RED p (Node left_x RED x right_x))) = 
    (Node (Node uncle RED g left_p) BLACK p (Node left_x RED x right_x))
restore_insert (Node uncle BLACK g (Node (Node left_x RED x right_x) RED p right_p)) = 
    (Node (Node uncle RED g left_x) BLACK x (Node right_x RED p right_p))

restore_insert tree = tree

insert_helper NIL val = (Node NIL RED val NIL)
insert_helper tree@(Node left color parent_val right) val
    | parent_val < val = restore_insert (Node left color parent_val (insert_helper right val))
    | parent_val > val = restore_insert (Node (insert_helper left val) color parent_val right)
    | otherwise = tree

