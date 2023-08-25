-- In addition to the requirements imposed on a binary search tree the following must be satisfied by a red–black tree:[18]

-- 1. Every node is either red or black.
-- 2. All NIL nodes (figure 1) are considered black.
-- 3. A red node does not have a red child.
-- 4. Every path from a given node to any of its descendant NIL nodes goes through the same number of black nodes.

-- (Conclusion) If a node N has exactly one child, it must be a red child, because if it were black, its NIL 
--     descendants would sit at a different black depth than N's NIL child, violating requirement 4.

data Color = RED | BLACK deriving (Show, Eq) 
data Tree a = NIL | Node (Tree a) Color a (Tree a) deriving (Show) 

recolor_root NIL = NIL
recolor_root tree@(Node left color val right)
    | color == RED = (Node left BLACK val right)
    | otherwise = tree

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

insert tree val = recolor_root (insert_helper tree val)