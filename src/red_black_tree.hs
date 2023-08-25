-- In addition to the requirements imposed on a binary search tree the following must be satisfied by a red–black tree:[18]

-- 1. Every node is either red or black.
-- 2. All NIL nodes (figure 1) are considered black.
-- 3. A red node does not have a red child.
-- 4. Every path from a given node to any of its descendant NIL nodes goes through the same number of black nodes.

-- (Conclusion) If a node N has exactly one child, it must be a red child, because if it were black, its NIL 
--     descendants would sit at a different black depth than N's NIL child, violating requirement 4.

-- INSERTION:
-- l1
-- l2
-- l3
-- l4
-- l5
-- l6

data Color = RED | BLACK
data Tree a = NIL | Node (Tree a) Color a (Tree a)

resore_insert (Node NIL color val NIL) = (Node NIL color val NIL)

restore_insert (Node (Node (Node left_x RED x right_x) RED p right_p) BLACK g (Node left_u RED u right_u)) = (Node (Node (Node left_x RED x right_x) BLACK p right_p) RED g (Node left_u BLACK u right_u))
restore_insert (Node (Node left_p RED p (Node left_x RED x right_x)) BLACK g (Node left_u RED u right_u)) = (Node (Node left_p BLACK p (Node left_x RED x right_x)) RED g (Node left_u BLACK u right_u))
restore_insert (Node (Node left_p RED p right_p) BLACK g (Node (Node left_x RED x right_x) RED u right_u)) = (Node (Node left_p BLACK p right_p) RED g (Node (Node left_x RED x right_x) BLACK u right_u))
restore_insert (Node (Node left_p RED p right_p) BLACK g (Node left_u RED u (Node left_x RED x right_x))) = (Node (Node left_p BLACK p right_p) RED g (Node left_u BLACK u (Node left_x RED x right_x)))

restore_insert (Node (Node (Node left_x RED x right_x) RED p right_p) BLACK g (Node left_u BLACK u right_u)) = (Node (Node left_x RED x right_x) BLACK p (Node right_p RED g (Node left_u BLACK u right_u)))
restore_insert (Node (Node left_p RED p (Node left_x RED x right_x)) BLACK g (Node left_u BLACK u right_u)) = (Node (Node left_p RED p left_x) BLACK x (Node right_x RED g (Node left_u BLACK u right_u)))
restore_insert (Node (Node left_u BLACK u right_u) BLACK g (Node left_p RED p (Node left_x RED x right_x))) = (Node (Node (Node left_u BLACK u right_u) RED g left_p) BLACK p (Node left_x RED x right_x))
restore_insert (Node (Node left_u BLACK u right_u) BLACK g (Node (Node left_x RED x right_x) RED p right_p)) = (Node (Node (Node left_u BLACK u right_u) RED g left_x) BLACK x (Node right_x RED p right_p))

restore_insert tree = tree

insert NIL val = (Node NIL RED val NIL)
insert tree@(Node left color parent_val right) val
    | parent_val > val = restore_insert (Node left color parent_val (insert right val))
    | parent_val > val = restore_insert (Node (insert left val) color parent_val right)
    | otherwise = tree



-- insert tree@(Node NIL BLACK parent_val NIL) val
--     | parent_val < val = (Node NIL BLACK parent_val (Node NIL RED val NIL))
--     | parent_val > val = (Node (Node NIL RED val NIL) BLACK parent_val NIL)
--     | otherwise = tree

