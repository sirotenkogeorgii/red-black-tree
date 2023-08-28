-- 1. Every node is either red or black.
-- 2. All NIL nodes (figure 1) are considered black.
-- 3. A red node does not have a red child.
-- 4. Every path from a given node to any of its descendant NIL nodes goes through the same number of black nodes.

-- (Conclusion) If a node N has exactly one child, it must be a red child, because if it were black, its NIL 
--     descendants would sit at a different black depth than N's NIL child, violating requirement 4.
import DisplayTree

-- data Color = RED | BLACK deriving (Show, Eq) 
-- data Tree a = NIL | Node (Tree a) Color a (Tree a) | DoubleBlackNode (Tree a) deriving (Show,Eq)
-- -- data DoubleBlackNode = Bool Tree

--TODO: probably add double black data type

get_color NIL = BLACK
get_color (Node _ color _ _) = color

recolor NIL _ = NIL
recolor (Node left _ val right) new_color = (Node left new_color val right)

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
restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node slib_left BLACK slib_val (Node r_left RED r_val r_right))) =
        Node (Node replacing_node parent_color parent_val slib_left) BLACK slib_val (Node r_left BLACK r_val r_right)

restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node (Node r_left RED r_val r_right) BLACK slib_val slib_right)) = 
        Node (Node replacing_node parent_color parent_val r_left) BLACK r_val (Node r_right BLACK slib_val slib_right)

restore_delete (Node (DoubleBlackNode replacing_node) parent_color parent_val (Node slib_left slib_color slib_val slib_right))
        | slib_color == BLACK && get_color slib_left == BLACK && get_color slib_right == BLACK = 
            DoubleBlackNode (Node replacing_node parent_color parent_val (Node slib_left RED slib_val slib_right))
        | otherwise = Node (Node replacing_node BLACK parent_val (recolor slib_left RED)) BLACK slib_val (recolor slib_right BLACK)

restore_delete tree = tree


extract_min (Node (Node left color val right) parent_color parent_val slib_branch)
    | left == NIL && color == BLACK && get_color right == BLACK =  (restore_delete (Node (DoubleBlackNode right) parent_color parent_val slib_branch), val)
    | left == NIL && color /= get_color right =  (recolor right BLACK, val)
    | otherwise = (restore_delete (Node new_tree color val right), min_val)
    where (new_tree, min_val) = extract_min left


delete NIL _ = NIL
delete tree@(Node left color node_val right) val
    | node_val < val = Node left color node_val (delete right val)
    | node_val > val = Node (delete left val) color node_val right
    | left == NIL && node_val == val = right
    | right == NIL && node_val == val = left
    | node_val == val = Node left color new_val new_right
    | otherwise = tree
    where (new_right, new_val) = extract_min right
    

-- Deletion
-- In Insertion we are solving the problem of consecutive red nodes.
-- In Deletion we are solving the problem of the number of the black nodes on the pathes.
-- 1. 
-- 2.
-- 3.
-- 4.
-- 5.
-- 6.
-- 7.
-- 8.

--                 ┌──19(BLACK)
--             ┌──18(RED)
--         ┌──17(BLACK)
--         |   |   ┌──16(BLACK)
--         |   └──15(RED)
--     ┌──14(BLACK)
--     |   └──13(RED)
--     |       └──12(BLACK)
--     |           └──11(BLACK)
-- ───10(RED)
--     └──9(BLACK)
--         └──8(BLACK)
--             └──7(BLACK)



--         ┌──16(BLACK)
--     ┌──15(BLACK)
--     |   └──13(RED)
--     |       └──12(BLACK)
--     |           └──11(BLACK)
-- ───10(RED)
--     └──9(BLACK)
--         └──8(BLACK)
--             └──7(BLACK)



-- (Node (Node (Node (Node NIL BLACK 7 NIL) BLACK 8 NIL) BLACK 9 NIL) RED 10 (Node (Node (Node (Node NIL BLACK 11 NIL) BLACK 12 NIL) RED 13 NIL) BLACK 14 (Node (Node NIL RED 15 (Node NIL BLACK 16 NIL)) BLACK 17 (Node NIL RED 18 (Node NIL BLACK 19 NIL)))))
-- (Node (Node (Node (Node NIL BLACK 11 NIL) BLACK 812 NIL) RED 13 NIL) BLACK 14 (Node (Node NIL RED 15 (Node NIL BLACK 16 NIL)) BLACK 17 (Node NIL RED 18 (Node NIL BLACK 19 NIL))))
-- (Node (Node NIL RED 15 (Node NIL BLACK 16 NIL)) BLACK 17 (Node NIL RED 18 (Node NIL BLACK 19 NIL)))
-- extract_min (Node (Node NIL RED 15 (Node NIL BLACK 16 NIL)) BLACK 17 (Node NIL RED 18 (Node NIL BLACK 19 NIL)))

-- (Node (Node (Node (Node NIL BLACK 7 NIL) BLACK 8 NIL) BLACK 9 NIL) RED 10 (Node (Node (Node (Node NIL BLACK 11 NIL) BLACK 812 NIL) RED 13 NIL) BLACK 14 (Node (Node NIL RED 15 (Node NIL BLACK 16 NIL))