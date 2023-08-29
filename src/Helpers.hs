module Helpers where

data Color = RED | BLACK deriving (Show, Eq) 
data Tree a = NIL | Node (Tree a) Color a (Tree a) | DoubleBlackNode (Tree a) deriving (Show,Eq)

get_color NIL = BLACK
get_color (Node _ color _ _) = color

-- get_val NIL = NIL
get_val (Node _ _ val _) = val

get_min (Node left _ val _)
    | left /= NIL = get_min left
    | otherwise = val

has2children NIL = False
has2children (Node left _ _ right)
    | left == NIL || right == NIL = False
    | otherwise = True

get_child NIL = NIL
get_child (Node left _ _ right)
    | left == NIL = right
    | right == NIL = left
    | otherwise = NIL

different_color tree1 tree2
    | get_color tree1 == get_color tree2 = False
    | otherwise = True

recolor NIL _ = NIL
recolor (Node left _ val right) new_color = (Node left new_color val right)
recolor (DoubleBlackNode (Node left _ val right)) new_color = DoubleBlackNode (Node left new_color val right)

set_val (Node left color _ right) val = (Node left color val right)
set_val (DoubleBlackNode (Node left color _ right)) val = DoubleBlackNode (Node left color val right)