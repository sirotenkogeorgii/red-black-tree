module DisplayTree where
import Data.List

data Color = RED | BLACK deriving (Show, Eq) 
data Tree a = NIL | Node (Tree a) Color a (Tree a) deriving (Show) 
data ParentSide = LeftSide | RightSide | Root deriving (Show,Eq) -- no parent for the root

my_zip xs [] = xs
my_zip [] xs = xs
my_zip (x:xs) (y:ys) = current_value : my_zip xs ys
    where current_value = if x == '|' then x else y

process_row parentDirection color value parentPosition level = foldl (my_zip) "" (((++"|").(flip replicate ' ') <$> (4*) <$> parentPosition) ++ [(replicate (4*level) ' ') ++ special_sign ++ show value ++ "(" ++ show color ++ ")"])
                           where special_sign = case parentDirection of Root      -> "───"
                                                                        RightSide -> "└──"
                                                                        LeftSide  -> "┌──"
                                                                        

display_helper _ _ _ NIL = [] -- if we have an empty tree
display_helper parentDirection parentPosition level (Node l color value r) = (display_helper LeftSide nextParentPosition1 (level+1) r) ++ [process_row parentDirection color value parentPosition level] ++ (display_helper RightSide nextParentPosition2 (level+1) l)
                                  where nextParentPosition1 = if parentDirection == RightSide then parentPosition ++ [level] else parentPosition
                                        nextParentPosition2 = if parentDirection == LeftSide then parentPosition ++ [level] else parentPosition

display t = putStr $ (intercalate "\r\n" (display_helper Root [] 0 t)) ++ "\r\n"

main :: IO ()
main = do
    let tree = Node (Node (Node NIL BLACK 9 NIL) RED 12 (Node NIL BLACK 17 NIL)) BLACK 18 (Node (Node NIL BLACK 20 NIL) RED 27 (Node NIL BLACK 32 (Node NIL RED 42 NIL)))
    display tree


--     ┌──5
--     ┌──4
--     |   └──3
-- ───2
--     └──1