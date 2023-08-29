module DisplayTree where
import Data.List

-- TODO: change <$> for map
-- TODO: change . for for (f())

data Color = RED | BLACK deriving (Show, Eq) 
data Tree a = NIL | Node (Tree a) Color a (Tree a) | DoubleBlackNode (Tree a) deriving (Show,Eq)
data ParentSide = LeftSide | RightSide | Root deriving (Show,Eq) -- no parent for the root

my_zip xs [] = xs
my_zip [] xs = xs
my_zip (x:xs) (y:ys) = current_value : my_zip xs ys
    where current_value = if x == '|' then x else y

process_row parentDirection color value parentPosition level = foldl (my_zip) "" (((++"|").(flip replicate ' ') <$> (4*) <$> parentPosition) ++ [(replicate (4*level) ' ') ++ special_sign ++ show value ++ "(" ++ show color ++ ")"])
                           where special_sign = case parentDirection of Root      -> "───"
                                                                        RightSide -> "└──"
                                                                        LeftSide  -> "┌──"
                                                                        

display_helper _ _ _ NIL = []
display_helper parentDirection parentPosition level (Node l color value r) = (display_helper LeftSide nextParentPosition1 (level+1) r) ++ [process_row parentDirection color value parentPosition level] ++ (display_helper RightSide nextParentPosition2 (level+1) l)
                                  where nextParentPosition1 = if parentDirection == RightSide then parentPosition ++ [level] else parentPosition
                                        nextParentPosition2 = if parentDirection == LeftSide then parentPosition ++ [level] else parentPosition

display t = putStr $ (intercalate "\r\n" (display_helper Root [] 0 t)) ++ "\r\n"