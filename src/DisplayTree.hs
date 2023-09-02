module DisplayTree where
import Helpers
import Data.List


-- parent direction in the tree.
data ParentSide = LeftSide | RightSide | Root deriving (Show,Eq) -- no parent for the root.


-- custom pseudo zip function: if the first element in the pair is '|' keep it, otherwise keep the second value.
my_zip xs [] = xs
my_zip [] xs = xs
my_zip (x:xs) (y:ys) = current_value : my_zip xs ys
    where current_value = if x == '|' then x else y


-- process the current vertex given the position of the parent and extract the result as a string.
process_row parentDirection color value parentPosition level = foldl (my_zip) "" (((fmap ((++"|").(flip replicate ' ')) (fmap (4*) parentPosition))) ++ [(replicate (4*level) ' ') ++ special_sign ++ show value ++ "(" ++ show color ++ ")"])
                           where special_sign = case parentDirection of Root      -> "───"
                                                                        RightSide -> "└──"
                                                                        LeftSide  -> "┌──"
                                                                        

-- helper function for display. dfs + current vertex processing.
display_helper _ _ _ NIL = []
display_helper parentDirection parentPosition level (Node l color value r) = (display_helper LeftSide nextParentPosition1 (level+1) r) ++ [process_row parentDirection color value parentPosition level] ++ (display_helper RightSide nextParentPosition2 (level+1) l)
                                  where nextParentPosition1 = if parentDirection == RightSide then parentPosition ++ [level] else parentPosition
                                        nextParentPosition2 = if parentDirection == LeftSide then parentPosition ++ [level] else parentPosition


-- main function for displaying the tree.
display t = putStr $ (intercalate "\r\n" (display_helper Root [] 0 t)) ++ "\r\n"