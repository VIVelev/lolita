module Walking where

import Objectify (LocalVariable (..), Program (..))

walk :: Program -> (Program -> Program) -> Program
walk p@(IntLiteral _) _ = p
walk p@(BoolLiteral _) _ = p
walk p@Nil _ = p
walk p@(LocalReference _) _ = p
walk p@(GlobalReference _) _ = p
walk (Alternative pc pt pf) f = Alternative (f pc) (f pt) (f pf)
walk (Sequence ps) f = Sequence $ map f ps
walk p@(Function {body}) f = p {body = f body}
walk (Application func args) f = Application (f func) (map f args)
walk p _ = p

-- | Make read/write of local variables read/write in boxes.
insertBox :: Program -> Program
insertBox r@(LocalReference (LocalVariable {isMutable}))
  | isMutable = BoxRead r
  | otherwise = r
insertBox f@(Function vars body) = f {body = Sequence [Sequence (boxify vars), body]}
  where
    boxify (x@LocalVariable {isMutable} : xs)
      | isMutable = BoxCreate x : boxify xs
      | otherwise = boxify xs
    boxify [] = []
insertBox p = walk p insertBox
