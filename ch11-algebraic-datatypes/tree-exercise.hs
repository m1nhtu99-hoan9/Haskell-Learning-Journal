module AlgebraicDataTypes.BinaryTreeADT where
  
  data BinaryTree a = Leaf 
                    | Node (BinaryTree a) a (BinaryTree a) --higher-kinded expression
                    deriving (Eq, Ord, Show)
  
  insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
  