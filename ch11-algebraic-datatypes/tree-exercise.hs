module AlgebraicDataTypes.BinaryTreeADT where
  
  data BinaryTree a = Leaf 
                    | Node (BinaryTree a) a (BinaryTree a) --higher-kinded expression
                    deriving (Eq, Ord, Show)
  
  insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
  -- initiate a new tree
  insert' v Leaf                    = Node Leaf v Leaf
  {--if inserted value equals existed value, nothing happens
      else in case of LT: insert argument value on the left sub-binary tree
      else in case of GT: insert argument value on the right sub-binary tree
  --}   
  insert' v (Node btLeft x btRight) = case compare v x of
    EQ -> Node btLeft x btRight
    LT -> Node (insert' v btLeft) x btRight
    GT -> Node btLeft x (insert' v btRight)

  mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
  mapTree _ Leaf = Leaf
  mapTree f (Node btLeft x btRight) = 
    Node (mapTree f btLeft) (f x) (mapTree f btRight)
  
  testTree' :: BinaryTree Integer
  testTree' = Node (Node Leaf 3 Leaf)
                   1
                   (Node Leaf 4 Leaf)
  
  expectedTree' :: BinaryTree Integer
  expectedTree' =  Node (Node Leaf 8 Leaf)
                   2
                   (Node Leaf 16 Leaf)

  testMapTree :: IO ()
  testMapTree = 
    if mapTree (2 ^) testTree' == expectedTree'
    then putStrLn "Map Tree passed"
    else putStrLn "Map Tree failed"

  toListPreOrder :: BinaryTree Integer -> [Integer]
  toListPreOrder Leaf                    = []
  toListPreOrder (Node btLeft v btRight) = concat [[v], (toListPreOrder btLeft), (toListPreOrder btRight)]