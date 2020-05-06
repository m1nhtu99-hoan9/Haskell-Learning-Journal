data Optional a = Nada | Only a 
                  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x1) Nada = Only x1
  mappend Nada (Only x1) = Only x1
  mappend (Only x1) (Only x2) = Only (mappend x1 x2)