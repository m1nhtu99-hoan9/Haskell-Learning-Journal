-- | A difference list is a function that given a list, returns the
-- original contents of the difference list prepended at the given list
newtype DList a = DList { unDL :: [a] -> [a] }

-- | Create a difference list containing no elements
empty :: DList a
empty = DList id
{-# INLINE empty #-}

-- | Create difference list with given single element
singleton :: a -> DList a
-- singleton = DList . (:)
singleton x = DList (x:)
{-# INLINE singleton #-}

-- | Convert a difference list back to normal list
toList :: DList a -> [a]
toList = ($[]) . unDL
{-# INLINE toList #-}

-- | Prepend a single element to a DList
infixr `cons`
cons :: a -> DList a -> DList a
cons x dxs = DList $ (x:) . unDL dxs
{-# INLINE cons #-}

-- | Append a single element to a DList
infixr `snoc`
snoc :: DList a -> a -> DList a
snoc dxs x = DList $ unDL dxs . (x:)
{-# INLINE snoc #-}

-- | Append `DList`s
append :: DList a -> DList a -> DList a
append dxs dys = DList $ unDL dxs . unDL dys
{-# INLINE append #-}