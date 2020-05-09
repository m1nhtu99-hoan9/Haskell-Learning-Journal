{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- EXERCISE: WRITE FUNCTOR INSTANCES
--    QUESTION 3
newtype Flip t b a = Flip (t a b) 
                     deriving (Show, Eq)
newtype Konst a b = Konst a 
                    deriving (Show, Eq)

instance Functor (Flip Konst b) where
  fmap f (Flip (Konst a)) = Flip (Konst (f a))

getFlipKonstVal :: (Show a) => Flip Konst b a -> a
getFlipKonstVal (Flip (Konst x)) = x

--    QUESION 5
data LiftOut f a = LiftOut (f a)
                   deriving (Show, Eq)
instance Functor f => Functor (LiftOut f) where 
  fmap f (LiftOut fa) = LiftOut (fmap f fa) 

--    QUESTION 6 
data Parappa f g a = DaWrappa (f a) (g a)
                     deriving (Show, Eq)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

--    QUESTION 7
data IgnoreOne f a g b = IgnoreSth (f a) (g b)
                         deriving (Show, Eq)
instance (Functor g) => Functor (IgnoreOne f a g) where 
  fmap f (IgnoreSth fa gb) = IgnoreSth fa (fmap f gb)

--    QUESTION 8
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where 
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

--    QUESTION 9  
data List a = 
    Nil
  | Cons a (List a)
  deriving (Show, Eq)
instance Functor List where 
  fmap f (Cons x y) = Cons (f x) (fmap f y)
  fmap f Nil = Nil 
  
--    QUESION 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Show, Eq)
instance Functor GoatLord where 
  fmap f (MoreGoats x1 x2 x3) 
                     = MoreGoats (fmap f x1) (fmap f x2) (fmap f x3)
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f NoGoat      = NoGoat

--    QUESTION 11
data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)
instance Functor TalkToMe where 
  fmap f (Read g)    = Read (f . g)  
  fmap f (Print s x) = Print s (f x)
  fmap _ Halt        = Halt