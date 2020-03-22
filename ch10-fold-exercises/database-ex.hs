module DatabaseElement_Processing where
  import Data.Time 
  import Data.Ord
  import Data.List
  
  data DatabaseElement = Name      String 
                       | Number    Integer
                       | DateValue UTCTime
                       deriving (Eq, Ord, Show)

  filterDbDate :: [DatabaseElement] -> [UTCTime]
  filterDbDate = foldr exec [] 
    where exec (DateValue a) acc = a : acc
          exec _ acc             = acc

  mostRecent :: [DatabaseElement] -> UTCTime
  mostRecent = head . sortOn Down . filterDbDate 

  filterDbNumber :: [DatabaseElement] -> [Integer]
  filterDbNumber = foldr exec []
    where exec (Number n) acc = n : acc
          exec _ acc          = acc

  dbCollection :: [DatabaseElement]  
  dbCollection = 
    [
      DateValue (UTCTime (fromGregorian 2019 3 30) 
                         (secondsToDiffTime 34123)
      ), 
      Number 9001, 
      Name "Hello the motherfucking world of DatabaseElement",
      Number 300399,
      DateValue (UTCTime (fromGregorian 2019 3 30)
                         (secondsToDiffTime 34120)
      )
    ]
  
  {--
  `filterDbDate dbCollection` expects `[2019-03-30 09:28:43 UTC,2019-03-30 09:28:40 UTC]`
  --}