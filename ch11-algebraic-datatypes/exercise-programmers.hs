module IntermissionExercises.Programmer where
  data OperatingSystem = GnuPlusLinux
                       | OpenBSDPlus
                       | MacOS
                       | Windows
                       deriving (Eq, Show)

  data ProgLang = Haskell
                | JavaScript
                | Scala
                | Idris
                deriving (Eq, Show)

  data Programmer = Programmer OperatingSystem ProgLang 
                  deriving (Eq, Show)

  
  allOperatingSystems :: [OperatingSystem]
  allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlus
    , MacOS
    , Windows
    ]

  allLanguages :: [ProgLang]
  allLanguages =
    [Haskell, Scala, Idris, JavaScript]

  -- length ([Programmer x y | x <- allOperatingSystems , y <- allLanguages ])
