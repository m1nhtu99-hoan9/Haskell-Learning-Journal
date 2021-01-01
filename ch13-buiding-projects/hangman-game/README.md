# hangman-game

Deadly simple demo project following guidance of chapter 13

### Notes on setting up project with Stack

`$ stack new {projectName} simple`

## My Errata on the book

- On eBook page 883 (book page 835): `Prelude> :t Test.QuickCheck.arbitrary` instead of `Prelude> arbitrary`. _GHCi_ displays:

```Haskell
Test.QuickCheck.arbitrary
  :: Test.QuickCheck.Arbitrary.Arbitrary a =>
     Test.QuickCheck.Gen.Gen a
```

- On eBook page 884 (book page 836): similarly to the previous erratum:

```Haskell
Prelude> :t Test.QuickCheck.sample
Test.QuickCheck.sample
  :: Show a => Test.QuickCheck.Gen.Gen a -> IO ()
Prelude> :t Test.QuickCheck.sample'
Test.QuickCheck.sample'
  :: Test.QuickCheck.Gen.Gen a -> IO [a]
```
