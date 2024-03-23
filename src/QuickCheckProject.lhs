> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module QuickCheckProject where

> import Test.QuickCheck

> import Data.List as List ()

> data Species = Dog | Cat | Chicken | Horse | Fish | Cow | Dragon | Whale
>        deriving(Show)

> data Pet = Pet Species (Maybe Int)
>       deriving(Show)

> data Herd = Herd [Pet]
>   deriving(Show)

> data Land = Plot Int
>   deriving(Show)

> data Homestead = Homestead Herd Land 
>   deriving(Show)

> genInt :: Gen Int
> genInt = elements [1..100]

> genAge :: Gen (Maybe Int)
> genAge = frequency([(4, fmap Just genInt), (1, return Nothing)])

> genSpecies :: Gen Species 
> genSpecies = elements[Dog, Cat, Chicken, Horse, Fish, Cow, Dragon, Whale]

> genPet :: Gen Pet
> genPet = do 
>       age <- genAge
>       species <- genSpecies
>       return (Pet species age)


> genHerd :: Gen Herd
> genHerd = 

> genLand :: Gen Land
> genLand = do
>       size <- genInt
>       return (Plot size) 
