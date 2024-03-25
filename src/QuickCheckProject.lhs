> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module QuickCheckProject where

> import Test.QuickCheck
> import Control.Monad (liftM2)
> import Data.List (sort)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Custom Data Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> data Species = Dog | Cat | Chicken | Horse | Fish | Cow | Dragon | Whale
>    deriving(Show, Eq, Enum, Ord)

> data Power = Speed | Strength | Growth | Intelligence
>           deriving(Show, Eq, Enum, Ord)

> data Pet = Pet Species (Maybe Int) (Maybe Power)
>    deriving(Show, Eq, Ord) 

> data Herd = Herd [Pet]
>   deriving(Show)

> data Land = Plot Int
>   deriving(Show)

> data Homestead = Homestead Herd Land 
>   deriving(Show) 

> data Tree = Leaf Homestead | Node Homestead Tree Tree | Empty
>     deriving (Show)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functions For Testing 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> coyote_attack :: [Pet] -> [Pet] 
> coyote_attack pets = case foldr attack (True, []) pets of
>                                (_, pets') -> pets'
>                                where attack pet (present, pets') = if not present
>                                                                  then (False, pet : pets')
>                                                                  else case pet of 
>                                                                   (Pet Dragon _ _)  -> (False, pet : pets')
>                                                                   (Pet Dog _ _ )    -> (False, pet : pets')
>                                                                   (Pet Chicken _ power) -> (True, Pet Chicken Nothing power : pets')
>
>                                                                   _                 -> (True, pet : pets')

> contains_predator :: [Pet] -> Bool
> contains_predator = foldr (\(Pet spec _ _) p -> p && spec /= Dragon && spec /= Dog) True

> herd_is_ordered :: [Pet] -> Bool
> herd_is_ordered (x : y : ys) = x <= y && herd_is_ordered (y: ys)
> herd_is_ordered _ = True

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Custom Generators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> genPower:: Gen (Maybe Power)
> genPower = oneof[fmap Just (chooseEnum(Speed, Intelligence)), return Nothing]

> genAge :: Gen (Maybe Int)
> genAge = frequency[(4, fmap Just (choose(0, 100))), (1, return Nothing)]

> genSpecies :: Gen Species 
> genSpecies = chooseEnum(Dog, Whale)

> genPet :: Gen Pet
> genPet = do 
>       age <- genAge
>       species <- genSpecies
>       power <- genPower
>       return (Pet species age power)

> genPetList:: Int -> Gen [Pet]
> genPetList n = frequency[(1, return []), (n, liftM2 (:) genPet (genPetList(n `div` 2)))]

> genHerd :: Gen Herd
> genHerd = do
>       pets <- sized genPetList
>       return (Herd pets)

> genOrderedHerd :: Int -> Gen Herd
> genOrderedHerd n = do
>                list <- genPetList n
>                return (Herd (sort list))   
        
> genLand :: Gen Land
> genLand = do
>       size <- choose(1,100000)
>       return (Plot size) 

> genHomestead :: Gen Homestead
> genHomestead = do 
>       herd <- genHerd
>       land <- genLand
>       return (Homestead herd land)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Custom Generator for Binary Tree
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> genEmptyTree:: Gen Tree
> genEmptyTree = return Empty

> genLeaf :: Gen Tree
> genLeaf = do 
>           homestead <- genHomestead
>           return (Leaf homestead)

> genNodeTree :: Int -> Gen Tree
> genNodeTree n = do
>       value <- genHomestead
>       left  <- frequency[(1, genLeaf), (1, genEmptyTree), (n, genNodeTree (n `div` 2))]
>       right <- frequency[(1, genLeaf), (1, genEmptyTree), (n, genNodeTree (n `div` 2))]
>       return (Node value left right)

> genTree :: Gen Tree
> genTree = sized (\n -> frequency[(1, genLeaf), (1, genEmptyTree), (n, genNodeTree n)])

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Arbitrary Instances 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> instance Arbitrary Species where
>    arbitrary = genSpecies

> instance Arbitrary Pet where
>    arbitrary = genPet

> instance Arbitrary Herd where
>    arbitrary = genHerd

> instance Arbitrary Land where
>    arbitrary = genLand

> instance Arbitrary Homestead where
>    arbitrary = genHomestead

> instance Arbitrary Tree where
>     arbitrary = genTree


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Properties
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> prop_PetAgeIsValid :: Pet -> Bool
> prop_PetAgeIsValid (Pet _ age _) = maybe True (\a -> a >= 0 && a <= 100) age

> prop_HerdPetsAreValid :: Herd -> Bool
> prop_HerdPetsAreValid (Herd pets) = foldr (\cur prev -> prev && prop_PetAgeIsValid cur) True pets

> prop_HerdAfterAttackIsValid:: Herd -> Property
> prop_HerdAfterAttackIsValid (Herd pets) = 
>       contains_predator pets ==> 
>       foldr (\(Pet spec age _ ) p -> (spec /= Chicken || age == Nothing) && p) True (coyote_attack pets)

> prop_IsHerdOrdered:: Property
> prop_IsHerdOrdered = forAll (sized genOrderedHerd) (\(Herd pets) -> herd_is_ordered pets)

> prop_HomesteadIsValid :: Homestead -> Bool
> prop_HomesteadIsValid (Homestead herd (Plot size)) = size >= 1 && prop_HerdPetsAreValid herd

> prop_TreeIsValid:: Tree -> Bool
> prop_TreeIsValid Empty                       = True
> prop_TreeIsValid (Leaf homestead)            = prop_HomesteadIsValid homestead
> prop_TreeIsValid (Node homestead left right) = prop_HomesteadIsValid homestead && prop_TreeIsValid left && prop_TreeIsValid right 

