> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module QuickCheckProject where

> import Test.QuickCheck(Arbitrary(..),Gen,Property,OrderedList(..),Testable(..),
>                         forAll,frequency,sized,oneof,label, listOf,
>                         quickCheck, verboseCheck,elements,sample)

> import Data.List as List ()

> data Species = Dog | Cat | Chicken | Horse | Fish | Cow | Dragon | Whale
>    deriving(Show)

> data Pet = Pet Species (Maybe Int)
>    deriving(Show)

> data Herd = Herd [Pet]
>   deriving(Show)

> data Land = Plot Int
>   deriving(Show)

> data Homestead = Homestead Herd Land 
>   deriving(Show)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Applying knowledge about monads and the Gen data type. Deriving new generators 
from existing ones for composite data types. 
Generators and data types below.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
> genHerd = do
>       pets <- listOf genPet
>       return (Herd pets)

> genLand :: Gen Land
> genLand = do
>       size <- genInt
>       return (Plot size) 

> genHomestead :: Gen Homestead
> genHomestead = do 
>       herd <- genHerd
>       land <- genLand
>       return (Homestead herd land)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generator for basic Binary Tree. Not sure if a generator for normal BST or BST defined
by us is required. FIXME
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> data BST a = Empty | Node a (BST a) (BST a)
>     deriving (Show)

> genEmptyTree :: Gen (BST a)
> genEmptyTree = return Empty

> genNodeTree :: Gen a -> Gen (BST a)
> genNodeTree genA = do
>       value <- genA
>       left <- genBinaryTree genA
>       right <- genBinaryTree genA
>       return (Node value left right)

> genBinaryTree :: Gen a -> Gen (BST a)
> genBinaryTree genA = frequency [(1, genEmptyTree),(3, genNodeTree genA)]


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generating a BST using new composite data types. Uses genHomestead to generated nodes in a BST. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

> insertHomestead :: Homestead -> BST Homestead -> BST Homestead
> insertHomestead h Empty = Node h Empty Empty
> insertHomestead h (Node h' left right)
>    | landSize h < landSize h' = Node h' (insertHomestead h left) right
>    | otherwise = Node h' left (insertHomestead h right)
>  where
>    landSize (Homestead _ (Plot size)) = size

> genBSTHomestead :: Gen (BST Homestead)
> genBSTHomestead = do
>    homesteads <- listOf genHomestead 
>    return (foldr insertHomestead Empty homesteads)


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Arbitrary instances of these values use the Gen monad to test property functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


prop_ValidHomestead :: Homestead -> Bool
prop_ValidHomestead homestead = validateHomestead homestead

Property for genPet: Every generated Pet should have an age within the bounds you've defined (1 to 100) or be Nothing.

> prop_PetAgeIsValid :: Pet -> Bool
> prop_PetAgeIsValid (Pet _ age) = maybe True (\a -> a >= 1 && a <= 100) age

Property for genHerd: The generated Herd should only contain Pets that satisfy the property defined for genPet.

> prop_HerdPetsAreValid :: Herd -> Bool
> prop_HerdPetsAreValid (Herd pets) = all prop_PetAgeIsValid pets

Property for genHomestead: Every Homestead must have a Plot size within the bounds you've defined (1 to 100).

> prop_HomesteadLandSizeIsValid :: Homestead -> Bool
> prop_HomesteadLandSizeIsValid (Homestead _ (Plot size)) = size >= 1 && size <= 100
