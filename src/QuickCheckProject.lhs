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

> newtype Age = Age (Maybe Int)
>      deriving(Show, Eq, Ord)

> data Pet = Pet Species Age (Maybe Power)
>    deriving(Show, Eq, Ord) 

> newtype Herd = Herd [Pet]
>   deriving(Show)

> newtype Land = Plot Int
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
>                                                                   then (False, pet : pets')
>                                                                   else case pet of 
>                                                                   (Pet Dragon _ _)  -> (False, pet : pets')
>                                                                   (Pet Dog _ _ )    -> (False, pet : pets')
>                                                                   (Pet Chicken _ power) -> (True, Pet Chicken (Age Nothing) power : pets')
>                                                                   _                 -> (True, pet : pets')

> bury_the_dead :: [Pet] -> [Pet] 
> bury_the_dead = foldr (\(Pet s (Age a) p) pets -> if a == Nothing then pets else Pet s (Age a) p : pets) [] 

> contains_predator :: [Pet] -> Bool
> contains_predator = foldr (\(Pet spec _ _) p -> p || spec == Dragon || spec == Dog) False

> herd_is_ordered :: [Pet] -> Bool
> herd_is_ordered (x : y : ys) = x <= y && herd_is_ordered (y: ys)
> herd_is_ordered _ = True

> add_to_herd :: Pet -> [Pet] -> [Pet]
> add_to_herd p []                     = [p]
> add_to_herd p  (p': ps)  | p < p'    = p : p' : ps
>                          | otherwise = p': add_to_herd p ps

> bury_the_tree :: Tree -> Tree 
> bury_the_tree Empty = Empty
> bury_the_tree (Leaf (Homestead (Herd pets) land)) = Leaf (Homestead (Herd (bury_the_dead pets)) land)
> bury_the_tree (Node (Homestead (Herd pets) land) left right) = Node (Homestead (Herd (bury_the_dead pets)) land) (bury_the_tree left) (bury_the_tree right) 

> size :: Tree -> Int
> size Empty      = 0
> size (Leaf _ )  = 1
> size (Node _ left right) = 1 + size left + size right

> count_herd_size :: Tree -> Bool -> Int
> count_herd_size Empty _ = 0
> count_herd_size (Leaf (Homestead (Herd pets) _ )) all' = foldr (\(Pet _ (Age a) _) acc -> acc + if a /= Nothing || all' then 1 else 0) 0 pets
> count_herd_size (Node (Homestead (Herd pets) _) left right) all' = foldr (\(Pet _ (Age a) _) acc -> acc + if a /= Nothing || all' then 1 else 0) 0 pets + count_herd_size left all' + count_herd_size right all'

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Custom Generators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> genPower:: Gen (Maybe Power)
> genPower = oneof[fmap Just (chooseEnum(Speed, Intelligence)), return Nothing]

> genAge :: Gen Age
> genAge = do
>        age <- choose(0, 151)
>        frequency[(4, return (Age (Just age))), (1, return (Age Nothing))]

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
>       size' <- choose(1,1000)
>       return (Plot size') 

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
>       left  <- frequency[(1, genLeaf), (n, genNodeTree (n `div` 2))]
>       right <- frequency[(1, genLeaf), (n, genNodeTree (n `div` 2))]
>       return (Node value left right)

> genTree :: Gen Tree
> genTree = sized (\n -> frequency[(1, genLeaf), (1, genEmptyTree), (n, genNodeTree n)])

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Arbitrary Instances 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> instance Arbitrary Age where
>    arbitrary = genAge

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

> validate_homestead :: Homestead -> Bool
> validate_homestead (Homestead (Herd pets) (Plot size'))=  size' >= 1 && foldr (\cur prev -> prev && prop_PetAgeIsValid cur) True pets

> validate_tree :: Tree -> Bool 
> validate_tree Empty                       = True
> validate_tree (Leaf homestead)            = validate_homestead homestead
> validate_tree (Node homestead left right) = validate_homestead homestead && validate_tree left && validate_tree right 

> prop_AgeIsValid :: Age -> Bool 
> prop_AgeIsValid  (Age Nothing)     = True
> prop_AgeIsValid  (Age (Just age))  = age >= 0 && age <= 151
 
> prop_PetAgeIsValid :: Pet -> Bool
> prop_PetAgeIsValid (Pet _ age _) = prop_AgeIsValid age

> prop_HerdPetsAreValid :: Herd -> Property
> prop_HerdPetsAreValid (Herd pets) = classify (null pets) "trivial validation" $
>                                            foldr (\cur prev -> prev && prop_PetAgeIsValid cur) True pets

> prop_HerdAfterAttackIsValid:: Herd -> Property
> prop_HerdAfterAttackIsValid (Herd pets) = not (contains_predator pets) ==> 
>                                                foldr (\(Pet spec (Age age) _ ) p -> (spec /= Chicken || age == Nothing) && p) True (coyote_attack pets)

> prop_HerdAttackedHerdAfterBuryingContainsNoChickens :: Herd -> Property
> prop_HerdAttackedHerdAfterBuryingContainsNoChickens (Herd pets) =  length pets - getChickenCount pets === length (bury_the_dead (coyote_attack pets))
>                   where getChickenCount pets' = case foldr (\(Pet s (Age a) _ ) (p, acc) -> 
>                                                              (p || s == Dragon || s == Dog, 
>                                                                    acc + if (not p && s == Chicken) || a == Nothing then 1 else 0)) (False, 0) pets' of 
>                                                                          (_, count) -> count;

> prop_HerdOrdered :: Property
> prop_HerdOrdered = forAll (sized genOrderedHerd) (\(Herd pets) -> herd_is_ordered pets)

> prop_HerdOrderedAfterInsert :: Herd -> Pet -> Property
> prop_HerdOrderedAfterInsert (Herd pets) pet = herd_is_ordered pets ==> 
>                                                                classify (null pets) "trivial validation" $       
>                                                                      herd_is_ordered (add_to_herd pet pets)

> prop_HerdIsAlive :: Herd -> Property
> prop_HerdIsAlive (Herd pets) = collect (foldr (\(Pet _ (Age a) _ ) acc -> acc + if a == Nothing then 1 else 0) 0 pets) $
>                                   foldr (\(Pet _ (Age a) _) p -> p && a /= Nothing) True (bury_the_dead pets)

> prop_HomesteadIsValid :: Homestead -> Property
> prop_HomesteadIsValid (Homestead (Herd pets) (Plot size')) = classify (null pets) "trivial validation" $
>                                                             classify (length pets > 13) "excessive validation" $
>                                                             collect size' $ 
>                                                                  validate_homestead (Homestead (Herd pets) (Plot size'))

> prop_TreeIsValid:: Tree -> Property
> prop_TreeIsValid tree = collect(size tree) $
>                           validate_tree tree

> prop_BurriedTreeIsValid :: Tree -> Property
> prop_BurriedTreeIsValid tree = count_herd_size tree False === count_herd_size (bury_the_tree tree) True 