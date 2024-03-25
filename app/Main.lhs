> module Main (Main.main) where

> import Test.QuickCheck
> import QuickCheckProject

> main :: IO ()
> main = do
>      print "Running Property Generated Tests" 
>      print "Testing Age Validity"
>      quickCheck prop_AgeIsValid
>      print "Testing Pet Validity"
>      quickCheck prop_PetAgeIsValid
>      print "Testing Herd Validity"
>      quickCheck prop_HerdPetsAreValid
>      print "Testing Herd Validity After Attack"
>      quickCheck prop_HerdAfterAttackIsValid
>      quickCheck prop_HerdAttackedHerdAfterBuryingContainsNoChickens
>      print "Testing Ordered Herd Validity"
>      quickCheck prop_HerdOrdered
>      print "Testing Ordered Herd Validity After Insert"
>      quickCheck prop_HerdOrderedAfterInsert
>      print "Testing Homestead Validity, collecting Plot size distribution"
>      quickCheck prop_HomesteadIsValid
>      print "Testing Tree Validity, collecting Tree size distribution"
>      quickCheck prop_TreeIsValid
>      print "Testing Completed"