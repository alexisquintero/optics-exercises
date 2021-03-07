{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter7.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Char (toUpper, toLower)
import Data.Tree
import Text.Read (readMaybe)
import Data.Either.Validation
import Data.List

----------------------------------------
-- 7.1 Introduction to Traversals
----------------------------------------

----------------------------------------
-- How do Traversals fit into the hieratchy?
----------------------------------------

----------------------------------------
-- A bit of Nostalgia
----------------------------------------

----------------------------------------
-- From fold to traversal
----------------------------------------

-- both :: Traversal (a, a) (b, b) a b
-- both :: Bitraversable r => Traversal (r a a) (r b b) a b

both1 = ("Bubbles", "Buttercup") ^.. both
-- [ "Bubbles", "Buttercup" ]
both2 = ("Bubbles", "Buttercup") & both %~ (++ "!")
-- ("Bubbles!", "Buttercup!")
both3 = ("Bubbles", "Buttercup") & both .~ "Blossom"
-- ("Blossom", "Blossom")
both4 = ("Bubbles", "Buttercup") & both %~ length
-- (7, 9)
each1 = (1, 2, 3) & each %~ (*10)
-- (10, 20, 30)
each2 = [1, 2, 3] & each %~ (*10)
-- [10, 20, 30]
each3 = "Here's Johnny" & each %~ toUpper
-- "HERE'S JOHNNY"
-- each4 = ("Houston we have a problem" :: T.Text) & each .~ (22 :: Int)
traversed1 = [1, 2, 3, 4, 5] & taking 3 traversed *~ 10
-- [ 10, 20, 30, 4, 5 ]
traversed2 = [1, 2, 3, 4, 5] & dropping 3 traversed *~ 10
-- [ 1, 2, 3, 40, 50 ]
traversed3 = "once upon a time - optics became mainstream"
               & takingWhile (/= '-') traversed
               %~ toUpper
-- "ONCE UPON A TIME - optics became mainstream"
traversed4 = [1, 2, 3, 4, 5]
               & traversed . filtered even
               *~ 10
-- [ 1, 20, 3, 40, 5 ]
traversed5 = ("short", "really long")
               & both . filtered ((> 5) . length)
               %~ reverse
-- ("short", "gnol yllaer")

----------------------------------------
-- 7.2 Traversal Combinators
----------------------------------------

----------------------------------------
-- Traversing each element of a container
----------------------------------------

-- error = [1, 2, 3] & folded %~ (*10)

-- traversed :: Traversable f => Traversal (f a) (f b) a b
-- traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b

traversed6 = [1, 2, 3] & traversed *~ 10
-- [ 10, 20, 30 ]
traversed7 = ("Batman", "Superman") & traversed %~ take 3
-- ("Batman", "Sup")
powerLevels = M.fromList
  [ ("Gohan", 710)
  , ("Goku", 9001)
  , ("Krillin", 5000)
  , ("Picollo", 408)
  ]
traversed8 = powerLevels
               & traversed
               %~ \n -> if n > 9000 then "Over 9000" else show n
-- fromList
--  [ ("Gohan", "710")
--  , ("Goku", "Over 9000")
--  , ("Krillin", "5000")
--  , ("Picollo", "408")
--  ]
opticsTree = Node "Lens" [Node "Fold" [], Node "Traversal" []]
traversed9 = opticsTree & traversed %~ reverse
-- Node "sneL" [Node "dloF" [], Node "lasrevarT" []]

----------------------------------------
-- More Combinators
----------------------------------------

-- worded :: Traversal' String String
-- lined :: Traversal' String String
--
-- worded :: Applicative f => IndexedLensLike' int f String String
-- lined :: Applicative f => IndexedLensLike' int f String String

worded1 = "I'll be back!" ^.. worded
-- [ "I'll", "be", "back!" ]
lined1 = "Run\nForrest\nRun" ^.. lined
-- [ "Run", "Forrest", "Run" ]
worded2 = "blue suede shoes" & worded %~ \s -> "*" ++ s ++ "*"
-- "*blue* *suede* *shoes*"
worded3 = "blue suede shoes" & worded %~ \(x:xs) -> toUpper x : xs
-- "Blue Suede Shoes"
lined2 = "blue\nsuede\nshoes" & lined %~ ("#" ++)
-- "#blue\n#suede\n#shoes"
worded4 = "blue \n suede \n \n shoes" & worded %~ id
-- "blue suede shoes"

----------------------------------------
-- Traversing multiple paths at once
----------------------------------------

-- beside :: Traversal s t a b
--        -> Traversal s' t' a b
--        -> Traversal (s, s') (t, t') a b
--
-- beside :: Lens s t a b -> Lens s' t' a b -> Traversal (s, s') (t, t') a b
-- beside :: Fold s a     -> Fold s' a      -> Fold (s, s') a

dinos = ("T-Rex", (42, "Stegosaurus"))
beside1 = dinos ^.. beside id _2
-- [ "T-Rex", "Stegosaurus" ]
numbers = ([(1, 2), (3, 4)], [5, 6, 7])
beside2 = numbers ^.. beside (traversed . both) traversed
-- [1, 2, 3, 4, 5, 6, 7]
beside2' = numbers ^.. beside (folded . both) folded
-- [1, 2, 3, 4, 5, 6, 7]
dinos2 = ("T-Rex", ("Ankylosaurus", "Stegosaurus"))
beside3 = dinos2 ^.. beside id both
-- [ "T-Rex", "Ankylosaurus", "Stegosaurus" ]

-- both = beside id id

beside4 = ("Cowabunga", ["let's", "order", "pizza"])
            & beside traversed (traversed . traversed)
            %~ toUpper
-- ("COWABUNGA", ["LET'S", "ORDER", "PIZZA"])

-- beside both traversed :: Traversal' (Either (Int, Int) [Int]) Int

beside5 = (Left (1, 2) :: Either (Int, Int) [Int])
            & beside both traversed
            %~ negate
-- Left (-1, -2)
beside6 = (Right [3, 4] :: Either (Int, Int) [Int])
            & beside both traversed
            %~ negate
-- Right [-3, -4]

----------------------------------------
-- Focusing a specific traversal element
----------------------------------------

-- element :: Traversable f => Int -> Traversal' (f a) a

element1 = [0, 1, 2, 3, 4] ^? element 2
-- Just 2
element2 = [0, 1, 2, 3, 4] & element 2 *~ 100
-- [0, 1, 200, 3, 4]

-- elementOf :: Traversal' s a -> Int -> Traversal' s a
-- elementOf :: Fold s a       -> Int -> Fold s a

elementOf1 = [0, 1, 2, 3, 4] ^? elementOf traversed 2
-- Just 2
elementOf2 = [[0, 1, 2], [3, 4], [5, 6, 7, 8]]
               ^? elementOf (traversed . traversed) 6
-- Just 6
elementOf3 = [[0, 1, 2], [3, 4], [5, 6, 7, 8]]
               & elementOf (traversed . traversed) 6 *~ 100
-- [[0, 1, 2], [3, 4], [5, 600, 7, 8]]

----------------------------------------
-- 7.3 Traversal Composition
----------------------------------------

tComp1 = "blue suede shoes" & worded . taking 1 traversed %~ toUpper
-- "Blue Suede Shoes"
tComp2 = ["short", "really long"]
           & traversed
           . filtered ((> 5) . length)
           . worded
           %~ \s -> "*" ++ s ++ "*"
-- ["short", "*really* *long*"]
tcomp3 = (("Ritchie", 100000), ("Archie", 32), ("Reggie", 4350))
           & each
           . filtered ((> 1000) . snd)
           . _1
           %~ ("Rich " ++)
-- (("Rich Ritchie", 100000)

----------------------------------------
-- 7.4 Traversal Actions
----------------------------------------

----------------------------------------
-- A Primer on Traversable
----------------------------------------

-- sequenceA :: (Traversable t, Applicative f)
--           => t (f a) -> f (t a)

sequenceA1 = sequenceA [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
sequenceA2 = sequenceA [Just 1, Nothing, Just 3]
-- Nothing
sequenceA3 = sequenceA $ Just (Left "Whoops")
-- Left "Whoops"
sequenceA4 =
  sequenceA ([pure 1, pure 2, pure 3] :: [IO Int]) >>= print
-- [1, 2, 3]

-- traverse :: (Traversable t, Applicative f)
--          => (a -> f b) -> t a -> f (t b)
-- traverse f t = sequenceA (fmap f t)

traverse1 = traverse readMaybe ["1", "2", "3"] :: Maybe [Int]
-- Just [1, 2, 3]
traverse2 = traverse readMaybe ["1", "snark", "3"] :: Maybe [Int]
-- Nothing
traverse3 = traverse readFile ["file1.txt", "file2.txt"]
-- IO ["file 1 contents", "file 2 contents"]
traverse4 = traverse (\n -> [n * 10, n * 100]) ("a", 10)
-- [("a", 100), ("a", 1000)]

----------------------------------------
-- Traverse on Traversals
----------------------------------------

-- traverseOf :: Traversal s t a b -> (a -> f b) -> s -> f t
-- traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t

traverseOf1 :: Maybe (Int, Int)
traverseOf1 = traverseOf both readMaybe ("1", "2")
-- Just (1, 2)
traverseOf2 :: Maybe (Int, Int)
traverseOf2 = traverseOf both readMaybe ("not a number", "2")
-- Nothing
traverseOf3 =
  traverseOf both (\c -> [toLower c, toUpper c]) ('a', 'b')
-- [('a', 'b'), ('a', 'B'), ('A', 'b'), ('A', 'B')]
traverseOf4 = traverseOf
                (both . traversed)
                (\c -> [toLower c, toUpper c])
                ("ab", "cd")
-- [ ("ab","cd"),("ab","cD"),("ab","Cd"),("ab","CD")
-- , ("aB","cd"),("aB","cD"),("aB","Cd"),("aB","CD")
-- , ("Ab","cd"),("Ab","cD"),("Ab","Cd"),("Ab","CD")
-- , ("AB","cd"),("AB","cD"),("AB","Cd"),("AB","CD")
-- ]

validateEmail :: String -> Either String String
validateEmail email | elem '@' email = Right email
                    | otherwise = Left ("missing '@': " <> email)

taverseOf5 = traverseOf (traversed . _2) validateEmail
               [ ("Mike", "mike@tmnt.io")
               , ("Raph", "raph@tmnt.io")
               , ("Don", "don@tmnt.io")
               , ("Leo", "leo@tmnt.io")
               ]
-- Right [ ("Mike", "mike@tmnt.io")
--       , ("Raph", "raph@tmnt.io")
--       , ("Don", "don@tmnt.io")
--       , ("Leo", "leo@tmnt.io")
--       ]
taverseOf6 = traverseOf (traversed . _2) validateEmail
               [ ("Mike", "mike@tmnt.io")
               , ("Raph", "raph@tmnt.io")
               , ("Don", "don@tmnt.io")
               , ("Leo", "leo@tmnt.io")
               ]
-- Left "missing '@': raph.io"

validateEmail' :: String -> Validation [String] String
validateEmail' email | elem '@' email = Success email
                     | otherwise = Failure ["missing '@': " <> email]

traverseOf5 = traverseOf (both . traversed) validateEmail'
                ( ["mike@tmnt.io", "raph@tmnt.io"]
                , ["don@tmnt.io", "leo@tmnt.io"]
                )
-- Success (["mike@tmnt.io", "raph@tmnt.io"] , ["don@tmnt.io", "leo@tmnt.io"])
traverseOf6 = traverseOf (both . traversed) validateEmail'
                ( ["mike@tmnt.io", "raph.io"]
                , ["don@tmnt.io", "leo.io"]
                )
-- Failure ["missing '@': raph.io", "missing '@': leo.io"]

-- forOf :: Traversal s t a b -> s -> (a -> f b) -> f t

sequenceAOf1 = sequenceAOf _1 (Just "Garfield", "Lasagna")
-- Just ("Garfield", "Lasagna")
sequenceAOf2 = sequenceAOf _1 (Nothing , "Lasagna")
-- Nothing
sequenceAOf3 = sequenceAOf (both . traversed)
                           ([Just "apples"], [Just "oranges"])
-- Just (["apples"], ["oranges"])
sequenceAOf4 = sequenceAOf (both . traversed)
                           ([Just "apples"], [Nothing])
-- Nothing

----------------------------------------
-- Infix traverseOf
----------------------------------------

-- traverseOf :: Traversal s t a b -> (a -> f b) -> s -> f t
-- (%%~)      :: Traversal s t a b -> (a -> f b) -> s -> f t

traverseOf7 :: Maybe (Int, Int)
traverseOf7 = ("1", "2") & both %%~ readMaybe
-- Just (1, 2)
traverseOf8 :: Maybe (Int, Int)
traverseOf8 = ("not a number", "2") & both %%~ readMaybe
-- Nothing

----------------------------------------
-- Using Traversals directly
----------------------------------------

directly1 :: Maybe (Int, Int)
directly1 = both readMaybe ("1", "2")

----------------------------------------
-- 7.5 Custom traversals
----------------------------------------

-- type LensLike f s t a b = (a -> f b) -> (s -> f t)
-- traverse :: (Traversable g, Applicative f)
--          => (a -> f b) -> (g a -> f (g b))
-- s = g a ; t = g b
-- myTraversal :: (Applicative f)
--             => (a -> f b) -> (s -> f t)
-- myLensLike :: (a -> f b) -> (s -> f t)
-- type Lens s t a b
--   = forall f. Functor f => LensLike f s t a b
-- type Traversal s t a b
--   = forall f. Applicative f => LensLike f s t a b
-- type Fold s a
--   = forall f. (Contravariant f, Applicative f) => LensLike f s t a b

----------------------------------------
-- Our first custom traversal
----------------------------------------

-- values :: Traversal [a] [b] a b
values :: Applicative f => (a -> f b) -> [a] -> f [b]
values _ [] = pure []
values handler (a:as) =
  liftA2 (:) (handler a) (values handler as)

values1 = ["one", "two", "three"] ^.. values
-- ["one", "two", "three"]
values2 = ["one", "two", "three"] & values %~ reverse
-- ["eno", "owt", "eerht"]
values3 = ["one", "two", "three"] & values %~ length
-- [3, 3, 5]

----------------------------------------
-- Traversals with custom logic
----------------------------------------

data Transaction =
    Withdrawal { _amount :: Int }
  | Deposit { _amount :: Int }
  deriving Show
makeLenses ''Transaction

-- amount :: Lens' Transaction Int

newtype BankAccount =
  BankAccount { _transactions :: [Transaction] }
  deriving Show
makeLenses ''BankAccount

-- transactions :: Lens' BankAccount [Transaction]
-- transactions :: Iso' BankAccount [Transaction]

aliceAccount =
  BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]

transactions1 = aliceAccount ^.. transactions . traversed
-- [Deposit {_amount = 100}, Withdrawal {_amount = 20}, Withdrawal {_amount = 10}]

----------------------------------------
-- Case Study: Transaction Traversal
----------------------------------------

-- deposits :: Traversal' [Transaction] Int
-- deposits :: Traversal [Transaction] [Transaction] Int Int
deposits :: Applicative f
         => (Int -> f Int) -> [Transaction] -> f [Transaction]
deposits _ [] = pure []
deposits handler (Withdrawal amt : rest) =
  liftA2 (:) (pure (Withdrawal amt)) (deposits handler rest)
deposits handler (Deposit amt : rest) =
  liftA2 (:) (Deposit <$> (handler amt)) (deposits handler rest)

depositsItems = [Deposit 10, Withdrawal 20, Deposit 30]
deposits1 = depositsItems ^.. deposits
-- [10, 30]
deposits2 = depositsItems & deposits *~ 10
-- [Deposit {_amount = 100}, Withdrawal {_amount = 20}, Deposit {_amount = 300}]

isDeposit :: Transaction -> Bool
isDeposit (Deposit _) = True
isDeposit _           = False

badDeposits :: Traversal' [Transaction] Int
badDeposits handler ts = traverse go (filter isDeposit ts)
  where go (Deposit amt) = Deposit <$> handler amt
        go (Withdrawal _) = error "This shouldn't happen"

badDeposits1 = depositsItems ^.. badDeposits
-- [10, 30]
badDeposits2 = depositsItems & badDeposits *~ 10
-- [Deposit {_amount = 100}, Deposit {_amount = 300}]

deposits' :: Traversal' [Transaction] Int
deposits' = traversed . filtered isDeposit . amount

----------------------------------------
-- 7.6 Traversal Laws
----------------------------------------

----------------------------------------
-- Law one: Respect Purity
----------------------------------------

-- traverseOf myTraversal pure x == pure x

lawOneData = ("don't", "touch")
lawOne1 = traverseOf both pure lawOneData :: [(String, String)]
-- [("don't", "touch")]
lawOne1' = pure ("don't", "touch") :: [(String, String)]
-- [("don't", "touch")]
lawOne2 = traverseOf both pure lawOneData :: Maybe (String, String)
-- Just ("don't", "touch")
lawOne2' = pure ("don't", "touch") :: Maybe (String, String)
-- Just ("don't", "touch")

badTupleSnd :: Traversal (Int, a) (Int, b) a b
badTupleSnd handler (n, a) =
  liftA2 (,) (pure (n + 1)) (handler a)

lawOne3 = traverseOf badTupleSnd pure (10, "Yo") :: [(Int, String)]
-- [(11, "yo")]
lawOne3' = pure (10, "Yo") :: [(Int, String)]
-- [(10, "yo")]

----------------------------------------
-- Law Two: Consistent Focuses
----------------------------------------

-- fmap (traverseOf myTrav f) . traverseOf myTrav g $ x
-- ==
-- getCompose . traverseOf myTrav (Compose . fmap f . g) $ x

-- x & myTraversal %~ f
--   & myTraversal %~ g
-- ==
-- x & myTraversal %~ (g . f)

lawTwo1 = (0, 0) & both %~ (+10)
                 & both %~ (*10)
-- (100, 100)
lawTwo1' = (0, 0) & both %~ (*10) . (+10)
-- (100, 100)

lawTwo2 = 2 & filtered even %~ (+1)
            & filtered even %~ (+10)
-- 3
lawTwo2' = 2 & filtered even %~ (+10) . (+1)
-- 30

----------------------------------------
-- Good Traversal Bad Traversal
----------------------------------------

----------------------------------------
-- 7.7 Advanced manipulatin
----------------------------------------

----------------------------------------
-- partsOf
----------------------------------------

-- partsOf :: Traversal' s a -> Lens' s [a]

partsOf1 = [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _1)
-- "abc"
partsOf2 = [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _2)
-- [1,2,3]
partsOf3 = [('a', 1), ('b', 2), ('c', 3)]
             & partsOf (traversed . _1)
             .~ ['c', 'a', 't']
-- [('c', 1), ('a', 2), ('t', 3)]
partsOf4 = [('a', 1), ('b', 2), ('c', 3)]
             & partsOf (traversed . _1)
             .~ ['l', 'e', 'o', 'p', 'a', 'r', 'd']
-- [('l', 1), ('e', 2), ('o', 3)]
partsOf5 = [('a', 1), ('b', 2), ('c', 3)]
             & partsOf (traversed . _1)
             .~ ['x']
-- [('x', 1), ('b', 2), ('c', 3)]
partsOf6 = [('a', 1), ('b', 2), ('c', 3)]
             & partsOf (traversed . _1)
             %~ reverse
-- [('c', 1), ('b', 2), ('a', 3)]
partsOf7 = [('o', 1), ('o', 2), ('f', 3)]
             & partsOf (traversed . _1)
             %~ sort
-- [('f', 1), ('o', 2), ('o', 3)]
partsOf8 = [('o', 1), ('o', 2), ('f', 3)]
             & partsOf (traversed . _1)
             %~ tail
-- [('o', 1), ('f', 2), ('f', 3)]
partsOf9 = ("how is a raven ", "like a ", "writing desk")
             & partsOf (each . traversed)
             %~ unwords . sort . words
-- ("a a desk how is", " like r", "aven writing")
partsOf10 = ("abc", "def") ^.. partsOf each . traversed
-- ["abc", "def"]
partsOf11 = ("abc", "def") ^.. partsOf (each . traversed)
-- ["abcdef"]
partsOf12 = [('a', 1), ('b', 2), ('c', 3)]
              & partsOf (traversed . _2)
              %~ \xs -> (/ sum xs) <$> xs
-- [('a', 0.16666), ('b', 0.33333), ('c', 0.5)]

----------------------------------------
-- Polymorphic partsOf
----------------------------------------

-- unsafePartsOf :: Traversal s t a b -> Lens s t [a] [b]

unsafePartsOf1 = [('a', 1), ('b', 2), ('c', 3)]
                   & unsafePartsOf (traversed . _1) .~ [True, False]
-- [ (True, 1)
-- , (False, 2)
-- , ( *** Exception: unsafePartsOf': not enough elements were supplied
unsafePartsOf2 = [('a', 1), ('b', 2), ('c', 3)]
                   & unsafePartsOf (traversed . _1) .~ [True, False, True]
-- [(True, 1) , (False, 2) , (True, 3)]
unsafePartsOf3 = [('a', 1), ('b', 2), ('c', 3)]
                   & unsafePartsOf (traversed . _1)
                   %~ \xs -> zipWith (,) xs ((Just <$> tail xs) ++ [Nothing])
-- [(('a', Just 'b'), 1), (('b', Just 'c'), 2), (('c', Nothing), 3)]

----------------------------------------
-- partsOf and other data structures
----------------------------------------

-- usersIds :: Tree UserId
-- lookupUsers :: [UserId] -> IO [User]
-- lookupUsers = error ""

-- treeLookup :: Tree UserId -> IO (Tree User)
-- treeLookup = ???
-- treeLookup = traverseOf (unsafePartsOf traversed) lookupUsers
