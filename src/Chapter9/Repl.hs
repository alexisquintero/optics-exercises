{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter9.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Data.ByteString as BS
import Data.List (stripPrefix)
import Data.Foldable (for_)
import Data.List (intercalate)

----------------------------------------
-- 9. Prisms
----------------------------------------

----------------------------------------
-- 9.1 Introduction to Prisms
----------------------------------------

----------------------------------------
-- How do Prisms fit into the hierarchy
----------------------------------------

----------------------------------------
-- Simple Pattern-Matching Prisms
----------------------------------------

-- _Left  :: Prism (Either l r) (Either l' r) l l'
-- _Right :: Prism (Either l r) (Either l r') r r'

left1 = Left "message" ^? _Left
-- Just "message"
right1 = Left "message" ^? _Right
-- Nothing
right2 = Right 42 ^? _Right
-- Just 42
left2 = Right 42 ^? _Left
-- Nothing
left3 = Left 10 & _Left +~ 5
-- Left 15
right3 = Right "howdy" & _Right %~ reverse
-- Right "ydwoh"

-- _Nothing :: Prism' (Maybe a) ()
-- _Just    :: Prism' (Maybe a) (Maybe b) a b

nothing1 = Nothing ^? _Nothing
-- Just ()
just1 = Nothing ^? _Just
-- Nothing
just2 = Just "gold" ^? _Just
-- Just "gold"
nothing2 = Just "gold" ^? _Nothing
-- Nothing
just3 = Just 20 & _Just %~ (+10)
-- Just 30

----------------------------------------
-- Checking pattern matches with prisms
----------------------------------------

-- has   :: Fold s a      -> s -> Bool
-- isn't :: Prism s t a b -> s -> Bool

has1 = has _Right (Left "message")
-- False
has2 = has _Right (Right 37)
-- True
isnt1 = isn't _Nothing (Just 12)
-- True
isnt2 = isn't _Left (Left ())
-- False

----------------------------------------
-- Generating prisms with makePrisms
----------------------------------------

type Path = [String]
type Body = String

data Request =
    Post Path Body
  | Get Path
  | Delete Path
  deriving Show

makePrisms ''Request

get1 = Get ["users"] ^? _Get
-- Just ["users"]
get2 = Get ["users"] ^? _Post
-- Nothing
get3 = Get ["users"] & _Get .~ ["posts"]
-- Get ["posts"]
post1 = Post ["users"] "name: John" ^? _Post
-- Just (["users"], "name: John")
post2 = Post ["users"] "name: John" & _Post . _1 <>~ ["12345"]
-- Post ["users", "12345"] "name: John"

----------------------------------------
-- Embedding values with prisms
----------------------------------------

-- review :: Prism s t a b -> b -> t
-- (#)    :: Prism s t a b -> b -> t

review1 = review _Get ["posts"]
-- Get ["Posts"]
review2 = _Get # ["posts"]
-- Get ["Posts"]
review3 = _Delete # ["posts"]
-- Delete["posts"]
review4 = _Post # (["posts"], "My blog post")
-- Post ["posts"] "My blog post"
review5 = review _Left "an error"
-- Left "an error"
review6 = review _Right 42
-- Right 42
review7 = _Just . _Left # 1337
-- Just (Left 1337))

----------------------------------------
-- Other types of patterns
----------------------------------------

-- class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
--  _Cons :: Prism s t (a, s) (b, t)
--
--  _Cons :: Prism  [a]        [b]        (a, [a])           (b, [b])
--  _Cons :: Prism  (Seq a)    (Seq b)    (a, Seq a)
--  _Cons :: Prism  (Vector a) (Vector b) (a, Vector a)      (b, Vector b)
--  _Cons :: Prism' String                (Char, String)
--  _Cons :: Prism' Text                  (Char, Text)
--  _Cons :: Prism' ByteString            (Word8, ByteString)

cons1 = [1, 2, 3] ^? _Cons
-- Just (1, [2, 3])
cons2 = ("Freedom!" :: String) ^? _Cons
-- Just ('F', "reedom!")
cons3 = ("" :: String) ^? _Cons
-- Nothing
cons4 = ("Freedom!" :: T.Text) ^? _Cons
-- Just ('F', "reedom!")
cons5 = ("Freedom!" :: BS.ByteString) ^? _Cons
-- Just (70, "reedom!")
cons6 = "Freedom!" & _Cons . _2 %~ reverse
-- "F!modeer"
cons7 = _Cons # ('F', ("reedom" :: String))
-- "Freedom"
cons8 = _Cons # (65, ("BC" :: BS.ByteString))
-- "ABC"

tail1 = "Freedom!" & _tail %~ reverse
-- "F!modeer"
head1 = "Hello" & _head .~ 'J' :: String
-- "Jello"

-- class AsEmpty a where
--  _Empty :: Prism' a ()

empty1 = isn't _Empty []
-- False
empty2 = isn't _Empty [1, 2, 3]
-- True
empty3 = has _Empty M.empty
-- True
empty4 = has _Empty (S.fromList [1, 2, 3])
-- False

-- _Show :: (Read a, Show a) => Prism' String a

show1 = "12" ^? _Show :: Maybe Int
-- Just 12
show2 = "12" ^? _Show :: Maybe Bool
-- Nothing
show3 = "apple pie" ^? _Show :: Maybe Int
-- Nothing
show4 = "It's True that I ate 3 apples and 5 oranges"
          ^.. worded . _Show :: [Int]
-- [3, 5]
show5 = "It's True that I ate 3 apples and 5 oranges"
          ^.. worded . _Show :: [Bool]
-- [ True ]

----------------------------------------
-- 9.2 Writing Custom Prisms
----------------------------------------

-- prism  :: (b -> t) -> (s -> Either t a) -> Prism s t a b
-- prism' :: (b -> t) -> (s -> Maybe    a) -> Prism s s a b

_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism embed match
  where
    match :: Maybe a -> Either (Maybe b) a
    match (Just a) = Right a
    match Nothing = Left Nothing
    embed :: b -> Maybe b
    embed b = Just b

-- _Nothing' :: Prism  (Maybe a) (Maybe a) () ()
_Nothing' :: Prism' (Maybe a) ()
_Nothing' = prism' embed match
  where
    match :: Maybe a -> Maybe ()
    match Nothing = Just ()
    match (Just _) = Nothing
    embed :: () -> Maybe a
    embed () = Nothing

----------------------------------------
-- Matching String Prefixes
----------------------------------------

_Prefix :: String -> Prism' String String
_Prefix prefix = prism' embed match
  where
    match :: String -> Maybe String
    match s = stripPrefix prefix s
    embed :: String -> String
    embed s = prefix <> s

prefix1 = "http://phishingscam.com" ^? _Prefix "https://"
-- Nothing
prefix2 = "https://totallylegit.com" ^? _Prefix "https://"
-- Just "totallylegit.com"

_Secure = _Prefix "https://"

secure1 = "https://mybank.com" & _Secure <>~ "?accountNumber=12345"
-- "https://mybank.com?accountNumber=12345"
secure2 = "http://fakebank.com" & _Secure <>~ "?accountNumber=12345"
-- "http://fakebank.com"

----------------------------------------
-- Cracking the coding interview: Prisms style!
----------------------------------------

_Factor :: Int -> Prism' Int Int
_Factor n = prism' embed match
  where
    embed :: Int -> Int
    embed i = i * n
    match :: Int -> Maybe Int
    match i
      | i `mod` n == 0 = Just (i `div` n)
      | otherwise = Nothing

factor1 = has (_Factor 3) 9
-- True
factor2 = isn't (_Factor 7) 9
-- True
factor3 = 15 ^? _Factor 7
-- Nothing
factor4 = 15 ^? _Factor 3 . _Factor 5
-- Just 1

prismFizzBuzz :: Int -> String
prismFizzBuzz n
  | has (_Factor 3 . _Factor 5) n = "FizzBuzz"
  | has (_Factor 3) n = "Fizz"
  | has (_Factor 5) n = "Buzz"
  | otherwise = show n

runFizzBuzz :: IO ()
runFizzBuzz = for_ [1..20] $ \n -> putStrLn (prismFizzBuzz n)

----------------------------------------
-- 9.3 Laws
----------------------------------------

----------------------------------------
-- Law One: Review-Preview
----------------------------------------

-- preview p (review p value) == Just value
lawone1 = preview _Left (review _Left "Jabberwocky") == Just "Jabberwocky"
-- True
lawone2 = preview _Just (review _Just "Jabberwocky") == Just "Jabberwocky"
-- True

----------------------------------------
-- Law Two: Prism Complement
----------------------------------------

-- let Just a = preview myPrism s
-- let s' = review myPrism a
-- s == s'

-- let s = Right 32
-- let Just a = preview _Right s
-- s' = review _Right a
-- s == s'
-- True

-- let s = "[1, 2, 3]"
-- "[1, 2, 3]"
-- let Just a = preview (_Show :: Prism' String [Int]) s
-- let s' = review _Show a
-- "[1,2,3]"
-- s == s'
-- False

----------------------------------------
-- Law Three: Pass-through Reversion
----------------------------------------

-- let Left t = matching l s
-- let Left s' = matching l t
-- s == s'

-- let s = Nothing :: Maybe Int
-- Nothing
-- let Left (t :: Maybe String) = matching _Just s
-- let Left (s' :: Maybe Int)   = matching _Just t
-- >>> s'
-- Nothing
-- s == s'
-- True

-- let s = Left "Yo" :: Either String Int
-- Left "Yo"
-- let Left (t :: Either String Bool) = matching _Right s
-- let Left (s' :: Either String Int) = matching _Right t
-- >>> s'
-- Left "Yo"
-- s == s'
-- True

----------------------------------------
-- Summary
----------------------------------------

----------------------------------------
-- 9.4 Case Study: Simple Server
----------------------------------------

path :: Lens' Request Path
path = lens getter setter
  where
    getter (Post p body) = p
    getter (Get p) = p
    getter (Delete p) = p
    setter (Post _ body) p = Post p body
    setter (Get _) p = Get p
    setter (Delete _) p = Delete p

path1 = Get ["posts", "12345"] ^. path
-- ["posts", "12345"]
path2 = Post ["posts", "12345"] "My new post" ^. path
-- ["posts", "12345"]
path3 = Get ["posts", "12345"] & path .~ ["hello"]
-- Get ["hello"]

serveRequest :: Request -> String
serveRequest _ = "404 Not Found"

serveRequest1 = serveRequest (Get ["hello"])
-- "404 Not Found"
serveRequest2 = serveRequest (Delete ["user"])
-- "404 Not Found"
serveRequest3 = serveRequest (Post ["hello"] "Is anyone there?")
-- "404 Not Found"

----------------------------------------
-- Path prefix matching
----------------------------------------

_PathPrefix :: String -> Prism' Request Request
_PathPrefix prefix = prism' embed match
  where
    embed :: Request -> Request
    embed req = req & path %~ (prefix :)
    match :: Request -> Maybe Request
    match req
      | has (path . _head . only prefix) req = Just (req & path %~ drop 1)
    match _ = Nothing

pathPrefix1 = (Get ["users", "all"]) ^? _PathPrefix "users"
-- Just (Get ["all"])
pathPrefix2 = (Delete ["posts", "12345"]) ^? _PathPrefix "posts"
-- Just (Delete ["12345"])
pathPrefix3 = (Get ["other"]) ^? _PathPrefix "users"
-- Nothing
pathPrefix4 = _PathPrefix "users" # Get ["all"]
-- Get ["users", "all"]
pathPrefix5 = _PathPrefix "posts" # Delete ["12345"]
-- Delete ["posts", "12345"]

-- prefixed :: Eq 1 => [a] -> Prism' [a] [a]

----------------------------------------
-- Altering sub-sets of functions
----------------------------------------

-- outside :: Prism s t a b -> Lens (t -> r) (b -> r) (a -> r)

tail2 = tail [1, 2, 3]
-- [2, 3]
tail3 = tail []
-- *** Exception: Prelude.tail: empty list

safeTail :: [a] -> [a]
safeTail = tail & outside _Empty .~ const []

safeTail1 = safeTail [1, 2, 3]
-- [2, 3]
safeTail2 = safeTail []
-- []

-- _Empty :: Prism' [a] ()
-- outside _Empty :: Lens' ([a] -> [a]) (() -> [a])

userHandler :: Request -> String
userHandler req =
  "User handler! Remaining path: " <> intercalate "/" (req ^. path)

server :: Request -> String
server = serveRequest
           & outside (_PathPrefix "users") .~ userHandler

server1 = server (Get ["users", "12345"])
-- "User handler! Remaining path: 12345"
server2 = server (Delete ["users", "12345", "profile"])
-- "User handler! Remaining path: 12345/profile"
server3 = server (Post ["Admin"] "DROP TABLE users")
-- "404 Not Found"

server' :: Request -> String
server' = serveRequest
           & outside (_PathPrefix "users") .~ userHandler
           & outside (_PathPrefix "posts") .~ const "Posts handler!"

server4 = server' (Post ["posts"] "My new post")

server'' :: Request -> String
server'' = serveRequest
           & outside (_PathPrefix "users") .~ userHandler
           & outside (_PathPrefix "posts") .~ const "Posts handler!"
           & outside (_PathPrefix "posts" . _PathPrefix "index")
             .~ const "Post Index"

server5 = server'' (Get ["posts"])
-- "Posts Handler!"
server6 = server'' (Get ["posts", "index"])
-- "Posts Index"

server''' :: Request -> String
server''' = serveRequest
              & outside (_PathPrefix "users") .~ userHandler
              & outside (_PathPrefix "posts") .~ postHandler

postHandler :: Request -> String
postHandler =
  const "Posts Handler!"
    & outside (_PathPrefix "index") .~ const "Post Index"

----------------------------------------
-- Matching on HTTP Verb
----------------------------------------

-- outside (_PathPrefix "posts")
--   :: Lens (Request -> String) (Request -> String)

-- outside (_PathPrefix "posts" . _Post)
--   :: Lens (Request -> String) ((Path, Body) -> String)

-- outside (_PathPrefix "posts" . _Get)
--   :: Lens (Request -> String) (Path -> String)

postHandler' :: Request -> String
postHandler' = const "404 Not Found"
  & outside _Post
    .~ (\(path', body) -> "Created post with body: " <> body)
  & outside _Get
    .~ (\path' -> "Fetching post at path: " <> intercalate "/" path')
  & outside _Delete
    .~ (\path' -> "Created post with body: " <> intercalate "/" path')

server'''' :: Request -> String
server'''' = serveRequest
              & outside (_PathPrefix "users") .~ userHandler
              & outside (_PathPrefix "posts") .~ postHandler'

server7 = server'''' (Get ["posts", "12345"])
-- "Fetching post at path: 12345"
server8 = server'''' (Post ["posts", "12345"] "My new post")
-- "Created post with body: My new post"
server9 = server'''' (Delete ["posts", "12345"])
-- "Deleting post at path: 12345"

postHandler'' :: Request -> String
postHandler'' (Post path' body) =
  "Created post with body: " <> body
postHandler'' (Get path') =
    "Fetching post at path: " <> intercalate "/" path'
postHandler'' (Delete path') =
    "Created post with body: " <> intercalate "/" path'
