{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter12.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

----------------------------------------
-- 12. Dealing with Type Errors
----------------------------------------

----------------------------------------
-- 12.1 Interpreting expanded optics types
----------------------------------------

error1 = [(1, True), (2, False), (3, True)]
           -- & folded . _1 *~ 10
           & traversed . _1 *~ 10

----------------------------------------
-- 12.2 Type Error Arena
----------------------------------------

----------------------------------------
-- First Foe: Level 1 Lenslion
----------------------------------------

error2 = view _1 ('a', 2)
-- Import Control.Lens

----------------------------------------
-- Level 2 Tuplicant
----------------------------------------

-- error3 = view ("abc", 123) _1
noterror3 = view _1 ("abc", 123)

----------------------------------------
-- Level 3 Settersiren
----------------------------------------

-- error4 = ("old", False) $ _1 .~ "new"
noterror4 = ("old", False) & _1 .~ "new"

----------------------------------------
-- Level 4 Composicore
----------------------------------------

-- error5 = view (_3 . _1) (('a', 'b', 'c'), 'd')
noterror5 = view (_1 . _3) (('a', 'b', 'c'), 'd')

----------------------------------------
-- Level 5 Foldasaurus
----------------------------------------

-- error6 = ("blue", Just (2 :: Int)) ^. _2 . _Just
noterror6 = ("blue", Just (2 :: Int)) ^? _2 . _Just

----------------------------------------
-- Level 6 Higher Order Beast
----------------------------------------

-- error7 = ['a'..'z'] ^.. taking 5 . folded
noterror7 = ['a'..'z'] ^.. taking 5 folded

----------------------------------------
-- Level 7 Traversacula
----------------------------------------

error8 = over both putStrLn ("one", "two")
noterror8 = traverseOf_ both putStrLn ("one", "two")

