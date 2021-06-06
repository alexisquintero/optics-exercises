module Chapter14.People where

data Person =
  Person { _name :: String
         , _favouriteFood :: String
         } deriving Show

-- makeFieldsNoPrefix ''Person
-- 
-- class HasName s a | s -> a where
--   name :: Lens' s a
--
-- class HasFavouriteFood s a | s -> a where
--   favouriteFood :: Lens' s a

-- makeClassy ''Person
--
-- class HasPerson c where
--   person :: Lens' c Person
--   favouriteFood :: Lens' c String
--   name :: Lens' c String
