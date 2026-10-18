module TypeClasses.Person where

data Person = MkPerson {nr :: Int, email :: String}

instance Show Person where
  show (MkPerson nr email) = "Person: " ++ show nr ++ ", Email: " ++ email

instance Eq Person where
  (MkPerson nr1 email1) == (MkPerson nr2 email2) = nr1 == nr2 && email1 == email2

instance Ord Person where
  compare (MkPerson nr1 _) (MkPerson nr2 _) = compare nr1 nr2