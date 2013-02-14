module Habolous.Types.Person (Person(..), nullPerson) where

import qualified Data.Time.Calendar as C (Day, fromGregorian)
import qualified Data.Text as T (Text, pack)

data Person = Person
    {firstName :: T.Text,
     lastName  :: T.Text,
     birthday  :: C.Day,
     personId  :: Integer}
    deriving (Show, Read, Eq)

nullPerson :: Person
nullPerson = Person {firstName = T.pack "",
                     lastName = T.pack "",
                     birthday = C.fromGregorian 1850 0 0,
                     personId = 0}
