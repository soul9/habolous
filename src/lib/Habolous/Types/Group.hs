module Habolous.Types.Group (Group(..), nullGroup) where

import qualified Data.Text as T (Text, pack)

data Group = Group
    {name    :: T.Text,
     groupId :: Integer
    } deriving (Show, Read, Eq)

nullGroup :: Group
nullGroup = Group {name = T.pack "",
                   groupId = 0}
