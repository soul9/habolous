module Habolous.Types.Role (Role(..), nullRole) where

import qualified Data.Text as T (Text, pack)

data Role = Role
    {name     :: T.Text,
     roleId   :: Integer
    } deriving (Show, Read, Eq)

nullRole :: Role
nullRole = Role {name = T.pack "",
                 roleId = 0}
