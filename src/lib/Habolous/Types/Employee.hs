module Habolous.Types.Employee (Employee(..), nullEmployee) where

import qualified Data.Text as T (Text, pack)
import qualified Habolous.Types.Person as FTP (Person(..), nullPerson)
import qualified Habolous.Types.Group as FTG (Group(..), nullGroup)
import qualified Habolous.Types.Role as FTR (Role(..), nullRole)

data Employee = Employee
    {loginName      :: T.Text,
     password       :: T.Text,
     employeeId     :: Integer,
     employeeGroup  :: FTG.Group,
     employeeRole   :: FTR.Role,
     employeePerson :: FTP.Person
    } deriving (Show, Read, Eq)

nullEmployee :: Employee
nullEmployee = Employee {employeeId = 0,
                         loginName = T.pack "",
                         password = T.pack "",
                         employeeGroup = FTG.nullGroup,
                         employeeRole = FTR.nullRole,
                         employeePerson = FTP.nullPerson}
