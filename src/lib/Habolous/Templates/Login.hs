{-# LANGUAGE OverloadedStrings #-}
module Habolous.Templates.Login (html) where

import qualified Text.Blaze as B ((!))
import qualified Text.Blaze.Html5 as BH (Html, h1, form, input,
                                         label, br, p, toHtml)
import qualified Text.Blaze.Html5.Attributes as BA (type_, name,
                                                    action, method,
                                                    value, enctype)
import qualified Data.Text as T (unpack)
import qualified Habolous.Types.Employee as FTE (Employee(..))
import qualified Habolous.Types.Person as FTP (Person(..))
import qualified Habolous.Templates.Master as FBTM (master)

html :: String -> Maybe FTE.Employee -> BH.Html
html title employee =
    FBTM.master t extraheaders body
    where
      t = BH.toHtml title
      extraheaders = []
      body = do
          BH.h1 t
          case employee of
              Nothing -> form
              Just e -> do
                  let fname = T.unpack $ FTP.firstName $ FTE.employeePerson e
                      lname = T.unpack $ FTP.lastName $ FTE.employeePerson e
                      name = fname ++ " " ++ lname
                  BH.p $ BH.toHtml $ "Welcome " ++ name ++ "!"

form :: BH.Html
form = do
    BH.form B.! BA.enctype "multipart/form-data" B.!
     BA.action "/login" B.! BA.method "post" $ do
        BH.label "Login: " >>
         BH.input B.! BA.type_ "text" B.! BA.name "login" >>
          BH.br
        BH.label "Password: " >>
         BH.input B.! BA.type_ "password" B.! BA.name "password" >>
          BH.br
        BH.input B.! BA.type_ "submit" B.! BA.value "Login"