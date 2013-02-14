{-# LANGUAGE OverloadedStrings #-}
module Habolous.Templates.DatabaseVersion (html) where

import qualified Text.Blaze.Html5 as BH (Html, h1, h2, p, toHtml)
import qualified Habolous.Templates.Master as FBTM (master)

html :: String -> String -> BH.Html
html srver clver =
    FBTM.master title extraheaders body
    where
      title = "Habolous database versions"
      extraheaders = []
      body = do
          BH.h1 title
          BH.h2 "Database server version"
          BH.p $ BH.toHtml srver
          BH.h2 "Database client version"
          BH.p $ BH.toHtml clver
