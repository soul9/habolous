{-# LANGUAGE OverloadedStrings #-}
module Habolous.Templates.Version (html) where

import qualified Text.Blaze.Html5 as BH (Html, h1, p, toHtml)
import qualified Habolous.Templates.Master as FBTM (master)

html :: String -> BH.Html
html ver =
    FBTM.master title extraheaders body
    where
      title = "Habolous version"
      extraheaders = []
      body = do
          BH.h1 $ BH.toHtml title
          BH.p $ BH.toHtml ver
