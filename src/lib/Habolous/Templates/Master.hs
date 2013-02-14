{-# LANGUAGE OverloadedStrings #-}
module Habolous.Templates.Master where

import qualified Text.Blaze as B ((!))
import qualified Text.Blaze.Html5 as BH (Html, html, head,
                                         title, meta, body,
                                         toHtml)
import qualified Text.Blaze.Html5.Attributes as BHA (content, httpEquiv)

master :: BH.Html -> [BH.Html] -> BH.Html -> BH.Html
master title headers body =
    BH.html $ do
      BH.head $ do
        BH.title $ BH.toHtml title
        BH.meta B.! BHA.httpEquiv "Content-Type" B.!
         BHA.content "text/html;charset=utf-8"
        sequence_ headers
      BH.body $ do
        body
