{-# Language OverloadedStrings #-}
module Data.Blend.Html (dumpHtml) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Data.Blend.Types

dumpHtml :: BBlend -> IO ()
dumpHtml blend = do
  let sdna = blendSdna blend
  (B.putStr . renderHtml) $ do
    H.docType
    H.meta ! A.charset "UTF-8"
    H.title "Blender SDNA structures"
    mapM_ (uncurry structAsHtml) $ zip [(0::Integer)..] sdna

structAsHtml :: (Integral i, Show i) => i -> Struct -> Html
structAsHtml i (n, fs) =
  H.div ! A.class_ "sdna-structure" $ do
    H.span ! A.class_ "sdna-index" $ H.string (show i)
    H.span ! A.class_ "sdna-structure-name" $ H.string
      $ "structure " ++ BC.unpack n
    H.ul $ mapM_ fieldAsHtml fs

fieldAsHtml :: Field -> Html
fieldAsHtml (n, t) = do
  H.li ! A.class_ "sdna-field" $ do
    H.span ! A.class_ "sdna-field-type" $ H.string $ show t
    H.span ! A.class_ "sdna-field-name" $ H.string $ BC.unpack n
