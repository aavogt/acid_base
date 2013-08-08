{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main.Instances 
    ( 
    ) where
import qualified Text.Blaze.Html5 as H (ToMarkup(..))
import Text.Trifecta hiding (string)
import qualified Text.XHtml as X (Html)

instance H.ToMarkup X.Html where
    preEscapedToMarkup = H.preEscapedToMarkup . show
    toMarkup = H.preEscapedToMarkup . show
