module Main.Instances 
    ( 
    ) where
import qualified Text.Blaze.Html5 as H (ToMarkup(..))
import qualified Text.XHtml as X (Html)

instance H.ToMarkup X.Html where
    preEscapedToMarkup = H.preEscapedToMarkup . show
    toMarkup = H.preEscapedToMarkup . show
