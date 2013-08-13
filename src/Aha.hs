-- | adapter for https://github.com/theZiz/aha
module Aha
    ( aha
    ) where
import Control.Applicative ((<$>))
import System.Process (readProcess)
import Text.HTML.TagSoup (parseTags, renderTags, Tag(TagClose, TagOpen))
import Text.Trifecta hiding (string)

aha s = renderTags . getBody . parseTags <$> readProcess "aha" [] s
    where
    getBody = takeWhile (/= TagClose "body") . drop 1 . dropWhile (/= TagOpen "body" [])
