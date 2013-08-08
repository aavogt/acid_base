{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- | TODO

* fancy ajax (update on type?)

* ghcjs

* other activity coefficient models

* what prints 7?

-}
module Form.Parsers.Internal 
    ( parsersToF
    , parse2
    , compoundName
    ) where



import Control.Applicative ((<$>), Applicative((<*)))
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Monoid (Monoid(mempty))
import Main.Aha (aha)
import qualified Text.Blaze.Html5 as H (ToMarkup(preEscapedToMarkup))
import Text.Digestive.Form (string, validateM)
import Text.Digestive.Types (Result(Error, Success))
import Text.Trifecta (noneOf, parseString, Result(Failure, Success), some, whiteSpace)
import Text.Trifecta hiding (string, string, string)




parsersToF from to x0 x = validateM (liftIO . parse2 from) (string (fmap (\y -> to (y `asTypeOf` x0)) x))



parse2 p str = case parseString p mempty str of
    Text.Trifecta.Success a -> return $ Text.Digestive.Types.Success a
    Failure d -> Error . H.preEscapedToMarkup <$> aha (show d)



compoundName = some (noneOf " \t\n") <* whiteSpace


