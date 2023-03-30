{-# LANGUAGE Rank2Types #-}
{- | lensy formlets

This pattern has redundancy:

> tripletFormlet :: Formlet v m (a,b,c)
> tripletFormlet init =
>   let (a,b,c) | Just (a,b,c) <- init = (Just a, Just b, Just c)
>               | otherwise = (Nothing, Nothing, Nothing)
>   in (,,)
>       <$> "label1" .: formlet1 a
>       <*> "label2" .: formlet2 b
>       <*> "label3" .: formlet3 c

Instead with this module we can have:

> tripletFormlet' :: (a,b,c) -> Formlet v m (a,b,c)
> tripletFormlet' init0 init = runLF init0 init $
>   lf "label1" formlet1 _1 <>
>   lf "label2" formlet2 _2 <>
>   lf "label3" formlet3 _3

Or if you can prove to yourself that all the fields have been included:

> tripletFormlet = tripletFormlet' undefined


-}
module Form.LF where

import Data.Monoid
import Control.Applicative
import Control.Lens
import Text.Digestive.Form
import Data.Text (Text)


-- | runLF valueFieldsAreSetFor value
runLF x x' lf = ($ x) <$> unLF lf x'

newtype LF f v m a = LF { unLF :: f a -> Form v m (a -> a) }

instance (Monad m, Monoid v) => Monoid (LF f v m a) where
    mempty = LF $ \_ -> pure id

instance (Monad m, Monoid v) => Semigroup (LF f v m a) where
    LF a <> LF b = LF $ \x -> (\f g -> f . g) <$> a x <*> b x

lf :: (Functor f, Monad m, Monoid v)
    => Text -- ^ label
    -> (f b  -> Form v m b) -- ^ formlet taking an initial values
    -> Lens' a b -- ^ gets the @b@ out of the @a@
    -> LF f v m a
lf n x l1 = LF $ \y -> lform n x l1 y

lform :: (Functor f, Monoid v, Monad m)
    => Text -- ^ label
    -> (f b  -> Form v m b) -- ^ formlet taking an initial values
    -> Lens' a b -- ^ gets the @b@ out of the @a@
    -> f a -- ^ initial value
    -> Form v m (a -> a)
lform n x l1 y = (l1 .~ ) <$> (n .: x (fmap (^. l1) y))



