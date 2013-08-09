{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Form 
    ( viewForm
    , form0) where

import AcidBase.Solve
import AcidBase.Data (charge, kAs)
import Control.Applicative
import qualified Data.Map as M (singleton, Map)
import Form.Parsers (chargeToStr, compToStr, kAsToStr, strToCharge, strToComp, strTokAs)
import Form.Parsers.Internal (parsersToF)
import qualified Text.Blaze.Html5 as H (h1, p, table, td, th, tr)
import Text.Digestive.Blaze.Html5 (childErrorList, form, inputSubmit, inputTextArea)
import Text.Digestive.Form -- ((.:))
import Text.Trifecta hiding (string, string, string, string, string)
import Control.Lens

import Data.Text (Text)
import Text.Digestive.Form.Internal (FormTree)
import Data.Monoid

viewForm v = form v "/" $ do
    H.h1 "Acid-Base equilibrium calculator"
    H.p "Given certain input concentrations, charges and pKas this calculator\
     \ can determine the concentration of all species in the ideal solution at equilibrium.\
     \ Spectator ions, which do not participate in the equilibria are not included\
     \ in the output (and input). However they are considered in the equation for electroneutrality,\
     \ in the sense that the initial concentration H2PO4 is the concentration of some\
     \ spectator cation such as Na+. \
     \ For the equilibrium data, the rows are specified as 'conjugate base' 'acid' pKa.\
     \ The actual names used are unimportant. \
     \ The davies equation is used the outer loop. Only one iteration seems to be necessary\
     \ for values to stabilize (in this case ionic strength varying less than 0.001%)\
     \ Water is included as an acid with the same convention for pKa as other acids: \
     \ in other words the equation used is -pKa = log(OH) + log(H) - log(H2O) rather than\
     \ the more usual -pKa = log(OH) + log(H)."
    H.table $ do
        H.tr $ do
            H.th "initial composition"
            H.th "ion charges"
            H.th "A HA pKa"

        let w= Just 25
            h= Just 20
        H.tr $ do
            H.td $ do
                childErrorList "comp" v
                inputTextArea h w "comp" v
            H.td $ do
                childErrorList "charge" v
                inputTextArea h w "charge" v
            H.td $ do
                childErrorList "eq" v
                inputTextArea h w "eq" v

    inputSubmit "submit"


form0 x = runLF x (pure x)
    $ lf "comp" (parsersToF strToComp compToStr) problem_initialConcentration
    <> lf "charge" (parsersToF strToCharge chargeToStr) problem_ionCharge
    <> lf "eq" (parsersToF strTokAs kAsToStr) problem_equilibrium

runLF x x' lf = ($ x) <$> unLF lf x'

newtype LF f v m a = LF { unLF :: f a -> Form v m (a -> a) }
instance (Monad m, Monoid v) => Monoid (LF f v m a) where
    mempty = LF $ \_ -> pure id
    mappend (LF a) (LF b) = LF $ \x -> (\f g -> f . g) <$> a x <*> b x

lf :: (Functor f, Monad m)
    => Text -- ^ label
    -> (f b  -> Form v m b) -- ^ formlet taking an initial values
    -> Lens' a b -- ^ gets the @b@ out of the @a@
    -> LF f v m a
lf n x l1 = LF $ \y -> lform n x l1 y

lform :: (Functor f, Monad m)
    => Text -- ^ label
    -> (f b  -> Form v m b) -- ^ formlet taking an initial values
    -> Lens' a b -- ^ gets the @b@ out of the @a@
    -> f a -- ^ initial value
    -> Form v m (a -> a)
lform n x l1 y = (l1 .~ ) <$> (n .: x (fmap (^. l1) y))







