{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Form 
    ( viewForm
    , c0Form
    , chargeForm
    , form0
    , kaForm
    ) where


import AcidBase.Charge (charge)
import AcidBase.KAs (kAs)
import Control.Applicative ((<$>), Applicative((<*>)))
import qualified Data.Map as M (singleton)
import Form.Parsers (chargeToStr, compToStr, kAsToStr, strToCharge, strToComp, strTokAs)
import Form.Parsers.Internal (parsersToF)
import qualified Text.Blaze.Html5 as H (h1, p, table, td, th, tr)
import Text.Digestive.Blaze.Html5 (childErrorList, form, inputSubmit, inputTextArea)
import Text.Digestive.Form ((.:))
import Text.Trifecta hiding (string, string, string, string, string)


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
     \ for values to stabilize (in this case ionic strength varying less than 0.001%)"
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


c0Form = parsersToF strToComp compToStr (M.singleton (""::String) (1.0::Double))


chargeForm = parsersToF strToCharge chargeToStr charge


form0 (a,b,c) = (,,) <$> "comp" .: c0Form a <*> "charge" .: chargeForm b <*> "eq" .: kaForm c

kaForm = parsersToF strTokAs kAsToStr kAs



