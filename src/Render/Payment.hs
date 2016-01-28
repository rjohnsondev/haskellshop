{-# LANGUAGE OverloadedStrings #-}

module Render.Payment where

import Application
import Heist
import HeistUtil
import Snap.Snaplet
import Snap.Snaplet.Session
import Control.Monad.Trans
import Render.Countries
import qualified Heist.Compiled as C
import qualified Data.Text as DT
import qualified Data.Map as M

paymentRuntime :: RuntimeSplice AppHandler (M.Map DT.Text DT.Text)
paymentRuntime = do
        lst <- lift $ with sess $ sessionToList
        return $ foldr (\(k, v) out -> M.insert k v out) M.empty lst

renderPayment :: C.Splice AppHandler
renderPayment =
        C.withSplices
            C.runChildren
            (do "shipping_full_name"       ## pt . get $ "shipping_full_name"
                "billing_full_name"        ## pt . get $ "billing_full_name"
                "shipping_address_1"       ## pt . get $ "shipping_address_1"
                "billing_address_1"        ## pt . get $ "billing_address_1"
                "shipping_address_2"       ## pt . get $ "shipping_address_2"
                "billing_address_2"        ## pt . get $ "billing_address_2"
                "shipping_city"            ## pt . get $ "shipping_city"
                "billing_city"             ## pt . get $ "billing_city"
                "shipping_state"           ## pt . get $ "shipping_state"
                "billing_state"            ## pt . get $ "billing_state"
                "shipping_postcode"        ## pt . get $ "shipping_postcode"
                "billing_postcode"         ## pt . get $ "billing_postcode"
                "email"                    ## pt . get $ "email"
                "phone"                    ## pt . get $ "phone"
                "billing_countries"        ## pn $ countryOptionsList . (get "billing_country")
                "shipping_countries"       ## pn $ countryOptionsList . (get "shipping_country")
                "billing_full_name_class"  ## pt . get $ "billing_full_name_class"
                "billing_address_1_class"  ## pt . get $ "billing_address_1_class"
                "billing_city_class"       ## pt . get $ "billing_city_class"
                "billing_country_class"    ## pt . get $ "billing_country_class"
                "shipping_full_name_class" ## pt . get $ "shipping_full_name_class"
                "shipping_address_1_class" ## pt . get $ "shipping_address_1_class"
                "shipping_city_class"      ## pt . get $ "shipping_city_class"
                "shipping_country_class"   ## pt . get $ "shipping_country_class"
                "email_class"              ## pt . get $ "email_class"
                "phone_class"              ## pt . get $ "phone_class")
            paymentRuntime
    where
        pt = C.pureSplice . escapingTextSplice
        pn = C.pureSplice . C.nodeSplice
        get k m = M.findWithDefault "" k m


