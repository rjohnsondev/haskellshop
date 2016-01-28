{-# LANGUAGE OverloadedStrings #-}

module Render.Products where

import Application
import Control.Monad.Trans
import Snap.Snaplet
import Data.Scientific
import qualified Heist.Compiled as C
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified ShopData.Product as P
import FormUtil
import Heist
import HeistUtil

mainImage :: P.Product -> RuntimeSplice AppHandler (P.Product, DT.Text)
mainImage p = do
        is <- lift $ with db $ P.images (P.productId p)
        return $ if null is
            then (p, "")
            else (p, head is)

-- would be better done in SQL, but without dynamic query creation it's
-- verbose.
filterPrice :: [(P.Product, DT.Text)] -> RuntimeSplice AppHandler (Scientific, Scientific, [(P.Product, DT.Text)])
filterPrice ps = do
        pricesStr <- lift $ getFormByteString "p" "0.0,0.0"
        let (min', max') = deSerializePrices (DTE.decodeUtf8 pricesStr)
        return $ (min', max', filter (\(p, _) -> (P.basePrice p) >= min' && (max' == 0.0 || (P.basePrice p) <= max')) ps)

filterAvailable :: [(P.Product, DT.Text)] -> [(P.Product, DT.Text)]
filterAvailable = filter (\(p, _) -> P.enabled p)

textToScientific :: DT.Text -> Scientific
textToScientific t = 
    case reads (DT.unpack t) :: [(Scientific, String)] of
        []       -> 0.0
        [(i, _)] -> i
        _        -> 0.0

deSerializePrices :: DT.Text -> (Scientific, Scientific)
deSerializePrices t =
        case textArrayToScientific (DT.splitOn "," t) of
            [min', max'] -> (min', max')
            _ -> (0.0, 0.0)
    where
        textArrayToScientific ta = map textToScientific ta

productListRuntime :: RuntimeSplice AppHandler [(P.Product, DT.Text)]
productListRuntime = do
        icid <- lift $ getFormInt "c" 0
        let cid = if icid == 0 then Nothing else Just icid
        vpos <- lift $ getFormIntArray "vo"
        ps <- lift $ with db $ P.products cid vpos
        mapM mainImage ps

productListFilteredRuntime :: RuntimeSplice AppHandler [(P.Product, DT.Text)]
productListFilteredRuntime = do
        ps <- productListRuntime
        (_, _, filteredPs) <- filterPrice (filterAvailable ps)
        return filteredPs

renderProductListDetails :: Splices (RuntimeSplice AppHandler (P.Product, DT.Text) -> C.Splice AppHandler)
renderProductListDetails =
    do
        "product_name"         ## pt (\(x, _) -> P.name x)
        "product_manufacturer" ## pt (\(x, _) -> P.manufacturer x)
        "product_id"           ## pt (\(x, _) -> DT.pack . show . P.productId $ x)
        "product_hash"         ## pt (\(_ ,i) -> i)
        "product_base_price"   ## pt (\(x ,_) -> DT.pack . formatPrice . P.basePrice $ x)
    where
        pt = C.pureSplice . escapingTextSplice


renderProductList :: RuntimeSplice AppHandler [(P.Product, DT.Text)] -> C.Splice AppHandler
renderProductList d = C.manyWithSplices C.runChildren renderProductListDetails d

type PriceFilterViewModel = ( Scientific -- selected min
                            , Scientific -- selected max
                            , Scientific -- actual min
                            , Scientific) -- actual max

priceFilterRuntime :: RuntimeSplice AppHandler PriceFilterViewModel
priceFilterRuntime = do
        ps' <- productListRuntime
        let ps = filterAvailable ps'
        let (min', max') = if null ps
                             then (0,0)
                             else ( minimum (map (\(p, _) -> P.basePrice p) ps)
                                  , maximum (map (\(p, _) -> P.basePrice p) ps))
        (selMin, selMax, _) <- filterPrice ps
        return (selMin, if selMax == 0 then max' else selMax, min', max')

renderPriceFilter :: RuntimeSplice AppHandler PriceFilterViewModel
                  -> C.Splice AppHandler
renderPriceFilter =
        C.withSplices
            C.runChildren
            (do "selected_min" ## pt $  (\(smin, _, _, _) -> fs smin)
                "selected_max" ## pt $  (\(_, smax, _, _) -> fs smax)
                "min"          ## pt $  (\(_, _, min', _) -> fs min')
                "max"          ## pt $  (\(_, _, _, max') -> fs max'))
    where
        fs x = DT.pack $ formatScientific Fixed (Just 0) x
        pt = C.pureSplice . escapingTextSplice
