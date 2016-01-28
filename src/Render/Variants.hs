{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module Render.Variants where

import HeistUtil
import Application
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Scientific
import FormUtil
import Heist
import Snap.Snaplet
import Text.Printf
import Render.Product (productRuntime)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as DT
import qualified Heist.Compiled as C
import qualified ShopData.Product as P
import qualified ShopData.ProductVariantOption as PVO
import qualified ShopData.Variant as V
import qualified ShopData.VariantOption as VO
import qualified Text.XmlHtml as X

type VariantViewModel = (S.Set Int, M.Map Int Scientific, V.Variant)
type VariantOptionViewModel = (V.Variant, M.Map Int Scientific, VO.VariantOption)

renderVariantOption :: Splices (RuntimeSplice AppHandler VariantOptionViewModel -> C.Splice AppHandler)
renderVariantOption = do
        "variant_option_name"             ## pt (\(_, _, vo) -> VO.option vo)
        "variant_option_id"               ## pt (\(_, _, vo) -> DT.pack . show . VO.variantOptionId $ vo)
        "variant_option_price"            ## pt $ variantOptionDisplayPrice
        "variant_option_enabled_checkbox" ## templatedNodeSplice . variantOptionEnabledCheckbox
        "variant_option_price_block"      ## templatedNodeSplice . variantOptionPriceBlock
        "variant_option_del_btn"          ## templatedNodeSplice . variantOptionDelBtn
    where
        pt = C.pureSplice . escapingTextSplice

variantOptionDelBtn :: RuntimeSplice AppHandler VariantOptionViewModel
                    -> X.Node
                    -> RuntimeSplice AppHandler X.Node
variantOptionDelBtn vovm n = vovm >>= \(_, _, vo) -> 
        return . setValue (VO.variantOptionId vo)
               . setDisabled ((VO.productsUsing vo) > 0)
               . setTitle (VO.productsUsing vo)
               $ n
    where
        setValue voId = X.setAttribute "value" (DT.pack . show $ voId)
        setDisabled b n' = if b then X.setAttribute "disabled" "" n' else n'
        setTitle pu = X.setAttribute "title" (DT.pack (printf "Currenty %d product(s) using this option" pu))

variantOptionDisplayPrice :: VariantOptionViewModel
                          -> DT.Text
variantOptionDisplayPrice (_, p, vo) =
        (case M.lookup (VO.variantOptionId vo) p
            of Nothing -> ""
               Just x -> (case compare x 0 of
                            GT -> (DT.concat ["+", DT.pack . formatPrice $ x])
                            LT -> DT.pack . formatPrice $ x
                            EQ -> ""))

variantOptionPriceBlock :: RuntimeSplice AppHandler VariantOptionViewModel
                        -> X.Node
                        -> RuntimeSplice AppHandler X.Node
variantOptionPriceBlock vovm n = vovm >>= \(v, p, vo) -> 
        return . setId (V.variantId v) (VO.variantOptionId vo)
               . setName (VO.variantOptionId vo)
               . setValue (case M.lookup (VO.variantOptionId vo) p
                                of Nothing -> ""
                                   Just x -> DT.pack . formatPrice $ x)
               . setDisabled (M.member (VO.variantOptionId vo) p)
               $ n
    where
        setId vid voId = X.setAttribute "id" (DT.pack (printf "price-%d-%d" vid voId))
        setValue = X.setAttribute "value"
        setName voId = X.setAttribute "name" $ DT.pack . printf "price-%d" $ voId
        setDisabled b n' = if b then n' else X.setAttribute "disabled" "" n'

variantOptionEnabledCheckbox :: RuntimeSplice AppHandler VariantOptionViewModel
                             -> X.Node
                             -> RuntimeSplice AppHandler X.Node
variantOptionEnabledCheckbox vovm n = vovm >>= \(v, p, vo) ->
        return . setChecked (M.member (VO.variantOptionId vo) p)
               . setDataTarget (V.variantId v) (VO.variantOptionId vo)
               . setValue (VO.variantOptionId vo)
               $ n
    where
        setChecked b n' = if b then X.setAttribute "checked" "" n' else n'
        setValue voId = X.setAttribute "value" (DT.pack .show $ voId)
        setDataTarget vid voId = X.setAttribute "data-target" (DT.pack (printf "#price-%d-%d" vid voId))

variantOptionsViewModels :: VariantViewModel -> [VariantOptionViewModel]
variantOptionsViewModels (_, pvoPrices, v) = map (\vo -> (v, pvoPrices, vo)) $ V.options v

variantOptionsSplice :: RuntimeSplice AppHandler VariantViewModel -> C.Splice AppHandler
variantOptionsSplice x  =
        C.manyWithSplices C.runChildren renderVariantOption $ liftM variantOptionsViewModels x
        
renderVariant :: Splices (RuntimeSplice AppHandler VariantViewModel -> C.Splice AppHandler)
renderVariant = do
        "variant_name"              ## pt $ (\(_, _, v) -> V.name v)
        "variant_id"                ## pt $ (\(_, _, v) -> DT.pack . show $ V.variantId v)
        "variant_enabled_checkbox"  ## templatedNodeSplice . variantEnabledCheckedBlock
        "variant_options"           ## variantOptionsSplice
        "variant_hide_delete_block" ## pt $
            (\(_, _, v) -> if (length (V.options v)) > 0 then "display: none;" else "")
    where
        pt = C.pureSplice . escapingTextSplice

variantEnabledCheckedBlock :: RuntimeSplice AppHandler VariantViewModel
                           -> X.Node
                           -> RuntimeSplice AppHandler X.Node
variantEnabledCheckedBlock vvm n = vvm >>= \(s, _, v) ->
        return . checkedAttr s (V.variantId v)
               . dataTargetAttr (V.variantId v)
               $ n
    where
        checkedAttr s vid n' = if S.member vid s then X.setAttribute "checked" "" n' else n'
        dataTargetAttr vid = X.setAttribute "data-target" (DT.pack $ printf "#container-%d" vid)


variantViewModels :: ([PVO.ProductVariantOption], [V.Variant]) -> [VariantViewModel]
variantViewModels (pvos, vs) = map (\x -> (checkedPVSet, pvoPrices, x)) vs
    where
       checkedPVSet = S.fromList $ map PVO.variantId pvos
       pvoPrices = M.fromList $ map (PVO.variantOptionId &&& PVO.priceAdjustment) pvos

renderVariants :: RuntimeSplice AppHandler ([PVO.ProductVariantOption], [V.Variant]) -> C.Splice AppHandler
renderVariants d =
        C.manyWithSplices C.runChildren renderVariant
        $ liftM variantViewModels d

variantsRuntime :: RuntimeSplice AppHandler ([PVO.ProductVariantOption], [V.Variant])
variantsRuntime = do
        pr <- productRuntime
        pvos <- case pr of Nothing -> return [] :: RuntimeSplice AppHandler [PVO.ProductVariantOption]
                           Just p -> lift $ with db $ P.productVariantOptions (P.productId p)
        v <- lift $ with db V.getVariants
        return (pvos, v)

filterActiveProductVariants :: ([PVO.ProductVariantOption], [V.Variant]) -> ([PVO.ProductVariantOption], [V.Variant])
filterActiveProductVariants (pvos, vs) =
        ( pvos
        , filter
            (\v -> length (V.options v) > 0)
            $ map
                (\v -> v {V.options = filter
                                        (\vo -> S.member (VO.variantOptionId vo) s)
                                        (V.options v)})
                vs)
    where
        s = S.fromList $ map PVO.variantOptionId pvos

variantsRuntimeActive :: RuntimeSplice AppHandler ([PVO.ProductVariantOption], [V.Variant])
variantsRuntimeActive = liftM filterActiveProductVariants variantsRuntime

variantsRuntimeUrl :: RuntimeSplice AppHandler ([PVO.ProductVariantOption], [V.Variant])
variantsRuntimeUrl = do
        voIds <- lift $ getFormIntArray "vo"
        let pvos = map (\pvoid -> PVO.ProductVariantOption 0 0 "" pvoid "" 0.0) voIds
        v <- lift $ with db V.getVariants
        return (pvos, v)
