{-# LANGUAGE OverloadedStrings #-}

module Basket where

import Application
import Snap.Snaplet
import Snap.Snaplet.Session
import Text.Printf
import Data.Scientific
import qualified Data.Text as DT
import qualified Data.Set as S
import qualified Data.Map as M
import qualified ShopData.Product as P
import qualified ShopData.ProductVariantOption as PVO

 -- basket is a map of (product id, (set of enabled product variant options)) to count
type Basket = M.Map (Int, S.Set Int) Int

textToInt :: DT.Text -> Int
textToInt t = 
    case reads (DT.unpack t) :: [(Int, String)] of
        []       -> 0
        [(i, _)] -> i
        _        -> 0

serializePVOIds :: S.Set Int -> String
serializePVOIds = S.foldr (\x out -> (show x) ++ (if out == "" then "" else "," ++ out)) ("" :: String)

deSerializePVOIds :: DT.Text -> S.Set Int
deSerializePVOIds pvoss =
        textArrayToSet (DT.splitOn "," pvoss)
    where
        textArrayToSet ta = S.fromList $ filter (> 0) $ map textToInt ta

serializeBasketKey :: (Int, S.Set Int) -> String
serializeBasketKey (pid, pvos) = printf "%d:%s" pid (serializePVOIds pvos)

deSerializeBasketKey :: DT.Text -> (Int, S.Set Int)
deSerializeBasketKey keys = dsrl $ DT.splitOn ":" keys
    where
        dsrl [pids, pvos] = (textToInt pids, deSerializePVOIds pvos)
        dsrl _            = (0, S.empty)

serializeBasket :: Basket -> DT.Text
serializeBasket b = DT.pack $
        M.foldrWithKey
            (\(key, pvos) count out ->
                printf "%s %s-%d"
                    out
                    (serializeBasketKey (key, pvos))
                    count)
            ("" :: String)
            b

deSerializeBasket :: DT.Text -> Basket
deSerializeBasket t = 
        foldr
            (\entry out -> insertTupleIntoBasket
                            (textArrayToKvTuple (DT.splitOn "-" entry))
                            out)
            (M.empty :: Basket)
            (DT.splitOn " " t :: [DT.Text])
    where
        textArrayToKvTuple [ks, counts] =
            (\(k, pvos) count -> if k > 0 && count > 0
                                    then Just (k, pvos, count)
                                    else Nothing)
                (deSerializeBasketKey ks)
                (textToInt counts)
        textArrayToKvTuple _ = Nothing
        insertTupleIntoBasket (Just (k, pvoss, count)) b = M.insert (k, pvoss) count b
        insertTupleIntoBasket Nothing b = b

basket :: AppHandler Basket
basket = with sess $ do
    b <- getFromSession "basket"
    return $ case b of Nothing ->  M.empty :: Basket
                       Just b' -> deSerializeBasket b'

addToBasket :: Int -> S.Set Int -> Basket -> Basket
addToBasket pid pvos b =
        if (pid <= 0)
            then b
            else M.insertWith (+) (pid, pvos) 1 b

reduceFromBasket :: Int -> S.Set Int -> Basket -> Basket
reduceFromBasket pid pvos b =
        if (pid <= 0)
            then b
            else M.update (\x -> if x <= 1 then Nothing else Just (x - 1)) (pid, pvos) b

saveBasket :: Basket -> AppHandler ()
saveBasket b = with sess $ do setInSession "basket" $ serializeBasket b
                              commitSession

productVariantOptionTextPrice :: Int -> S.Set Int -> AppHandler (DT.Text, Scientific)
productVariantOptionTextPrice pid voIds = do
        pvos <- with db $ P.productVariantOptions pid
        let prices = foldr (\x out -> M.insert
                                        (PVO.variantOptionId x)
                                        ( DT.concat [PVO.variantName x, ": ", PVO.variantOptionName x]
                                        , PVO.priceAdjustment x)
                                        out)
                           M.empty
                           pvos
        let pvoTxt = S.foldr (\voId (t, p) ->
                                case M.lookup voId prices of
                                    Nothing -> (t, p)
                                    Just (xt, xp) -> ( DT.concat [t, (if t == "" then "" else ", "), xt]
                                                     , p + xp))
                           ("", 0.0)
                           voIds
        return pvoTxt

type BasketViewModel = ( DT.Text -- serialized key for the entry
                       , P.Product -- product
                       , Scientific -- line total
                       , Int -- quantity
                       , DT.Text -- product options
                       , DT.Text) -- image hash

basketViewModels :: AppHandler [BasketViewModel]
basketViewModels = do
        b <- basket
        M.foldrWithKey
            (\(pid, pvos) count out -> do
                o <- out
                mp <- with db $ P.getProduct pid
                case mp of Nothing -> return o
                           Just p -> do (pvot, pvop) <- productVariantOptionTextPrice pid pvos
                                        img <- productImage pid
                                        return $ (DT.pack (serializeBasketKey (P.productId p, pvos))
                                                 , p
                                                 , (P.basePrice p + pvop) * (scientific (toInteger count) 0)
                                                 , count
                                                 , pvot
                                                 , img) : o)
            (return [])
            b
    where
        productImage pid = do
                images <- with db $ P.images pid
                if null images then return ""
                               else return $ head images

