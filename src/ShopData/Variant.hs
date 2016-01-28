{-# LANGUAGE OverloadedStrings #-}

module ShopData.Variant where

import Snap.Snaplet.PostgresqlSimple
import qualified Data.Text as DT
import qualified ShopData.VariantOption as VO

data Variant = Variant
    {   variantId :: Int,
        name :: DT.Text,
        adjustsPrice :: Bool,
        isSearchable :: Bool,
        options :: [VO.VariantOption]
    } deriving (Show)

instance FromRow Variant where
        fromRow = (\i n a s -> (Variant i n a s [])) <$> field <*> field <*> field <*> field

getVariants :: HasPostgres m => m [Variant]
getVariants = do
    results <- query_ "SELECT variant_id, name, adjusts_price, searchable FROM variants"
    let r = results :: [Variant]
    pr <- populateVariantOptions r
    return (pr :: [Variant])

-- TODO: make this betterer
populateVariantOptions :: HasPostgres m => [Variant] -> m [Variant]
populateVariantOptions = foldl (\out v -> do
                                    opts <- getVariantOptions $ variantId v
                                    out' <- out
                                    return $ out' ++ [v { options = opts }])
                               (return [])

getPopulatedVariants :: HasPostgres m => m [Variant]
getPopulatedVariants = do
    results <- query_ "SELECT DISTINCT v.variant_id, v.name, v.adjusts_price, v.searchable FROM variants v INNER JOIN variant_options vo ON v.variant_id = vo.variant_id"
    return (results :: [Variant])

getOptions :: HasPostgres m => Variant -> m [VO.VariantOption]
getOptions x = getVariantOptions $ variantId x

insertNewVariant :: HasPostgres m => Variant -> m Variant
insertNewVariant v = do
    r <- Snap.Snaplet.PostgresqlSimple.query
            "INSERT INTO variants (name, adjusts_price, searchable) VALUES (?, ?, ?) RETURNING variant_id, name, adjusts_price, searchable"
            ((name v)
            , (adjustsPrice v)
            , (isSearchable v))
            :: HasPostgres s => s [Variant]
    return $ head r

updateExistingVariant :: HasPostgres m => Variant -> m Variant
updateExistingVariant v =
        if variantId v < 1
            then fail "Error"
            else do
                r <- Snap.Snaplet.PostgresqlSimple.query
                        "UPDATE variants SET name = ?, adjusts_price = ?, searchable = ? WHERE variant_id = ? RETURNING variant_id, name, adjusts_price, searchable"
                            ((name v)
                            , (adjustsPrice v)
                            , (isSearchable v)
                            , (variantId v))
                        :: HasPostgres s => s [Variant]
                return $ head r

saveVariant :: HasPostgres m => Variant -> m Variant
saveVariant v =
        if variantId v < 1 then insertNewVariant v else updateExistingVariant v

delVariant :: HasPostgres m => Int -> m ()
delVariant i = do
    execute "DELETE FROM variants WHERE variant_id = ?" (Only i)
    return ()

getVariantOptions :: HasPostgres m => Int -> m [VO.VariantOption]
getVariantOptions i = do
    results <- query
                "SELECT\
                \     variant_option_id,\
                \     variant_id,\
                \     option,\
                \     (SELECT COUNT(*) FROM product_variant_options pvo WHERE pvo.variant_option_id = vo.variant_option_id)\
                \ FROM\
                \     variant_options vo\
                \ WHERE\
                \     variant_id = ?"
                (Only i) :: HasPostgres m => m [VO.VariantOption]
    return (results :: [VO.VariantOption])
