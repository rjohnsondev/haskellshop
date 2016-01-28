{-# LANGUAGE OverloadedStrings #-}

module ShopData.VariantOption where

import qualified Snap.Snaplet.PostgresqlSimple as PS
import Database.PostgreSQL.Simple
import Data.Text

data VariantOption = VariantOption
    {   variantOptionId  :: Int,
        variantId        :: Int,
        option           :: Text,
        productsUsing    :: Int
    } deriving (Show)

instance FromRow VariantOption where
        fromRow = VariantOption <$> PS.field <*> PS.field <*> PS.field <*> PS.field

variantOptions :: PS.HasPostgres m => [Int] -> m [VariantOption]
variantOptions voIds = do
    results <- PS.query
                "SELECT\
                \     variant_option_id,\
                \     variant_id,\
                \     option,\
                \     (SELECT COUNT(*) FROM product_variant_options pvo WHERE pvo.variant_option_id = vo.variant_option_id)\
                \ FROM\
                \     variant_options vo\
                \ WHERE\
                \     variant_option_id IN ?"
                (Only $ In voIds) :: PS.HasPostgres m => m [VariantOption]
    return (results :: [VariantOption])

addVariantOption :: PS.HasPostgres s3 => Int -> Text -> s3 Int
addVariantOption parentId opt = do
    r <- PS.returning
            "INSERT INTO variant_options (variant_id, option) VALUES (?, ?) RETURNING variant_option_id"
            [(parentId, opt)]
            :: PS.HasPostgres s => s [Only Int]
    let x = case r of (f:_) -> f
                      [] -> Only 0
    let (Only i) = x
    return i

delVariantOption :: PS.HasPostgres m => Int -> m ()
delVariantOption i = do
    PS.execute "DELETE FROM variant_options WHERE variant_option_id = ?" (Only i)
    return ()

