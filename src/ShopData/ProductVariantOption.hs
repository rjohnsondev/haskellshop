{-# LANGUAGE OverloadedStrings #-}

module ShopData.ProductVariantOption where

import Snap.Snaplet.PostgresqlSimple
import Data.Text
import Data.Scientific

data ProductVariantOption = ProductVariantOption
    { productId         :: Int,
      variantId         :: Int,
      variantName       :: Text,
      variantOptionId   :: Int,
      variantOptionName :: Text,
      priceAdjustment   :: Scientific
    } deriving (Show)

instance FromRow ProductVariantOption where
        fromRow = ProductVariantOption <$> field <*> field <*> field <*> field <*> field <*> field

