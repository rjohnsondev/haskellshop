{-# LANGUAGE OverloadedStrings #-}

module ShopData.Product where

import qualified Snap.Snaplet.PostgresqlSimple as PS
import Database.PostgreSQL.Simple
import Data.Scientific
import Control.Monad
import Control.Monad.IO.Class
import qualified ShopData.Thumbnail as T
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString as BS
import qualified Crypto.Hash as H
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified ShopData.ProductVariantOption as PVO
import qualified ShopData.ProductDetail as PD

data Product = Product
    {   productId    :: Int,
        name         :: DT.Text,
        manufacturer :: DT.Text,
        basePrice    :: Scientific,
        enabled      :: Bool
    } deriving (Show)

imagePath :: String
imagePath = "./static/img/products/"

instance FromRow Product where
        fromRow = Product <$> PS.field <*> PS.field <*> PS.field <*> PS.field <*> PS.field

getProduct :: PS.HasPostgres m => Int -> m (Maybe Product)
getProduct pid = do
    results <- PS.query
        "SELECT \
        \    product_id, \
        \    name, \
        \    manufacturer, \
        \    base_price, \
        \    enabled \
        \FROM \
        \    products \
        \WHERE \
        \    product_id = ?" (PS.Only pid)
    let p = results :: [Product]
    return $ if length p == 0 then Nothing
                              else Just $ head p

productVariantOptions :: PS.HasPostgres m => Int -> m [PVO.ProductVariantOption]
productVariantOptions pid = do
    results <- PS.query "SELECT \
                        \    pvo.product_id, \
                        \    v.variant_id, \
                        \    v.name, \
                        \    vo.variant_option_id, \
                        \    vo.option, \
                        \    pvo.price_adjustment \
                        \FROM \
                        \    product_variant_options pvo \
                        \    NATURAL JOIN variant_options vo \
                        \    NATURAL JOIN variants v \
                        \WHERE \
                        \    product_id = ?" (PS.Only pid)
    let r = results :: [PVO.ProductVariantOption]
    return r

categories :: PS.HasPostgres m => Int -> m [Int]
categories pid = do

    results <- PS.query "SELECT \
                        \    category_id \
                        \FROM \
                        \    product_categories \
                        \WHERE \
                        \    product_id = ?" (PS.Only pid)
    let r = results :: [Only Int]
    return $ map (\x -> fromOnly x) r


saveProduct :: PS.HasPostgres m => Product -> m Int
saveProduct p = do
        r <- if (productId p) == 0
                 then PS.query
                         "INSERT INTO products (name, manufacturer, base_price, enabled) \
                         \ VALUES (?, ?, ?, ?) RETURNING product_id"
                         (name p, manufacturer p, basePrice p, enabled p)
                 else PS.query
                         "UPDATE products SET\
                         \     name = ?, \
                         \     manufacturer = ?, \
                         \     base_price = ?, \
                         \     enabled = ? \
                         \ WHERE \
                         \     product_id = ? \
                         \ RETURNING product_id"
                         (name p, manufacturer p, basePrice p, enabled p, productId p)
                 :: PS.HasPostgres s => s [PS.Only Int]
        let x = case r of (f:_) -> f
                          [] -> PS.Only 0
        let (PS.Only i) = x
        return i

saveCategories :: PS.HasPostgres m => Int -> [Int] -> m ()
saveCategories pid cats = PS.withTransaction $ do
    PS.execute "DELETE FROM product_categories WHERE product_id = ?" (Only pid)

    -- we need to ensure all parent categories are also included...
    PS.execute     "WITH RECURSIVE cats(category_id, name, parent_category_id, order_by) AS (\
                   \    SELECT category_id, name, parent_category_id, order_by FROM categories WHERE category_id IN ?\
                   \  UNION\
                   \    SELECT c.category_id, c.name, c.parent_category_id, c.order_by\
                   \    FROM categories c, cats\
                   \    WHERE cats.parent_category_id = c.category_id\
                   \  )\
                   \  INSERT INTO product_categories (product_id, category_id)\
                   \     (SELECT ?, category_id FROM cats);"
        $ (In cats, pid)
    return ()

saveProductVariantOptions :: PS.HasPostgres m => Int -> [(Int, Scientific)] -> m ()
saveProductVariantOptions pid pvos = PS.withTransaction $ do
    PS.execute "DELETE FROM product_variant_options WHERE product_id = ?" (Only pid)
    PS.executeMany
        "INSERT INTO product_variant_options (product_id, variant_option_id, price_adjustment) \
        \ VALUES (?, ?, ?)"
        $ map (\(voId, adj) -> (pid, voId, adj)) pvos
    return ()

addImage :: PS.HasPostgres m => Int -> FilePath -> m ()
addImage pid fp = do
        fs <- liftIO $ C8.readFile fp
        let hh = H.hashlazy fs :: H.Digest H.SHA256
            h = DT.pack . show $ hh
        liftIO $ copyFile fp $ "./static/img/products/" ++ show hh ++ "_orig.jpg"
        t <- liftIO $ T.mkThumbnail' ((100, 100), (170, 170)) fs
        case t of Left s -> liftIO $ print (s :: String)
                  Right n -> liftIO $ T.saveFile (n :: T.Thumbnail) (imagePath ++ show hh ++ "_med.jpg") 
        t' <- liftIO $ T.mkThumbnail' ((70, 70), (85, 85)) fs
        case t' of Left s -> liftIO $ print (s :: String)
                   Right n -> liftIO $ T.saveFile (n :: T.Thumbnail) (imagePath ++ show hh ++ "_sml.jpg") 
        PS.execute "INSERT INTO product_images (hash, product_id) VALUES (?, ?)" (h, pid)
        return ()

deleteImage :: PS.HasPostgres m => Int -> BS.ByteString -> m ()
deleteImage pid hash = do
        PS.execute "DELETE FROM product_images WHERE hash = ? AND product_id = ?"
                (DTE.decodeUtf8 hash, pid)
        return ()

moveImage :: PS.HasPostgres m => Int -> Bool -> BS.ByteString -> m ()
moveImage pid down h = PS.withTransaction $ do
    r <- PS.query "SELECT order_by FROM product_images WHERE hash = ?" (Only h)
            :: PS.HasPostgres m => m [[Int]]
    unless (null r) $ do
        let o = head $ head r
        results <- if down then PS.query "SELECT \
                                         \    hash, \
                                         \    order_by \
                                         \FROM \
                                         \    product_images \
                                         \WHERE \
                                         \    product_id = ? AND \
                                         \    order_by > ? \
                                         \ORDER BY order_by ASC LIMIT 1" [pid, o]
                                         :: PS.HasPostgres m => m [(DT.Text, Int)]
                           else PS.query "SELECT \
                                         \    hash, \
                                         \    order_by \
                                         \FROM \
                                         \    product_images \
                                         \WHERE \
                                         \    product_id = ? AND \
                                         \    order_by < ? \
                                         \ORDER BY order_by DESC LIMIT 1" [pid, o]
                                         :: PS.HasPostgres m => m [(DT.Text, Int)]
        unless (null results) $ do
                let (h', o') = head results :: (DT.Text, Int)
                PS.execute "UPDATE product_images SET order_by = ? WHERE product_id = ? AND hash = ?" (o', pid, h)
                PS.execute "UPDATE product_images SET order_by = ? WHERE product_id = ? AND hash = ?" (o, pid, h')
                return ()

images :: PS.HasPostgres m => Int -> m [DT.Text]
images pid = do
        r <- PS.query "SELECT hash FROM product_images WHERE product_id = ? ORDER BY order_by ASC"
                (Only pid)
                :: PS.HasPostgres m => m [[DT.Text]]
        return $ map head r

details :: PS.HasPostgres m => Int -> m [PD.ProductDetail]
details pid =
        PS.query "SELECT product_detail_id, product_id, title, detail FROM product_details WHERE product_id = ? ORDER BY order_by ASC"
            (Only pid)

products :: PS.HasPostgres m => Maybe Int -> [Int] -> m [Product]
products mcat pvos =
        if length pvos == 0
            then case mcat of
                 Nothing -> PS.query_
                        "SELECT\
                        \   p.product_id, p.name, p.manufacturer, p.base_price, p.enabled\
                        \ FROM products p"
                 Just cat -> PS.query
                        "SELECT \
                        \    product_id, name, manufacturer, base_price, enabled\
                        \ FROM \
                        \    products\
                        \    NATURAL JOIN product_categories\
                        \ WHERE\
                        \    category_id = ?"
                        (Only cat)
            else case mcat of
                 Nothing -> PS.query
                       "SELECT \
                       \    product_id, name, manufacturer, base_price, enabled\
                       \ FROM \
                       \    products\
                       \    NATURAL JOIN product_variant_options\
                       \ WHERE\
                       \    variant_option_id IN ?"
                       (Only $ In pvos)
                 Just cat -> PS.query
                       "SELECT \
                       \    product_id, name, manufacturer, base_price, enabled\
                       \ FROM \
                       \    products\
                       \    NATURAL JOIN product_categories\
                       \    NATURAL JOIN product_variant_options\
                       \ WHERE\
                       \    category_id = ?\
                       \    AND variant_option_id IN ?"
                       (cat, In pvos)

