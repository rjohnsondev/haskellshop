{-# LANGUAGE OverloadedStrings #-}

module ShopData.Category where

import qualified Snap.Snaplet.PostgresqlSimple as PS
import Database.PostgreSQL.Simple
import Control.Monad
import qualified Data.Text as DT

data Category = Category
    {   categoryId       :: Int,
        name             :: DT.Text,
        parentCategoryId :: Maybe Int,
        orderBy          :: Int
    } deriving (Show)

instance FromRow Category where
        fromRow = Category <$> PS.field <*> PS.field <*> PS.field <*> PS.field

getCategory :: PS.HasPostgres m => Int -> m (Maybe Category)
getCategory cid = do
    results <- PS.query
                "SELECT \
                \    category_id, \
                \    name, \
                \    parent_category_id, \
                \    order_by \
                \FROM \
                \    categories \
                \WHERE \
                \    category_id = ?" (PS.Only cid)
    let r = results :: [Category]
    if length r == 0
        then return Nothing
        else return $ Just (head r)

parent :: PS.HasPostgres m => Category -> m (Maybe Category)
parent c =
    case parentCategoryId c of
        Nothing -> return Nothing
        Just i -> getCategory i

children :: PS.HasPostgres m => Maybe Int -> m [Category]
children cid = do
    results <-
        case cid of
           Nothing -> PS.query_
                        "SELECT \
                        \    category_id, \
                        \    name, \
                        \    parent_category_id, \
                        \    order_by \
                        \FROM \
                        \    categories \
                        \WHERE \
                        \    parent_category_id IS NULL \
                        \ORDER BY order_by ASC" 
           Just i -> PS.query
                        "SELECT \
                        \    category_id, \
                        \    name, \
                        \    parent_category_id,  \
                        \    order_by \
                        \FROM \
                        \    categories \
                        \WHERE \
                        \    parent_category_id = ? \
                        \ORDER BY order_by ASC" (PS.Only i)
    let r = results :: [Category]
    return r

addCategory :: PS.HasPostgres m => DT.Text -> Maybe Int -> m Int
addCategory n pid = do
        r <- PS.returning
                "INSERT INTO categories (name, parent_category_id) \
                \VALUES (?, ?) RETURNING category_id"
                [(n, pid)]
                :: PS.HasPostgres s => s [PS.Only Int]
        let x = case r of (f:_) -> f
                          [] -> PS.Only 0
        let (PS.Only i) = x
        return i

moveCategory :: PS.HasPostgres m => Category -> Bool -> m ()
moveCategory c down = PS.withTransaction $ do
    results <-
        if down then
            case parentCategoryId c
                of Nothing -> PS.query
                                "SELECT \
                                \    category_id, \
                                \    name, \
                                \    parent_category_id, \
                                \    order_by \
                                \FROM \
                                \    categories \
                                \WHERE \
                                \    parent_category_id IS NULL AND \
                                \    order_by > ? \
                                \ORDER BY order_by ASC LIMIT 1" (Only (orderBy c))
                                :: PS.HasPostgres m => m [Category]
                   Just pid -> PS.query 
                                "SELECT \
                                \    category_id, \
                                \    name, \
                                \    parent_category_id, \
                                \    order_by \
                                \FROM \
                                \    categories \
                                \WHERE \
                                \    parent_category_id = ? AND \
                                \    order_by > ? \
                                \ORDER BY order_by ASC LIMIT 1" [pid, orderBy c]
                                :: PS.HasPostgres m => m [Category]
        else
            case parentCategoryId c
                of Nothing -> PS.query
                             "SELECT \
                             \    category_id, \
                             \    name, \
                             \    parent_category_id, \
                             \    order_by \
                             \FROM \
                             \    categories \
                             \WHERE \
                             \    parent_category_id IS NULL AND \
                             \    order_by < ? \
                             \ORDER BY order_by DESC LIMIT 1" (Only (orderBy c))
                             :: PS.HasPostgres m => m [Category]
                   Just pid -> PS.query
                             "SELECT \
                             \   category_id, \
                             \   name, \
                             \   parent_category_id, \
                             \   order_by \
                             \FROM \
                             \   categories \
                             \WHERE \
                             \   parent_category_id = ? AND \
                             \   order_by < ? \
                             \ORDER BY order_by DESC LIMIT 1" [pid, orderBy c]
                             :: PS.HasPostgres m => m [Category]
    unless (null results) $ do
        let sc = head results :: Category
        PS.execute "UPDATE categories SET order_by = ? WHERE category_id = ?" [orderBy c, categoryId sc]
        PS.execute "UPDATE categories SET order_by = ? WHERE category_id = ?" [orderBy sc, categoryId c]
        return ()


delCategory :: PS.HasPostgres m => Int -> Int -> m ()
delCategory cid ncid = PS.withTransaction $ do

    -- if we have products that are on an existing category as well,
    -- just remove this category from their list.
    PS.execute "DELETE FROM product_categories WHERE\
               \     category_id = ? AND\
               \     product_id IN \
               \         (SELECT\
               \              product_id\
               \          FROM\
               \              product_categories\
               \          WHERE\
               \              category_id != ? AND\
               \              product_id IN\
               \                  (SELECT\
               \                      product_id\
               \                   FROM\
               \                      product_categories\
               \                   WHERE\
               \                      category_id = ?))"
               (cid, cid, cid)
                
    -- move the remaining products to the new category
    PS.execute "UPDATE product_categories SET category_id = ? WHERE category_id = ?"
               (ncid, cid)

    -- move any child categories to the new category as well
    PS.execute "UPDATE categories SET parent_category_id = ? WHERE parent_category_id = ?"
               (ncid, cid)

    -- kill off the category!
    PS.execute "DELETE FROM categories WHERE category_id = ?" (Only cid)
    return ()

