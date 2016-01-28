{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Admin.Categories where

import Admin.Feedback
import Application
import Control.Applicative
import FormUtil
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Data.Map as M
import qualified Data.Text.Encoding as DTE
import qualified ShopData.Category as Cat

handleNewCategory :: AppHandler ()
handleNewCategory = do
    name <- getFormByteString "category-name" ""
    pcid <- getFormInt "new-category-parent-id" 0
    let pcidm = if pcid == 0 then Nothing
                             else Just pcid
    with db $ Cat.addCategory (DTE.decodeUtf8 name) pcidm
    infoRedirect "/admin/categories" "New Category Added"

handleDelCategory :: AppHandler ()
handleDelCategory = do
        cid <- getFormInt "del-category-id" 0
        ncid <- getFormInt "del-new-category-id" 0
        resp ncid cid
    where
        resp ncid cid
            | ncid == 0 = dangerRedirect "/admin/categories"
                                         "Refusing to move products to non-existant category"
            | ncid == cid = dangerRedirect "/admin/categories"
                                           "Refusing to move products to category you are deleting!"
            | otherwise = do
                with db $ Cat.delCategory cid ncid
                infoRedirect "/admin/categories" "Category Deleted"

moveCategory :: Bool -> AppHandler ()
moveCategory down = do
    let key = if down then "category-move-down" else "category-move-up"
    cid <- getFormInt key 0
    if cid == 0
        then dangerRedirect "/admin/categories" "Attempted to move missing category"
        else do
            mc <- with db $ Cat.getCategory cid
            case mc of Nothing -> return ()
                       Just c -> with db $ Cat.moveCategory c down
            infoRedirect "/admin/categories" "Category Moved"

handleCategories :: AppHandler ()
handleCategories = method GET handleCategoriesGet <|> method POST handleCategoriesPost
    where
        handleCategoriesGet = cRender "admin/_categories"
        handler p
            | "category-move-down" `M.member` p = moveCategory True
            | "category-move-up" `M.member` p   = moveCategory False
            | "new-category" `M.member` p       = handleNewCategory
            | "del-category-id" `M.member` p    = handleDelCategory
            | otherwise                         = redirect "/admin/categories"
        handleCategoriesPost = do
            p <- getParams
            handler p
