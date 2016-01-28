{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Render.Categories where

import HeistUtil
import Application
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Control.Monad
import Control.Monad.Trans
import FormUtil
import Heist
import Heist.Interpreted as I
import Render.Product (productRuntime)
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Data.Set as S
import qualified Data.Text as DT
import qualified Heist.Compiled as C
import qualified ShopData.Category as Cat
import qualified ShopData.Product as P
import qualified Text.XmlHtml as X

-- Category Tree

interpretedRenderCategory :: Cat.Category -> I.Splice AppHandler
interpretedRenderCategory x = do
        c <- lift $ with db $ Cat.children $ Just (Cat.categoryId x)
        I.callTemplate "_interpreted_categories_children"
            (do "category" ##
                    I.runChildrenWith
                        (do "children" ## interpretedRenderCategories c
                            "name"     ## I.textSplice (Cat.name x)
                            "id"       ## I.textSplice (DT.pack . show $ Cat.categoryId x))
                "categories" ## I.textSplice "")

interpretedRenderCategories :: [Cat.Category] -> I.Splice AppHandler
interpretedRenderCategories = I.mapSplices interpretedRenderCategory

interpretedRenderRootCategories :: I.Splice AppHandler
interpretedRenderRootCategories = do
        c <- lift $ with db $ Cat.children Nothing
        interpretedRenderCategories c

categoryTree :: Handler App App Builder
categoryTree = do
        s <- getHeistState
        let y = bindSplices (do "placeholder" ## interpretedRenderRootCategories) s
        t <- I.renderTemplate y "admin/_interpreted" 
        case t of Just (b, _) -> return b
                  Nothing -> return $ fromString  "Unable to find _interpreted template!!"

categoryTreeRuntime :: RuntimeSplice (Handler App App) Builder
categoryTreeRuntime = lift categoryTree

renderCategoriesCompiled :: C.Splice AppHandler
renderCategoriesCompiled = return $ C.yieldRuntime categoryTreeRuntime

-- Category list for drop-downs

getCategoryList :: RuntimeSplice AppHandler [(Int, DT.Text)]
getCategoryList = do
        cats <- lift $ with db $ Cat.children Nothing
        titles <- mapM (getCategoryListChildren "") cats
        return $ concat titles

getCategoryListChildren :: DT.Text -> Cat.Category -> RuntimeSplice AppHandler [(Int, DT.Text)]
getCategoryListChildren prefix c = do
        cats <- lift $ with db $ Cat.children $ Just (Cat.categoryId c)
        let title = DT.concat [prefix, Cat.name c]
        titles <- mapM (getCategoryListChildren (DT.concat [title, " / "])) cats
        return $ concat ([((Cat.categoryId c), title)] : titles)

renderCategoryListCheckbox :: RuntimeSplice AppHandler (Int, DT.Text, Bool) -> X.Node -> RuntimeSplice AppHandler X.Node
renderCategoryListCheckbox catm n = catm >>= \(cid, _, chk) ->
        return . setChecked chk
               . setValue cid
               $ n
    where
        setValue v = X.setAttribute "value" (DT.pack . show $ v)
        setChecked b n' = if b then X.setAttribute "checked" "" n' else n'

renderCategoryList :: RuntimeSplice AppHandler [(Int, DT.Text, Bool)] -> C.Splice AppHandler
renderCategoryList d =
        C.manyWithSplices C.runChildren 
            (do "category_name"     ## pt (\(_, t, _) -> t)
                "category_checkbox" ## templatedNodeSplice . renderCategoryListCheckbox
                "category_id"       ## pt (\(i, _, _) -> DT.pack. show $ i))
            d
    where
        pt = C.pureSplice . escapingTextSplice

categoryListRuntime :: RuntimeSplice AppHandler [(Int, DT.Text, Bool)]
categoryListRuntime = do
        c <- getCategoryList
        p <- productRuntime
        pc <- case p of Nothing -> return [] :: RuntimeSplice AppHandler [Int]
                        Just p' -> lift $ with db $ P.categories (P.productId p')
        let pcs = S.fromList pc
        return $ map (\(i, t) -> (i, t, (S.member i pcs))) c

-- front-end category rendering

type CategoryViewModel = ( Int -- category id
                         , DT.Text -- cateogry name
                         , DT.Text) -- html element class

renderCategorySplices :: Splices (RuntimeSplice AppHandler CategoryViewModel -> C.Splice AppHandler)
renderCategorySplices =
        (do "category_name"  ## pt (\(_, t, _) -> t)
            "category_id"    ## pt (\(i, _, _) -> DT.pack . show $ i)
            "category_class" ## pt (\(_, _, c) -> c))
    where
        pt = C.pureSplice . escapingTextSplice

renderCategories :: RuntimeSplice AppHandler [CategoryViewModel] -> C.Splice AppHandler
renderCategories = C.manyWithSplices C.runChildren renderCategorySplices

renderCategory :: RuntimeSplice AppHandler CategoryViewModel -> C.Splice AppHandler
renderCategory = C.withSplices C.runChildren renderCategorySplices

categoriesTopLevelRuntime :: RuntimeSplice AppHandler [CategoryViewModel]
categoriesTopLevelRuntime =
        liftM (map (\x -> (Cat.categoryId x, Cat.name x, "")))
              (lift $ with db $ Cat.children Nothing)

categoriesRuntime :: RuntimeSplice AppHandler [CategoryViewModel]
categoriesRuntime = do
        cid <- lift $ getFormInt "c" 0
        cats <- lift $ with db $ Cat.children (if cid == 0 then Nothing else Just cid)
        return $ map (\x -> (Cat.categoryId x, Cat.name x, "")) cats

renderHideCategories :: RuntimeSplice AppHandler [CategoryViewModel] -> C.Splice AppHandler
renderHideCategories x =
        return $ C.yieldRuntimeText (x >>= (\cvms -> if null cvms then return "display: none;" else return ""))

categoryParent :: Cat.Category -> [CategoryViewModel] -> RuntimeSplice AppHandler [CategoryViewModel]
categoryParent c lst = do
        mp <- lift $ with db $ Cat.parent c
        case mp of Nothing -> return $ lst
                   Just p -> categoryParent p ((Cat.categoryId p, Cat.name p, "") : lst)

categoryCrumbtrailRuntime :: RuntimeSplice AppHandler [CategoryViewModel]
categoryCrumbtrailRuntime = do
        cid <- lift $ getFormInt "c" 0
        pid <- lift $ getFormInt "product_id" 0
        mc <- lift $ with db $ Cat.getCategory cid
        case mc of Nothing -> do cats <- lift $ with db $ P.categories pid
                                 if null cats
                                     then return []
                                     else do mc' <- lift $ with db $ Cat.getCategory $ head cats
                                             case mc' of Nothing  -> return []
                                                         Just c -> categoryParent c [(Cat.categoryId c, Cat.name c, "active")]
                   Just c -> categoryParent c [(Cat.categoryId c, Cat.name c, "active")]

renderCategoryId :: C.Splice AppHandler
renderCategoryId = C.pureSplice (escapingTextSplice id) categoryIdRuntime

categoryIdRuntime :: RuntimeSplice AppHandler DT.Text
categoryIdRuntime = do
        bm <- lift $ getFormInt "c" 0
        return $ DT.pack . show $ bm

categoryRuntime :: RuntimeSplice AppHandler CategoryViewModel
categoryRuntime = do
        cid <- lift $ getFormInt "c" 0
        mc <- lift $ with db $ Cat.getCategory cid
        case mc of Nothing -> return $ (0, "Home", "")
                   Just cat -> return $ (Cat.categoryId cat, Cat.name cat, "")

