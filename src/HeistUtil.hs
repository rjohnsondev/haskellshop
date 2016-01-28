{-# LANGUAGE OverloadedStrings #-}

module HeistUtil where

import qualified Text.XmlHtml as X
import qualified Heist.Compiled as C
import qualified Data.Text as DT
import qualified Blaze.ByteString.Builder as BB
import Heist
import Data.Scientific
-- import Data.ByteString.Builder
import Control.Monad

------------------------------------------------------------------------------
-- | Splice wrapped around a HTML node transform; the first child node in the
-- block is passed as the param to the RuntimeSplice function, allowing for
-- runtime injection of an updated node.
-- 
--   > <splice_key>
--   >   <node template="passed" to="runtime" function />
--   > </splice_key>
--
templatedNodeSplice :: Monad n => (X.Node -> RuntimeSplice n X.Node) -> C.Splice n
templatedNodeSplice f = do
        paramNode <- getParamNode
        let n = Prelude.head $ X.childElements paramNode
        return $ C.yieldRuntime $ liftM render (f n)
    where
        render x = X.renderHtmlFragment X.UTF8 [x]

formatPrice :: Scientific -> String
formatPrice num = "£" ++ (formatScientific Fixed (Just 2) num)


escapingTextSplice :: (a -> DT.Text) -> a -> BB.Builder
escapingTextSplice f a = C.nodeSplice (\b -> [X.TextNode b]) (f a)
