{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nauva.Catalog.TH
    ( catalogPage
    , nauvaCatalogPage

    , renderInline
    , renderBlock
    ) where


import           Data.ByteString      (ByteString)
import           Data.Text            (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import           Data.Functor.Identity (runIdentity)

import           Text.Markdown         (def, MarkdownSettings(..))
import           Text.Markdown.Block
import           Text.Markdown.Inline

import           Language.Haskell.TH         hiding (Inline)
import           Language.Haskell.TH.Quote
import           Language.Haskell.Meta.Parse (parseExp)
import           Instances.TH.Lift ()

import           Nauva.View
import           Nauva.CSS



markdownBlocks :: ByteString -> [Block [Inline]]
markdownBlocks = markdownBlocksT . T.decodeUtf8

markdownBlocksT :: Text -> [Block [Inline]]
markdownBlocksT = map (fmap $ toInline mempty) . parseMarkdown
  where
    parseMarkdown md = runIdentity (yield md $$ toBlocks def =$ CL.consume)


renderBlock :: Block [Inline] -> Q Exp -- [Q Element]
renderBlock (BlockPara x) = do
    appE [| \x -> [pageParagraph $ mconcat x] |] (ListE <$> mapM renderInline x)

renderBlock (BlockHeading level x) = case level of
    1 -> appE [| \x -> [pageH2 $ mconcat x] |] (ListE <$> mapM renderInline x)
    2 -> appE [| \x -> [pageH3 $ mconcat x] |] (ListE <$> mapM renderInline x)
    3 -> appE [| \x -> [pageH4 $ mconcat x] |] (ListE <$> mapM renderInline x)
    _ -> appE [| \x -> [pageH4 $ mconcat x] |] (ListE <$> mapM renderInline x)

renderBlock (BlockPlainText x) =
    appE [| mconcat |] (ListE <$> mapM renderInline x)

renderBlock (BlockQuote x) =
    appE [| \x -> [pageBlockquote $ mconcat x] |] (ListE <$> mapM renderBlock x)

renderBlock (BlockCode mbType x) = case mbType of
    Nothing -> [| [pageCodeBlock x] |]
    Just "nauva" -> do
        exp <- case parseExp (T.unpack x) of
            Left e  -> [| div_ [str_ (T.pack e)] |]
            Right x -> pure x
        appE [| \c -> [pageElementContainer [c], pageCodeBlock x] |] (pure exp)
    Just "hint" -> do
        let blocks = markdownBlocksT x
        children <- ListE <$> mapM renderBlock blocks
        appE [| \x -> [pageHint $ mconcat x] |] (pure children)

    _ -> [| [pageCodeBlock x] |]


renderInline :: Inline -> Q Exp -- [Q Element]
renderInline (InlineText x) = [| [str_ x] |]
renderInline (InlineItalic x) = appE [| \x -> [i_ $ mconcat x] |] (ListE <$> mapM renderInline x)
renderInline (InlineBold x) = appE [| \x -> [strong_ $ mconcat x] |] (ListE <$> mapM renderInline x)
renderInline (InlineLink url mtitle content) = appE [| \x -> [a_ [href_ url] (mconcat x)] |] (ListE <$> mapM renderInline content)
renderInline (InlineCode x) = [| [pageCode [str_ x]] |]


catalogPage :: ByteString -> Q Exp -- Element
catalogPage bs = do
    children <- ListE <$> mapM renderBlock (markdownBlocks bs)

    appE [| pageRoot . mconcat |] (pure children)


nauvaCatalogPage :: QuasiQuoter
nauvaCatalogPage = QuasiQuoter
    { quoteExp  = catalogPage . T.encodeUtf8 . T.pack
    , quotePat  = error "nauvaCatalogPage: quotePat"
    , quoteType = error "nauvaCatalogPage: quoteType"
    , quoteDec  = error "nauvaCatalogPage: quoteDec"
    }
