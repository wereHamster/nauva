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
import           Data.Yaml
import           Data.Monoid

import           Text.Markdown         (def)
import           Text.Markdown.Block
import           Text.Markdown.Inline

import           Language.Haskell.TH         hiding (Inline)
import           Language.Haskell.TH.Quote
import           Language.Haskell.Meta.Parse (parseExp)
import           Instances.TH.Lift ()

import           Nauva.View hiding (Exp)
import           Nauva.Catalog.Elements



markdownBlocks :: ByteString -> [Block [Inline]]
markdownBlocks = markdownBlocksT . T.decodeUtf8

markdownBlocksT :: Text -> [Block [Inline]]
markdownBlocksT = map (fmap $ toInline mempty) . parseMarkdown
  where
    parseMarkdown md = runIdentity (yield md $$ toBlocks def =$ CL.consume)


data MState = NoState | InList ListType [Exp]

renderBlocks :: Bool -> [Block [Inline]] -> Q Exp -- Q [Element]
renderBlocks isTopLevel = fmap ListE . go NoState
  where
    go :: MState -> [Block [Inline]] -> Q [Exp]
    go s [] = case s of
        NoState              -> pure []
        (InList listType es) -> do
            es' <- case listType of
                Ordered   -> appE [| \x -> [pageOL $ mconcat x] |] (pure $ ListE es)
                Unordered -> appE [| \x -> [pageUL $ mconcat x] |] (pure $ ListE es)
            pure [es']

    go NoState (b@(BlockList listType _):bs) = do
        e <- renderBlock isTopLevel b
        go (InList listType [e]) bs

    go NoState (b:bs) = do
        e <- renderBlock isTopLevel b
        rest <- go NoState bs
        pure $ [e] <> rest

    go (InList inListType es) (b@(BlockList listType _):bs) = if inListType == listType
        then do
            e <- renderBlock isTopLevel b
            go (InList listType (es <> [e])) bs
        else do
            es' <- case inListType of
                Ordered   -> appE [| \x -> [pageOL $ mconcat x] |] (pure $ ListE es)
                Unordered -> appE [| \x -> [pageUL $ mconcat x] |] (pure $ ListE es)
            e <- renderBlock isTopLevel b
            rest <- go (InList listType [e]) bs
            pure $ [es'] <> rest

    go (InList listType es) (b:bs) = do
        es' <- case listType of
            Ordered   -> appE [| \x -> [pageOL $ mconcat x] |] (pure $ ListE es)
            Unordered -> appE [| \x -> [pageUL $ mconcat x] |] (pure $ ListE es)
        e <- renderBlock isTopLevel b
        rest <- go NoState bs
        pure $ [es', e] <> rest


renderBlock :: Bool -> Block [Inline] -> Q Exp -- Q [Element]
renderBlock isTopLevel b = case b of
    (BlockPara is) ->
        appE [| \x -> [pageParagraph isTopLevel $ mconcat x] |] (ListE <$> mapM renderInline is)

    (BlockHeading level is) -> case level of
        1 -> appE [| \x -> [pageH2 $ mconcat x] |] (ListE <$> mapM renderInline is)
        2 -> appE [| \x -> [pageH3 $ mconcat x] |] (ListE <$> mapM renderInline is)
        3 -> appE [| \x -> [pageH4 $ mconcat x] |] (ListE <$> mapM renderInline is)
        _ -> appE [| \x -> [pageH4 $ mconcat x] |] (ListE <$> mapM renderInline is)

    (BlockPlainText is) ->
        appE [| mconcat |] (ListE <$> mapM renderInline is)

    (BlockQuote is) ->
        appE [| \x -> [pageBlockquote $ mconcat x] |] (ListE <$> mapM (renderBlock False) is)

    (BlockCode mbType str) -> case mbType of
        Nothing -> [| [pageCodeBlock str] |]
        Just "nauva" -> do
            let pepDef = PageElementProps {pepTitle = Nothing, pepSpan = 6}
            let cspDef = CodeSpecimenProps {cspPEP = pepDef, cspNoSource = False }
            (csp, expr, str2) <- case T.splitOn "---\n" str of
                [str1] -> case parseExp (T.unpack str1) of
                    Left err -> do
                        err1 <- [| div_ [str_ (T.pack err)] |]
                        pure (cspDef, err1, str)
                    Right expr -> pure (cspDef, expr, str1)

                [rawYAML, str1] -> case decodeEither (T.encodeUtf8 rawYAML) of
                        Left yamlErr -> do
                            err1 <- [| div_ [str_ $ T.pack $ show yamlErr] |]
                            pure (cspDef, err1, str1)
                        Right csp -> case parseExp (T.unpack str1) of
                            Left err -> do
                                err1 <- [| div_ [str_ (T.pack err)] |]
                                pure (csp, err1, str)
                            Right expr -> pure (csp, expr, str1)
                _ -> do
                    expr <- [| div_ [str_ "Unrecognized expression"] |]
                    pure (cspDef, expr, str)

            appE [| \c -> [pageElement (cspPEP csp) [codeSpecimen csp c "Haskell" str2]] |] (pure expr)
        Just "hint" -> do
            let blocks = markdownBlocksT str
            children <- ListE <$> mapM (renderBlock False) blocks
            appE [| \x -> [pageHint $ mconcat x] |] (pure children)

        Just "element" -> do
            el <- case parseExp (T.unpack str) of
                Left err -> [| div_ [str_ (T.pack err)] |]
                Right expr -> pure expr

            appE [| (: []) |] (pure el)

        _ -> [| [pageCodeBlock str] |]

    (BlockList Ordered inlineOrBlocks) ->
        appE [| \x -> [li_ $ mconcat x] |] $ case inlineOrBlocks of
            Left is -> ListE <$> mapM renderInline is
            Right bs -> ListE <$> mapM (renderBlock False) bs

    (BlockList Unordered inlineOrBlocks) ->
        appE [| \x -> [li_ $ mconcat x] |] $ case inlineOrBlocks of
            Left is -> ListE <$> mapM renderInline is
            Right bs -> ListE <$> mapM (renderBlock False) bs

    (BlockHtml _) ->
        [| [div_ [str_ "TODO: BlockHtml"]] |]

    BlockRule ->
        [| [hr_ []] |]

    (BlockReference _ _) ->
        [| [div_ [str_ "TODO: BlockReference"]] |]


renderInline :: Inline -> Q Exp -- [Q Element]
renderInline i = case i of
    (InlineText str) ->
        [| [str_ str] |]

    (InlineItalic is) ->
        appE [| \x -> [i_ $ mconcat x] |] (ListE <$> mapM renderInline is)

    (InlineBold is) ->
        appE [| \x -> [strong_ $ mconcat x] |] (ListE <$> mapM renderInline is)

    (InlineLink url _title is) ->
        appE [| \x -> [a_ [href_ url] (mconcat x)] |] (ListE <$> mapM renderInline is)

    (InlineCode str) ->
        [| [pageCode [str_ str]] |]

    (InlineHtml _) ->
        [| [str_ "TODO: InlineHtml"] |]

    (InlineImage url _ _) ->
        [| [img_ [src_ url]] |]

    (InlineFootnoteRef _) ->
        [| [str_ "TODO: InlineFootnoteRef"] |]

    (InlineFootnote _) ->
        [| [str_ "TODO: InlineFootnote"] |]


catalogPage :: ByteString -> Q Exp -- Element
catalogPage bs = do
    children <- renderBlocks True (markdownBlocks bs)

    appE [| pageRoot . mconcat |] (pure children)


nauvaCatalogPage :: QuasiQuoter
nauvaCatalogPage = QuasiQuoter
    { quoteExp  = catalogPage . T.encodeUtf8 . T.pack
    , quotePat  = error "nauvaCatalogPage: quotePat"
    , quoteType = error "nauvaCatalogPage: quoteType"
    , quoteDec  = error "nauvaCatalogPage: quoteDec"
    }
