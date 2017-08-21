{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Nauva.Catalog.TH
    ( catalogPage
    , catalogPageFromFile
    , nauvaCatalogPage

    , renderInline
    , renderBlock
    ) where


import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
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
import           Language.Haskell.TH.Syntax (Lift(lift))
import           Language.Haskell.Meta.Parse (parseExp)

import           Instances.TH.Lift ()

import           System.FilePath

import           Nauva.View
import           Nauva.Catalog.Elements
import           Nauva.Catalog.Specimens.CodeSpecimen
import           Nauva.Catalog.Specimens.NauvaSpecimen


instance Lift F where
    lift (F{..}) = do
        fId' <- lift fId
        fConstructors' <- lift fConstructors
        fArguments' <- lift fArguments
        fBody' <- lift fBody
        pure $ AppE (AppE (AppE (AppE (ConE 'Tag) fId') fConstructors') fArguments') fBody'

instance Lift FID where
    lift (FID x) = AppE (ConE 'FID) <$> lift x

instance Lift Tag where
    lift (Tag x) = AppE (ConE 'Tag) <$> lift x

instance Lift Suffix where
    lift (Suffix x) = AppE (ConE 'Suffix) <$> lift x

instance Lift Hash where
    lift (Hash x) = AppE (ConE 'Hash) <$> lift x

instance Lift CSSValue where
    lift (CSSValue x) = AppE (ConE 'CSSValue) <$> lift x

instance Lift Condition where
    lift (CMedia x) = AppE (ConE 'CMedia) <$> lift x
    lift (CSupports x) = AppE (ConE 'CSupports) <$> lift x

instance Lift AttributeValue where
    lift (AVBool x)   = AppE (ConE 'AVBool)   <$> lift x
    lift (AVString x) = AppE (ConE 'AVString) <$> lift x
    lift (AVInt x)    = AppE (ConE 'AVInt)    <$> lift x
    lift (AVDouble x) = AppE (ConE 'AVDouble) <$> lift x

instance Lift Style where
    lift (Style x) = AppE (ConE 'Style) <$> lift x

instance Lift CSSRule where
    lift (CSSStyleRule name hash conditions suffixes styleDeclarations) = do
        name' <- lift name
        hash' <- lift hash
        conditions' <- lift conditions
        suffixes' <- lift suffixes
        styleDeclarations' <- lift styleDeclarations

        pure $ foldl1 AppE
            [ (ConE 'CSSStyleRule)
            , name'
            , hash'
            , conditions'
            , suffixes'
            , styleDeclarations'
            ]

    lift (CSSFontFaceRule hash styleDeclarations) = undefined

instance Lift Attribute where
    lift (AVAL t av) = do
        t' <- lift t
        av' <- lift av
        pure $ AppE (AppE (ConE 'AVAL) t') av'

    lift (AEVL (EventListener t f)) = do
        t' <- lift t
        f' <- lift f
        pure $ AppE (ConE 'AEVL) (AppE (AppE (ConE 'EventListener) t') f')

    lift (ASTY style) = do
        style' <- lift style
        pure $ AppE (ConE 'ASTY) style'

instance Lift Element where
    lift e = case e of
        ENull -> pure (ConE 'ENull)
        EText str -> (AppE (ConE 'EText)) <$> lift str
        ENode tag attrs children -> do
            tag' <- lift tag
            attrs' <- lift attrs
            children' <- lift children

            pure $ foldl1 AppE
                [ (ConE 'ENode)
                , tag'
                , attrs'
                , children'
                ]

        EThunk _ _ -> error "EThunk"
        EComponent _ _ -> error "EComponent"


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
        lift [pageParagraph isTopLevel $ concatMap renderInline is]

    (BlockHeading level is) -> case level of
        1 -> lift [pageH2 $ concatMap renderInline is]
        2 -> lift [pageH3 $ concatMap renderInline is]
        3 -> lift [pageH4 $ concatMap renderInline is]
        _ -> lift [pageH4 $ concatMap renderInline is]

    (BlockPlainText is) ->
        lift $ concatMap renderInline is

    (BlockQuote is) ->
        appE [| \x -> [pageBlockquote $ mconcat x] |] (ListE <$> mapM (renderBlock False) is)

    (BlockCode mbType str) -> case mbType of
        Nothing -> lift [pageCodeBlock "" str]
        Just tag -> case T.splitOn "|" tag of
            [tag'] -> case tag' of
                "code" -> lift [pageCodeBlock "" str]
                "nauva" -> do
                    let pepDef = PageElementProps {pepTitle = Nothing, pepSpan = 6}
                    let cspDef = CodeSpecimenProps {cspPEP = pepDef, cspNoSource = False }
                    (csp, expr, str2) <- case T.splitOn "---\n" str of
                        [str1] -> case parseExp (T.unpack str1) of
                            Left err -> do
                                err1 <- lift $ div_ [str_ (T.pack err)]
                                pure (cspDef, err1, str)
                            Right expr -> pure (cspDef, expr, str1)

                        [rawYAML, str1] -> case decodeEither (T.encodeUtf8 rawYAML) of
                                Left yamlErr -> do
                                    err1 <- lift $ div_ [str_ $ T.pack $ show yamlErr]
                                    pure (cspDef, err1, str1)
                                Right csp -> case parseExp (T.unpack str1) of
                                    Left err -> do
                                        err1 <- lift $ div_ [str_ (T.pack err)]
                                        pure (csp, err1, str)
                                    Right expr -> pure (csp, expr, str1)
                        _ -> do
                            expr <- lift $ div_ [str_ "Unrecognized expression"]
                            pure (cspDef, expr, str)

                    appE [| \c -> [pageElement (cspPEP csp) [nauvaSpecimen $ NauvaSpecimen csp c "Haskell" str2]] |] (pure expr)
                "hint" -> do
                    let blocks = markdownBlocksT str
                    children <- ListE <$> mapM (renderBlock False) blocks
                    appE [| \x -> [pageHint $ mconcat x] |] (pure children)

                "element" -> do
                    el <- case parseExp (T.unpack str) of
                        Left err -> lift $ div_ [str_ (T.pack err)]
                        Right expr -> pure expr

                    appE [| (: []) |] (pure el)

                _ -> lift [pageCodeBlock "" str]
            _ -> lift [pageCodeBlock "" str]

    (BlockList Ordered inlineOrBlocks) ->
        appE [| \x -> [li_ $ mconcat x] |] $ case inlineOrBlocks of
            Left is -> lift $ map renderInline is
            Right bs -> ListE <$> mapM (renderBlock False) bs

    (BlockList Unordered inlineOrBlocks) ->
        appE [| \x -> [li_ $ mconcat x] |] $ case inlineOrBlocks of
            Left is -> lift $ map renderInline is
            Right bs -> ListE <$> mapM (renderBlock False) bs

    (BlockHtml _) ->
        lift [div_ [str_ "TODO: BlockHtml"]]

    BlockRule ->
        lift [hr_ []]

    (BlockReference _ _) ->
        lift [div_ [str_ "TODO: BlockReference"]]

{-}
renderBlock' :: Bool -> Block [Inline] -> [Element]
renderBlock' isTopLevel b = case b of
    (BlockPara is) ->
        (\x -> [pageParagraph isTopLevel $ mconcat x]) (mapM renderInline is)

    (BlockHeading level is) -> case level of
        1 -> (\x -> [pageH2 $ mconcat x]) (mapM renderInline is)
        2 -> (\x -> [pageH3 $ mconcat x]) (mapM renderInline is)
        3 -> (\x -> [pageH4 $ mconcat x]) (mapM renderInline is)
        _ -> (\x -> [pageH4 $ mconcat x]) (mapM renderInline is)

    (BlockPlainText is) ->
        mconcat (mapM renderInline is)

    (BlockQuote is) ->
        (\x -> [pageBlockquote $ mconcat x]) (mapM (renderBlock' False) is)

    (BlockCode mbType str) -> case mbType of
        Nothing -> [pageCodeBlock "" str]
        Just tag -> case T.splitOn "|" tag of
            [tag'] -> case tag' of
                "code" -> [pageCodeBlock "" str]
                "nauva" ->
                    let pepDef = PageElementProps {pepTitle = Nothing, pepSpan = 6}
                        cspDef = CodeSpecimenProps {cspPEP = pepDef, cspNoSource = False }
                        (csp, expr, str2) = case T.splitOn "---\n" str of
                            [str1] -> case parseExp (T.unpack str1) of
                                Left err ->
                                    let err1 = div_ [str_ (T.pack err)]
                                    in (cspDef, err1, str)
                                Right expr -> (cspDef, expr, str1)

                            [rawYAML, str1] -> case decodeEither (T.encodeUtf8 rawYAML) of
                                    Left yamlErr -> do
                                        let err1 = div_ [str_ $ T.pack $ show yamlErr]
                                        in (cspDef, err1, str1)
                                    Right csp -> case parseExp (T.unpack str1) of
                                        Left err ->
                                            let err1 = div_ [str_ (T.pack err)]
                                            in (csp, err1, str)
                                        Right expr -> (csp, expr, str1)
                            _ ->
                                let expr = div_ [str_ "Unrecognized expression"]
                                in (cspDef, expr, str)

                    in (\c -> [pageElement (cspPEP csp) [nauvaSpecimen $ NauvaSpecimen csp c "Haskell" str2]]) expr

                "hint" ->
                    let blocks = markdownBlocksT str
                        children = mapM (renderBlock' False) blocks
                    in (\x -> [pageHint $ mconcat x] |] (pure children)

                "element" -> do
                    (: []) $ case parseExp (T.unpack str) of
                        Left err -> div_ [str_ (T.pack err)]
                        Right expr -> expr

                _ -> [pageCodeBlock str]
            _ -> [pageCodeBlock str]

    (BlockList Ordered inlineOrBlocks) ->
        (\x -> [li_ $ mconcat x]) $ case inlineOrBlocks of
            Left is -> mapM renderInline is
            Right bs -> mapM (renderBlock' False) bs

    (BlockList Unordered inlineOrBlocks) ->
        (\x -> [li_ $ mconcat x]) $ case inlineOrBlocks of
            Left is -> mapM renderInline is
            Right bs -> mapM (renderBlock' False) bs

    (BlockHtml _) ->
        [div_ [str_ "TODO: BlockHtml"]]

    BlockRule ->
        [hr_ []]

    (BlockReference _ _) ->
        [div_ [str_ "TODO: BlockReference"]]
-}

renderInline :: Inline -> [Element]
renderInline i = case i of
    (InlineText str) ->
        [str_ str]

    (InlineItalic is) ->
        [i_ $ concatMap renderInline is]

    (InlineBold is) ->
        [strong_ $ concatMap renderInline is]

    (InlineLink url _title is) ->
        [a_ [href_ url] (concatMap renderInline is)]

    (InlineCode str) ->
        [pageCode [str_ str]]

    (InlineHtml _) ->
        [str_ "TODO: InlineHtml"]

    (InlineImage url _ _) ->
        [img_ [src_ url]]

    (InlineFootnoteRef _) ->
        [str_ "TODO: InlineFootnoteRef"]

    (InlineFootnote _) ->
        [str_ "TODO: InlineFootnote"]



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


catalogPageFromFile :: String -> Q Exp -- Element
catalogPageFromFile relativePath = do
    loc <- location
    let path = takeDirectory (loc_filename loc) </> relativePath
    contents <- runIO $ BS.readFile path
    catalogPage contents
