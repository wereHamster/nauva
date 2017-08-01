{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nauva.NJS.TH
    ( njs
    ) where


import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import           Data.Text            (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Monoid
import qualified Data.Attoparsec.Text as AP
import           Data.Char

import           Control.Applicative
import           Control.Monad.Writer.Lazy

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Nauva.Internal.Types
import           Nauva.NJS



njs :: QuasiQuoter
njs = QuasiQuoter
    { quoteExp  = njsQ . T.pack
    , quotePat  = error "njs: quotePat"
    , quoteType = error "njs: quoteType"
    , quoteDec  = error "njs: quoteDec"
    }


njsQ :: Text -> Q Exp
njsQ t = do
    let templ = template t
    (body, w) <- runWriterT $ render templ

    pure $ foldl1 AppE
        [ VarE 'createF
        , ListE $ map (LitE . StringL . T.unpack) w
        , ListE $ map (\(x, _) -> LitE $ StringL $ T.unpack x) $ arguments templ
        , body
        ]



data Fragment
    = StringF !Text
    | RefF !Text
    | ConsF !Text
    deriving (Show, Eq)

data Template = Template
    { arguments :: [(Text, Maybe Text)]
    , body :: [Fragment]
    }


template :: Text -> Template
template input = case AP.parseOnly expParser input of
    Left  x -> error $ x ++ " on input '" ++ T.unpack input ++ "'"
    Right x -> x

mergeStrings :: [Fragment] -> [Fragment]
mergeStrings = reverse . foldl f []
  where
    f acc              (StringF "") = acc
    f ((StringF a):xs) (StringF b)  = (StringF $ a <> b) : xs
    f acc              frag         = frag:acc

expParser :: AP.Parser Template
expParser = do
    AP.skipSpace
    arguments <- argumentsParser
    AP.skipSpace
    fragments <- fragmentParser

    pure $ Template
        { arguments = arguments
        , body = fragments
        }

argumentsParser :: AP.Parser [(Text, Maybe Text)]
argumentsParser = do
    args <- multipleArguments <|> singleArgument
    AP.skipSpace
    AP.string "=>"
    AP.skipSpace
    pure args

  where
    arg = do
        AP.skipSpace
        n <- AP.takeWhile1 isAlpha
        AP.skipSpace
        ch <- AP.peekChar'
        case ch of
            ':' -> do
                AP.char ':'
                AP.skipSpace
                t <- AP.takeWhile1 isAlpha
                AP.skipSpace
                pure (n, Just t)

            _ -> do
                pure (n, Nothing)

    multipleArguments = do
        AP.string "("
        args <- arg `AP.sepBy` (AP.char ',')
        AP.string ")"
        pure args

    singleArgument = do
        text <- AP.takeWhile1 isAlpha
        pure [(text, Nothing)]

fragmentParser :: AP.Parser [Fragment]
fragmentParser = normalBody <|> arrowBody
  where
    normalBody = do
        AP.string "{"
        fragments <- mergeStrings <$> go

        -- Delete the closing '}' from the last fragment (if it's a string)
        let len = length fragments
        if len == 0
            then pure fragments
            else do
                let (init, last) = (take (len - 1) fragments, drop (len - 1) fragments)
                case last of
                    [StringF x] -> pure $ init <> [StringF (T.dropEnd 1 $ T.strip x)]
                    _ -> pure fragments


    -- Prepend "return " so that we get a valid JS function body.
    arrowBody = do
        fragments <- go
        pure $ mergeStrings $ [StringF "return "] <> fragments

    go = ref <|> cons <|> string <|> (pure [] <* AP.endOfInput)

    ref = do
        AP.string "@"
        text <- AP.takeWhile1 isAlpha
        (RefF text :) <$> go

    cons = do
        AP.string "$"
        text <- AP.takeWhile1 isAlpha
        (ConsF text :) <$> go

    string = do
        text <- T.singleton <$> AP.anyChar
        (StringF text :) <$> go

render :: Template -> WriterT [Text] Q Exp
render (Template _ frags) = do
    exprs <- traverse renderFragment frags
    pure $ AppE (VarE 'T.pack) $ foldl1 (\a b -> UInfixE a (VarE '(<>)) b) exprs
  where
    renderFragment :: Fragment -> WriterT [Text] Q Exp
    renderFragment (StringF s) =
        pure $ LitE $ StringL $ T.unpack s

    renderFragment (RefF s) = do
        name <- lift $ do
            n <-lookupValueName (T.unpack s)
            case n of
                Nothing -> fail $ "renderFragment: ref not found: " <> T.unpack s
                Just x -> pure x

        pure $ foldl1 (\a b -> UInfixE a (VarE '(<>)) b)
            [ LitE $ StringL "nv$ref("
            , AppE (VarE 'show) $ AppE (VarE 'unRefKey) (VarE name)
            , LitE $ StringL ")"
            ]

    renderFragment (ConsF s) = do
        name <- lift $ do
            n <-lookupValueName (T.unpack s)
            case n of
                Nothing -> fail $ "renderFragment: constructor not found: " <> T.unpack s
                Just x -> pure x

        info <- lift $ reify name
        case info of
            DataConI _ _ _ -> pure ()
            _ -> fail "renderFragment: not a data constructor"

        tell [s]

        pure $ LitE $ StringL $ T.unpack $ "nv$" <> s
