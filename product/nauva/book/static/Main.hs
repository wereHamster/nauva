{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main (main) where


import qualified Data.Text as T
import           Data.List
import           Data.Monoid
import           Data.Foldable

import           Control.Monad
import           Control.Concurrent.STM

import           System.Directory
import           System.FilePath

import           Nauva.App
import           Nauva.Catalog
import           Nauva.Static (elementToMarkup)

import           Nauva.Product.Nauva.Book.App (bookApp, catalogPages)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String



renderCSSDeclarations :: CSSStyleDeclaration -> T.Text
renderCSSDeclarations = mconcat . intersperse ";" . map renderDeclaration
  where
    renderDeclaration (k, CSSValue v) = k <> ":" <> v

cssRuleSelector :: Hash -> [Suffix] -> T.Text
cssRuleSelector hash suffixes = ".s" <> unHash hash <> mconcat (map unSuffix suffixes)

wrapInConditions [] t = t
wrapInConditions (CMedia x:xs) t = "@media " <> x <> " {" <> wrapInConditions xs t <> "}"

renderCSSRule :: CSSRule -> T.Text
renderCSSRule (CSSStyleRule hash conditions suffixes styleDeclaration) = wrapInConditions conditions $ mconcat
    [ cssRuleSelector hash suffixes <> " {"
    , renderCSSDeclarations styleDeclaration
    , "}"
    ]
renderCSSRule (CSSFontFaceRule _ styleDeclaration) = mconcat
    [ "@font-face {"
    , renderCSSDeclarations styleDeclaration
    , "}"
    ]

main :: IO ()
main = do
    putStrLn "Nauva Book"

    forM_ (onlyLeaves catalogPages) $ \leaf -> do
        putStrLn $ T.unpack $ leafHref leaf

        headH <- do
            var <- newTVarIO []
            pure HeadH
                { hElements = var
                , hReplace = atomically . writeTVar var
                }

        routerH <- do
            locVar <- newTVarIO $ Location (leafHref leaf)
            locChan <- newTChanIO

            pure $ RouterH (locVar, locChan) (\_ -> pure ())

        (bodyHtml, styles, actions) <- atomically $ elementToMarkup $
            rootElement bookApp (AppH headH routerH)

        sequence_ actions

        headElements <- atomically $ readTVar (hElements headH)
        headElementsHtml <- forM headElements $ \el -> do
            (html, _, _) <- atomically $ elementToMarkup el
            pure html

        let html = H.docTypeHtml $ do
                H.head $ do
                    H.meta H.! A.charset "utf-8"
                    mconcat headElementsHtml

                H.style $ H.text $ mconcat $ intersperse "\n" $
                    nub $ map renderCSSRule (mconcat $ map unStyle styles)

                H.body
                    bodyHtml

        createDirectoryIfMissing True ("output" `joinDrive` T.unpack (leafHref leaf))
        writeFile ("output" `joinDrive` T.unpack (leafHref leaf) `joinDrive` "index.html") (renderHtml html)
