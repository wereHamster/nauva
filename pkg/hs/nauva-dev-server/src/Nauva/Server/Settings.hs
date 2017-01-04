{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nauva.Server.Settings
    ( mkStaticSettings
    ) where


import           Data.ByteString  (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid
import           Data.FileEmbed   (embedDir)

import           System.Environment
import           System.FilePath

import           Language.Haskell.TH (Loc(loc_filename), location)

import           Prelude

import           Snap.Core (Snap, MonadSnap (..), route, writeBS)
import           Snap.Util.FileServe (serveDirectory)



embeddedPublicDir :: [(FilePath, ByteString)]
embeddedPublicDir = $(do
    loc <- location
    embedDir $ (takeDirectory $ loc_filename loc) <> "/../../../public")


mkStaticSettings :: IO (Snap ())
mkStaticSettings = do
    mbPublicPath <- lookupEnv "NAUVA_PUBLIC_PATH"
    case mbPublicPath of
        Nothing -> do
            putStrLn $ "Nauva.Server: serving embedded files"
            pure $ route $ map toRoute embeddedPublicDir
        Just publicPath -> do
            putStrLn $ "Nauva.Server: serving files from " <> publicPath
            pure $ serveDirectory publicPath

toRoute :: MonadSnap m => (FilePath, ByteString) -> (ByteString, m ())
toRoute (path, content) =
    ( BS8.pack path, do
        -- modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
        writeBS content
    )
