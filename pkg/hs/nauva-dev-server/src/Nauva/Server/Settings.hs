{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nauva.Server.Settings
    ( mkStaticSettings
    ) where


import           Data.ByteString  (ByteString)
import           Data.Monoid
import           Data.FileEmbed   (embedDir)

import           System.Directory
import           System.Environment
import           System.FilePath

import           Network.Wai.Application.Static

import           Language.Haskell.TH (Q, Loc(loc_filename), location, runIO, reportWarning)

import           Prelude



embeddedPublicDir :: [(FilePath, ByteString)]
embeddedPublicDir = $(do
    loc <- location
    embedDir $ (takeDirectory $ loc_filename loc) <> "/../../../public")


mkStaticSettings :: IO StaticSettings
mkStaticSettings = do
    mbPublicPath <- lookupEnv "NAUVA_PUBLIC_PATH"
    case mbPublicPath of
        Nothing -> do
            putStrLn $ "Nauva.Server: serving embedded files"
            pure $ embeddedSettings embeddedPublicDir
        Just publicPath -> do
            putStrLn $ "Nauva.Server: serving files from " <> publicPath
            pure $ defaultFileServerSettings publicPath
