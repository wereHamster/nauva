{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Data.Text (Text, replace, isSuffixOf, toTitle, pack, strip)
import qualified Data.Text as T

import           Data.String (fromString)
import           Data.Monoid ((<>), mconcat)

import           Control.Monad (join, forM_)

import           Filesystem.Path.CurrentOS as FS hiding (hasExtension)

import           Options.Applicative (Parser, execParser, helper)
import           Options.Applicative.Builder (auto, progDesc, argument, command, subparser, info, str, metavar, command)

import           Shelly hiding (command)



main :: IO ()
main = do
    nvc <- execParser (info (helper <*> nvCommand) mempty)
    case nvc of
        (NCCreate n)  -> createNewProject n
        (NCStart n)   -> start n
        (NCHaddock n) -> haddock n
        (NCNative n)  -> native n



data NvCommand
    = NCCreate Text
    | NCStart Text
    | NCHaddock Text
    | NCNative Text

nvCommand :: Parser NvCommand
nvCommand = subparser
    ( command "create" (info (
        NCCreate <$> fmap fromString (argument str (metavar "PROJECT NAME"))) (progDesc "Creates new project"))
   <> command "start" (info (
        NCStart <$> fmap fromString (argument str (metavar "PROJECT NAME"))) (progDesc "Starts the project in development mode"))
   <> command "haddock" (info (
        NCHaddock <$> fmap fromString (argument str (metavar "PROJECT NAME"))) (progDesc "Build the haddock documentation for the project"))
   <> command "native" (info (
        NCNative <$> fmap fromString (argument str (metavar "PROJECT NAME"))) (progDesc "Compile the project to native JavaScript code"))
    )


-- Copies the template projects and renames files
createNewProject :: Text -> IO ()
createNewProject projectName = shelly $ do
    cd "product"
    let projectNameFP = fromText projectName

    projectExists <- test_e projectNameFP
    when projectExists $ errorExit $ mconcat ["Project ", projectName, " already exists"]

    cp_r "template" projectNameFP

    -- needs to be done this way, because otherwise we collide with template-haskell
    let projectPackageName = mconcat ["nauva-product-", projectName]
    let projectModuleName = mconcat ["Nauva.Product.", toTitle projectName]
    let replace_template = replace "nauva-product-template" projectName
    let replace_Template = replace "Nauva.Product.Template" projectModuleName

    let changeCabalFile file = do
            modifyTextFile replace_template file
            modifyTextFile replace_Template file
            modifyFileName replace_template file
    mapM_ changeCabalFile =<< findWhen (hasExtension ".cabal") projectNameFP

    let changeSourceFile file = do
            modifyTextFile replace_Template file
            modifyFileName replace_template file
    mapM_ changeSourceFile =<< findWhen (hasExtension ".hs") projectNameFP

    dirs <- findWhen (\f -> (&&) <$>  isFileName "Template" f <*> test_d f) projectNameFP
    mapM_ (\f -> mv f $ append (parent f) (fromText (toTitle projectName)))   dirs

    let echoA = echo . mconcat
    echoA ["Project ", projectName, " created"]
    echo "To run use command:"
    echoA [" ./bin/nauva start ", projectName, "/app"]

  where
    modifyTextFile :: (Text -> Text) -> FS.FilePath -> Sh ()
    modifyTextFile u f = readfile f >>= pure . u >>= writefile f

    modifyFileName :: (Text -> Text) -> FS.FilePath -> Sh ()
    modifyFileName u f = mv f $ fromText (u $ toTextIgnore f)

    hasExtension :: Monad m => Text -> FS.FilePath -> m Bool
    hasExtension e = pure . isSuffixOf e . toTextIgnore

    isFileName :: Monad m => Text -> FS.FilePath -> m Bool
    isFileName n f = pure $ n == (toTextIgnore . filename) f



-------------------------------------------------------------------------------
-- start – start an application in dev mode

start :: Text -> IO ()
start projectName = shelly $ do
    currentWorkingDirectory <- pwd
    let projectDevNameFP = fromText ("product/" <> projectName <> "/dev")

    projectExists <- test_e projectDevNameFP
    unless projectExists $
        errorExit $ mconcat ["Project ", projectName, " does not exist"]

    let nauvadBase = pack (encodeString currentWorkingDirectory <> "/pkg/hs/nauvad")
        nauvadStackYaml = nauvadBase <> "/stack.yaml"

    echo "Building nauvad… this may take a while"
    run_ "stack"
        [ "--stack-yaml", nauvadStackYaml
        , "--install-ghc"
        , "build"
        ]

    setenv "NAUVAD_PUBLIC_PATH" (nauvadBase <> "/public")
    cd projectDevNameFP

    run_ "stack"
        [ "exec"
        , "--stack-yaml", nauvadStackYaml
        , "nauvad"
        , "--"
        , "--command=stack ghci"
        , "--test=:main"
        , "--warnings"
        ]



-------------------------------------------------------------------------------
-- haddock – build haddock for the 'shared' part of a product

haddock :: Text -> IO ()
haddock projectName = shelly $ do
    currentWorkingDirectory <- pwd
    let projectSharedNameFP = fromText ("product/" <> projectName <> "/shared")

    projectExists <- test_e projectSharedNameFP
    unless projectExists $
        errorExit $ mconcat ["Project ", projectName, " does not exist"]

    run_ "stack"
        [ "--install-ghc"
        , "--stack-yaml", "product/" <> projectName <> "/shared/stack.yaml"
        , "haddock"
        ]

    localDocRoot <- strip <$> run "stack"
        [ "--stack-yaml", "product/" <> projectName <> "/shared/stack.yaml"
        , "path"
        , "--local-doc-root"
        ]

    run_ "open" [localDocRoot <> "/index.html"]



-------------------------------------------------------------------------------
-- native – build the project into native JavaScript code

native :: Text -> IO ()
native projectName = shelly $ do
    currentWorkingDirectory <- pwd
    let projectNativeNameFP = fromText ("product/" <> projectName <> "/native")

    projectExists <- test_e projectNativeNameFP
    unless projectExists $
        errorExit $ mconcat ["Project ", projectName, " does not exist"]

    run_ "stack"
        [ "--install-ghc"
        , "--stack-yaml", "product/" <> projectName <> "/native/stack.yaml"
        , "build"
        ]

    localInstallRoot <- strip <$> run "stack"
        [ "--stack-yaml", "product/" <> projectName <> "/native/stack.yaml"
        , "path"
        , "--local-install-root"
        ]

    rm_rf "build"
    mkdir_p "build"

    let binDir = localInstallRoot <> "/bin/nauva-product-" <> replace "/" "-" projectName <> "-native.jsexe/"
    filesToCopy <- ls (fromText binDir)
    forM_ filesToCopy $ \fp -> cp fp "build"

    echo "Files copied to 'build/' folder"
