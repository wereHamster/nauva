{-# LANGUAGE OverloadedStrings #-}

module Main where

import Shelly hiding (command)
import Data.Text (Text, replace, isSuffixOf, toTitle)
import Data.String (IsString, fromString)
import Filesystem.Path.CurrentOS as FS hiding (hasExtension)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Builder (auto, progDesc, argument, command, subparser, info, str, metavar, command)
import Control.Monad (join)
import Data.Monoid ((<>), mconcat)


opts = subparser
  (
    command "create" (info (
      createNewProject <$> fmap fromString (argument str (metavar "PROJECT NAME"))) (progDesc "Creates new project"))
  )

main :: IO ()
main = join $ execParser (info (helper <*> opts) mempty)

hasExtension :: Monad m => Text -> FS.FilePath -> m Bool
hasExtension e = pure . isSuffixOf e . toTextIgnore

isFileName :: Monad m => Text -> FS.FilePath -> m Bool
isFileName n f = pure $ n == (toTextIgnore . filename) f

-- Copies the template projects and renames files
createNewProject :: Text -> IO ()
createNewProject projectName = shelly $ do
  cd "../../../product"
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
  echoA [" ./bin/dev ", projectName, "/app"]

modifyTextFile :: (Text -> Text) -> FS.FilePath -> Sh ()
modifyTextFile u f = readfile f >>= pure . u >>= writefile f

modifyFileName :: (Text -> Text) -> FS.FilePath -> Sh ()
modifyFileName u f = mv f $ fromText (u $ toTextIgnore f)