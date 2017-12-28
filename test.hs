#!/usr/bin/env stack
{-  stack --resolver=lts-10.0
    script
        --package=aeson
        --package=directory
        --package=dlist
        --package=filepath
        --package=formatting
        --package=mtl
        --package=process
        --package=temporary
        --package=text
        --package=yaml
-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson.TH
import           Data.Char
import qualified Data.DList as DList
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding
import qualified Data.Text.IO as Text
import           Data.Yaml as Yaml
import           Formatting
import           Numeric.Natural
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process

data Language = NoLanguage | C
    deriving Show

data ExampleConfig = ExampleConfig
    { after   :: Maybe Text
    , include :: Maybe [FilePath]
    }
    deriving Show

deriveFromJSON defaultOptions ''ExampleConfig

defaultExampleConfig :: ExampleConfig
defaultExampleConfig = ExampleConfig {after = Nothing, include = Nothing}

data Example = Example
    { config    :: ExampleConfig
    , content   :: Text
    , filepath  :: FilePath
    , language  :: Language
    , startLine :: Natural
    }
    deriving Show

main :: IO ()
main = do
    files <- listDirectory "."
    for_ (sort files) $ \file -> case takeExtension file of
        ".md" -> do
            putStr $ file ++ " ...\t"
            checkExamples file
            putStrLn "OK"
        _ -> pure () -- ignore
    putStrLn "OK"

checkExamples :: FilePath -> IO ()
checkExamples = extract >=> traverse_ check

extract :: FilePath -> IO [Example]
extract file = do
    fileContents <- Text.readFile file
    let (lastExample, examples) =
            runWriter
                $ (`execStateT` Nothing)
                $ traverse_ parseExampleLine
                $ zip [1 ..]
                $ Text.lines fileContents
    when (isJust lastExample) $ error $ show lastExample
    pure $ toList examples
  where

    parseExampleLine (lineNo, line) = do
        currentExample <- get
        case Text.stripPrefix "```" line of
            Nothing -> when (isJust currentExample) $ appendCurrent line
            Just "" -> case currentExample of
                Just example -> flush example
                Nothing      -> startNoLanguage lineNo
            Just languageSpec -> case Text.break isSpace languageSpec of
                ("c", configText)
                    | isJust currentExample -> error
                        "cannot start snippet inside snippet"
                    | otherwise -> start configText lineNo
                (exLanguage, _) ->
                    error $ "unknown language: " ++ show exLanguage

    appendCurrent line = modify $ fmap $ \case
        example@Example { content } ->
            example { content = content <> "\n" <> line }

    flush example = do
        tell $ DList.singleton example
        put Nothing

    startNoLanguage lineNo = put $ Just Example
        { config    = defaultExampleConfig
        , content   = Text.empty
        , filepath  = file
        , language  = NoLanguage
        , startLine = lineNo
        }

    start configText lineNo = put $ Just Example
        { config    = decodeConfig configText
        , content   = Text.empty
        , filepath  = file
        , language  = C
        , startLine = lineNo
        }
      where
        decodeConfig =
            fromMaybe defaultExampleConfig
                . fromRight (error . (msg ++))
                . Yaml.decodeEither
                . encodeUtf8
        msg = formatToString (string % ", line " % int % ": ") file lineNo
        fromRight a = either a id

check :: Example -> IO ()
check Example { language = NoLanguage } = pure ()
check example@Example { language = C } =
    withSystemTempDirectory "ComputerScience.test" $ \tmp -> do
        let src = tmp </> "source.c"
        Text.writeFile src
            $  Text.unlines
            $  [ sformat ("#include \"" % string % "\"") inc
               | inc <- fromMaybe [] include
               ]
            ++ [ sformat ("#line " % int % " \"" % string % "\"")
                         startLine
                         filepath
               , content
               , fromMaybe "" after
               ]
        srcDir <- getCurrentDirectory
        callProcess
            "gcc"
            [ "-c"
            , "-I" <> srcDir
            , "-Wall"
            , "-Werror"
            , "-Wextra"
            , "-pedantic"
            , src
            , "-o" <> (src -<.> "o")
            ]
  where
    Example { config, content, filepath, startLine } = example
    ExampleConfig { after, include }                 = config
