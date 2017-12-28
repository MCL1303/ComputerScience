#!/usr/bin/env stack
-- stack --resolver=lts-10.0 script
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           Prelude hiding (FilePath)

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson.TH
import           Data.Char
import qualified Data.DList as DList
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Data.Yaml as Yaml
import           Filesystem.Path.CurrentOS (encodeString, replaceExtension)
import           Numeric.Natural
import           System.Directory
import           Turtle

data Language = NoLanguage | C
    deriving Show

data ExampleConfig = ExampleConfig
    { after   :: Maybe Text
    , include :: Maybe [Text]
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
main = sh $ do
    file      <- ls "."
    Just "md" <- pure $ extension file
    checkExamples file

checkExamples :: FilePath -> Shell ()
checkExamples = extract >=> check

extract :: FilePath -> Shell Example
extract file = do
    fileContents <- liftIO $ readTextFile file
    let (lastExample, examples) =
            runWriter
                $ (`execStateT` Nothing)
                $ mapM_ parseExampleLine
                $ zip [1 ..]
                $ Text.lines fileContents
    when (isJust lastExample) $ error $ show lastExample
    select examples
  where
    parseExampleLine (lineNo, line) = do
        currentExample <- get
        case Text.stripPrefix "```" line of
            Nothing -> when (isJust currentExample) $ appendCurrent line
            Just "" -> case currentExample of
                Just example -> flush example
                Nothing      -> start lineNo
            Just languageSpec -> case Text.break isSpace languageSpec of
                ("c", configText)
                    | isJust currentExample -> error
                        "cannot start snippet inside snippet"
                    | otherwise -> startWith configText lineNo
                (exLanguage, _) ->
                    error $ "unknown language: " ++ show exLanguage

    appendCurrent line = modify $ fmap $ \case
        example@Example { content } ->
            example { content = content <> "\n" <> line }

    flush example = do
        tell $ DList.singleton example
        put Nothing

    start lineNo = put $ Just Example
        { config    = defaultExampleConfig
        , content   = Text.empty
        , filepath  = file
        , language  = NoLanguage
        , startLine = lineNo
        }

    startWith configText lineNo = put $ Just Example
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
        msg = Text.unpack $ format (fp % ", line " % d % ": ") file lineNo
        fromRight a = either a id

check :: Example -> Shell ()
check example = case language of
    NoLanguage -> pure () -- ok
    C          -> do
        systemTempDir <- fromString <$> liftIO getTemporaryDirectory
        tmp           <- mktempdir systemTempDir "ComputerScience.test."
        let src = tmp </> "source.c"
        liftIO
            $  writeTextFile src
            $  Text.unlines
            $  [ format ("#include \"" % s % "\"") inc
               | inc <- fromMaybe [] include
               ]
            ++ [ format ("#line " % d % " \"" % fp % "\"") startLine filepath
               , content
               , fromMaybe "" after
               ]
        wd <- pwd
        procs
            "gcc"
            [ "-c"
            , "-I" <> encodeText wd
            , "-Wall"
            , "-Werror"
            , "-Wextra"
            , "-pedantic"
            , encodeText src
            , "-o" <> encodeText (src `replaceExtension` "o")
            ]
            empty
  where
    Example { config, content, filepath, language, startLine } = example
    ExampleConfig { after, include }                           = config

encodeText :: FilePath -> Text
encodeText = fromString . encodeString
