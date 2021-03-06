#!/usr/bin/env stack
-- stack --resolver=lts-10.0 script
{-# OPTIONS -Werror #-}
{-# OPTIONS -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import           Prelude (error, fail)
import           Protolude hiding (FilePath)
import           Turtle

import           Data.Char (isSpace)
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import           Data.Yaml as Yaml
import           Filesystem.Path.CurrentOS (encodeString)

data Language = NoLanguage | C
    deriving Show

data SnippetConfig = SnippetConfig
    { after   :: Text
    , include :: [Text]
    }
    deriving Show

instance FromJSON SnippetConfig where
    parseJSON (Object v) = do
        after   <- v .:? "after"   .!= ""
        include <- v .:? "include" .!= []
        pure SnippetConfig{after, include}
    parseJSON Null = pure defaultSnippetConfig
    parseJSON _    = fail "Expected object for SnippetConfig"

defaultSnippetConfig :: SnippetConfig
defaultSnippetConfig = SnippetConfig{after = "", include = []}

data Snippet = Snippet
    { config    :: SnippetConfig
    , content   :: Text
    , filepath  :: Text
    , language  :: Language
    , startLine :: Int
    }
    deriving Show

main :: IO ()
main = do
    files <- Turtle.sort $ ls "."
    for_ files $ \file ->
        case extension file of
            Just "md" -> do
                putText $ pathToS file <> " ...\t"
                checkSnippets =<< realpath file
            _ -> pure () -- ignore
    putText "OK"

checkSnippets :: FilePath -> IO ()
checkSnippets = extract >=> traverse_ checkSnippet

extract :: FilePath -> IO [Snippet]
extract file =
    map parseSnippet . splitSnippets . zip [1 ..] . Text.lines
    <$> readTextFile file
  where

    splitSnippets [] = []
    splitSnippets fileLines = fromMaybe [] $ do
        (lineNo, specLine) : afterSpec <- pure $ skipToSnippet fileLines
        spec <- Text.stripPrefix "```" specLine
        let (snippet, rest) = break (isCodeMarker . snd) afterSpec
        pure $ (lineNo, spec, map snd snippet) : splitSnippets (take 1 rest)
      where
        skipToSnippet = dropWhile $ not . isCodeMarker . snd
        isCodeMarker line = "```" `Text.isPrefixOf` line

    parseSnippet (lineNo, spec, content) = Snippet
        { config    =
            either (error . ([i|#{file}, line #{lineNo}: |] <>)) identity
            $ Yaml.decodeEither
            $ encodeUtf8 configText
        , content   = Text.unlines content
        , filepath  = toS $ encodeString file
        , language  = case lang of
            ""  -> NoLanguage
            "c" -> C
            _   -> error [i|#{file}, line #{lineNo}: unknown language #{lang}|]
        , startLine = lineNo
        }
      where
        (lang, configText) = Text.break isSpace spec

checkSnippet :: Snippet -> IO ()
checkSnippet Snippet{language = NoLanguage} = pure ()
checkSnippet snippet@Snippet{language = C} = sh $ do
    sourceDir <- pwd
    mktempdir "" "ComputerScience.test" >>= pushd
    liftIO . writeTextFile snippetFile . Text.unlines $
        [ [i|#include "#{inc}"|] | inc <- include ]
        <> [[i|#line #{startLine} "#{filepath}"|], content, after]
    compile sourceDir
  where
    snippetFile = "snippet.c"
    Snippet{config, content, filepath, startLine} = snippet
    SnippetConfig{after, include} = config
    compile includeDir = procs
        "gcc"
        (gccOptions <> ["-c", "-I" <> pathToS includeDir, pathToS snippetFile])
        empty
    gccOptions = ["-Wall", "-Werror", "-Wextra", "-pedantic"]

pathToS :: FilePath -> Text
pathToS = toS . encodeString
