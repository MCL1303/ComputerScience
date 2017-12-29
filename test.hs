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
import           Filesystem.Path.CurrentOS (encodeString, replaceExtension)

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
    parseJSON _ = fail "Expected object for SnippetConfig"

defaultSnippetConfig :: SnippetConfig
defaultSnippetConfig = SnippetConfig {after = "", include = []}

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
    for_ files $ \file -> case extension file of
        Just "md" -> do
            putStr $ encodeString file <> " ...\t"
            checkSnippets file
            putText "OK"
        _ -> pure () -- ignore
    putText "OK"

checkSnippets :: FilePath -> IO ()
checkSnippets file = do
    snippets <- extract file
    for_ snippets checkSnippet

extract :: FilePath -> IO [Snippet]
extract file = do
    fileContents <- readTextFile file
    pure $ map parseSnippet $ splitSnippets $ zip [1 ..] $ Text.lines
        fileContents
  where

    splitSnippets []        = []
    splitSnippets fileLines = fromMaybe [] $ do
        (_text, (lineNo, spec):afterHeader) <- pure $ breakSnippet fileLines
        languageSpec                        <- Text.stripPrefix "```" spec
        let (snippet, rest) = breakSnippet afterHeader
        Just $ (lineNo, languageSpec, map snd snippet) : splitSnippets
            (take 1 rest)
      where
        breakSnippet = break $ \(_lineNo, line) -> "```" `Text.isPrefixOf` line

    parseSnippet (lineNo, spec, content) = Snippet
        { config    = decodeConfig configText
        , content   = Text.unlines content
        , filepath  = toS $ encodeString file
        , language
        , startLine = lineNo
        }
      where
        (lang, configText) = Text.break isSpace spec
        language           = case lang of
            ""  -> NoLanguage
            "c" -> C
            _   -> error [i|#{file}, line #{lineNo}: unknown language #{lang}|]
        decodeConfig =
            fromMaybe defaultSnippetConfig
                . fromRight (error . ([i|#{file}, line #{lineNo}: |] <>))
                . Yaml.decodeEither
                . encodeUtf8
        fromRight a = either a identity

checkSnippet :: Snippet -> IO ()
checkSnippet Snippet { language = NoLanguage } = pure ()
checkSnippet snippet@Snippet { language = C }  = sh $ do
    tmp <- mktempdir "" "ComputerScience.test"
    liftIO $ do
        let snippetFile = tmp </> "snippet.c"
        writeTextFile snippetFile
            $  Text.unlines
            $  [ [i|#include "#{inc}"|] | inc <- include ]
            <> [[i|#line #{startLine} "#{filepath}"|], content, after]
        sourceDir <- pwd
        procs
            "gcc"
            [ "-c"
            , "-I" <> toS (encodeString sourceDir)
            , "-Wall"
            , "-Werror"
            , "-Wextra"
            , "-pedantic"
            , toS $ encodeString snippetFile
            , "-o" <> toS (encodeString $ snippetFile `replaceExtension` "o")
            ]
            empty
  where
    Snippet { config, content, filepath, startLine } = snippet
    SnippetConfig { after, include }                 = config
