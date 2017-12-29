#!/usr/bin/env stack
{-  stack   --resolver=lts-10.0
    script  --package=directory
            --package=filepath
            --package=interpolate
            --package=process
            --package=protolude
            --package=temporary
            --package=text
            --package=yaml
-}
{-# OPTIONS -Werror #-}
{-# OPTIONS -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import           Prelude (error, fail)
import           Protolude

import           Data.Char (isSpace)
import           Data.String.Interpolate.IsString (i)
import           Data.Text (lines, stripPrefix, unlines)
import qualified Data.Text as Text
import           Data.Yaml as Yaml
import           System.Directory (getCurrentDirectory, listDirectory)
import           System.FilePath (takeExtension, (-<.>), (</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (callProcess)

data Language = NoLanguage | C
    deriving Show

data SnippetConfig = SnippetConfig
    { after   :: Text
    , include :: [FilePath]
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
    , filepath  :: FilePath
    , language  :: Language
    , startLine :: Int
    }
    deriving Show

main :: IO ()
main = do
    files <- listDirectory "."
    for_ (sort files) $ \file -> case takeExtension file of
        ".md" -> do
            putStr $ file <> " ...\t"
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
    fileContents <- readFile file
    pure $ toList $ map parseSnippet $ splitRawSnippets $ zip [1 ..] $ lines
        fileContents
  where

    splitRawSnippets []    = []
    splitRawSnippets input = fromMaybe [] $ do
        (_text, (lineNo, header):afterHeader) <- pure $ breakSnippet input
        languageSpec                          <- stripPrefix "```" header
        let (snippet, rest) = breakSnippet afterHeader
        Just $ (lineNo, languageSpec, map snd snippet) : splitRawSnippets
            (take 1 rest)
      where
        breakSnippet = break $ \(_lineNo, line) -> "```" `Text.isPrefixOf` line

    parseSnippet (lineNo, header, content) = Snippet
        { config    = decodeConfig configText
        , content   = unlines content
        , filepath  = file
        , language
        , startLine = lineNo
        }
      where
        (lang, configText) = Text.break isSpace header
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
checkSnippet snippet@Snippet { language = C } =
    withSystemTempDirectory "ComputerScience.test" $ \tmp -> do
        let src = tmp </> "source.c"
        writeFile src
            $  unlines
            $  [ [i|#include "#{inc}"|] | inc <- include ]
            <> [[i|#line #{startLine} "#{filepath}"|], content, after]
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
    Snippet { config, content, filepath, startLine } = snippet
    SnippetConfig { after, include }                 = config
