#!/usr/bin/env stack
{-  stack --resolver=lts-10.0
    script
        --package=directory
        --package=filepath
        --package=interpolate
        --package=process
        --package=temporary
        --package=text
        --package=yaml
-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.String.Interpolate.IsString
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding
import qualified Data.Text.IO as Text
import           Data.Yaml as Yaml
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process

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
            putStrLn "OK"
        _ -> pure () -- ignore
    putStrLn "OK"

checkSnippets :: FilePath -> IO ()
checkSnippets file = do
    snippets <- extract file
    for_ snippets check

extract :: FilePath -> IO [Snippet]
extract file = do
    fileContents <- Text.readFile file
    pure
        $ toList
        $ map parseSnippet
        $ splitRawSnippets
        $ zip [1 ..]
        $ Text.lines fileContents
  where

    splitRawSnippets []    = []
    splitRawSnippets input = fromMaybe [] $ do
        (_text, (lineNo, header):afterHeader) <- pure $ breakSnippet input
        languageSpec                          <- Text.stripPrefix "```" header
        let (snippet, rest) = breakSnippet afterHeader
        Just $ (lineNo, languageSpec, map snd snippet) : splitRawSnippets
            (take 1 rest)
      where
        breakSnippet = break $ \(_lineNo, line) -> "```" `Text.isPrefixOf` line

    parseSnippet (lineNo, header, content) = Snippet
        { config    = decodeConfig configText
        , content   = Text.unlines content
        , filepath  = file
        , language
        , startLine = lineNo
        }
      where
        (languageText, configText) = Text.break isSpace header
        language                   = case languageText of
            ""  -> NoLanguage
            "c" -> C
            _   -> error $ concat
                [ file
                , ", line "
                , show lineNo
                , ": unknown language "
                , Text.unpack languageText
                ]
        decodeConfig =
            fromMaybe defaultSnippetConfig
                . fromRight (error . ([i|#{file}, line #{lineNo}: |] <>))
                . Yaml.decodeEither
                . encodeUtf8
        fromRight a = either a id

check :: Snippet -> IO ()
check Snippet { language = NoLanguage } = pure ()
check snippet@Snippet { language = C } =
    withSystemTempDirectory "ComputerScience.test" $ \tmp -> do
        let src = tmp </> "source.c"
        Text.writeFile src
            $  Text.unlines
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
