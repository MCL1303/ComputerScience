#!/usr/bin/env stack
-- stack --resolver=lts-10.0 script
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Prelude                   hiding (FilePath)

import           Control.Lens
import           Control.Monad.RWS
import qualified Data.DList                as DList
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS (encodeString)
import           Numeric.Natural
import           System.Directory
import           Turtle

main :: IO ()
main = sh $ do
    file <- ls "."
    case extension file of
        Nothing          -> pure () -- ignore
        Just "hs"        -> pure () -- ignore
        Just "md"        -> checkExamples file
        Just "mediawiki" -> pure () -- ignore
        Just "sh"        -> pure () -- ignore
        ext              -> error $ "I don't know what to do with " ++ repr ext

data Language = NoLanguage | C
    deriving Show

data Example = Example
    { file      :: FilePath
    , startLine :: Natural
    , language  :: Language
    , content   :: Text
    }
    deriving Show

checkExamples :: FilePath -> Shell ()
checkExamples = extract >=> check

extract :: FilePath -> Shell Example
extract file = do
    fileContents                <- liftIO $ readTextFile file
    ((Nothing, _, _), examples) <-
        exec $ for_ (zip [1 ..] $ Text.lines fileContents) $ \(lineNo, line) ->
            do
                currentLanguage <- use _1
                case Text.stripPrefix "```" line of
                    Nothing ->
                        when (isJust currentLanguage)
                            $   _2
                            <>= DList.singleton line
                    Just "" -> case currentLanguage of
                        Just language -> do
                            content   <- Text.unlines . DList.toList <$> use _2
                            startLine <- use _3
                            tell $ DList.singleton Example
                                { file
                                , language
                                , content
                                , startLine
                                }
                            put (Nothing, DList.empty, 0)
                        Nothing -> put (Just NoLanguage, DList.empty, lineNo)
                    Just "c"
                        | isJust currentLanguage -> error
                            "cannot start snippet inside snippet"
                        | otherwise -> put (Just C, DList.empty, lineNo)
                    Just language ->
                        error $ "unknown language: " ++ repr language
    select examples
    where exec action = execRWST action () (Nothing, DList.empty, 0)

check :: Example -> Shell ()
check Example { file, startLine, language, content } = case language of
    NoLanguage -> pure () -- ok
    C          -> do
        systemTempDir <- fromString <$> liftIO getTemporaryDirectory
        tmp           <- mktempdir systemTempDir "ComputerScience.test."
        let src = tmp </> "source.c"
        liftIO $ writeTextFile src $ format
            ("#line " % d % " \"" % fp % "\" \n" % s)
            (startLine + 1)
            file
            content
        procs "gcc"
              ["-Wall", "-Werror", "-Wextra", "-pedantic", encodeText src]
              empty

encodeText :: FilePath -> Text
encodeText = fromString . encodeString
