#!/usr/bin/env stack
-- stack --resolver=lts-10.0 script
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Prelude                   hiding (FilePath)

import           Control.Lens
import           Control.Monad.RWS
import           Data.DList                as DList
import           Data.Foldable
import           Data.Maybe
import           Data.Text                 as Text
import           Filesystem.Path.CurrentOS
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
    { file     :: FilePath
    -- , lineStart
    , language :: Language
    , content  :: Text
    }
    deriving Show

checkExamples :: FilePath -> Shell ()
checkExamples = extract >=> check

extract :: FilePath -> Shell Example
extract file = do
    fileContents             <- liftIO $ readTextFile file
    ((Nothing, _), examples) <-
        exec $ for_ (Text.lines fileContents) $ \line -> do
            currentLanguage <- use _1
            case Text.stripPrefix "```" line of
                Nothing ->
                    when (isJust currentLanguage) $ _2 <>= DList.singleton line
                Just "" -> case currentLanguage of
                    Just language -> do
                        content <- Text.unlines . DList.toList <$> use _2
                        tell $ DList.singleton Example
                            { file
                            , language
                            , content
                            }
                        put (Nothing, DList.empty)
                    Nothing -> _1 .= Just NoLanguage
                Just "c"
                    | isJust currentLanguage -> error
                        "cannot start snippet inside snippet"
                    | otherwise -> _1 .= Just C
                Just language -> error $ "unknown language: " ++ repr language
    select examples
    where exec action = execRWST action () (Nothing, DList.empty)

check :: Example -> Shell ()
check = undefined
