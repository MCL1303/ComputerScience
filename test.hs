#!/usr/bin/env stack
-- stack --resolver=lts-10.0 script
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Prelude                   hiding (FilePath)

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson.TH
import           Data.Char
import qualified Data.DList                as DList
import           Data.Maybe
import qualified Data.Text                 as Text
import           Data.Text.Encoding
import           Data.Yaml                 as Yaml
import           Filesystem.Path.CurrentOS (encodeString, replaceExtension)
import           Numeric.Natural
import           System.Directory
import           Turtle

data Language = NoLanguage | C
    deriving Show

data ExampleConfig = ExampleConfig
    { _after   :: Maybe Text
    , _include :: Maybe [Text]
    }
    deriving Show

deriveFromJSON defaultOptions{fieldLabelModifier = drop 1} ''ExampleConfig
makeLenses ''ExampleConfig

defaultExampleConfig :: ExampleConfig
defaultExampleConfig = ExampleConfig {_after = Nothing, _include = Nothing}

data Example = Example
    { _config    :: ExampleConfig
    , _content   :: Text
    , _filepath  :: FilePath
    , _language  :: Language
    , _startLine :: Natural
    }
    deriving Show

makeLenses ''Example

main :: IO ()
main = sh $ do
    file <- ls "."
    case extension file of
        Nothing          -> pure () -- ignore
        Just "h"         -> pure () -- ignore
        Just "hs"        -> pure () -- ignore
        Just "md"        -> checkExamples file
        Just "mediawiki" -> pure () -- ignore
        Just "sh"        -> pure () -- ignore
        ext              -> error $ "I don't know what to do with " ++ show ext

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

    appendCurrent line = _Just . content <>= Text.cons '\n' line

    flush example = do
        tell $ DList.singleton example
        put Nothing

    start lineNo = put $ Just Example
        { _config    = defaultExampleConfig
        , _content   = Text.empty
        , _filepath  = file
        , _language  = NoLanguage
        , _startLine = lineNo
        }

    startWith configText lineNo = put $ Just Example
        { _config    = decodeConfig configText
        , _content   = Text.empty
        , _filepath  = file
        , _language  = C
        , _startLine = lineNo
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
check ex = case ex ^. language of
    NoLanguage -> pure () -- ok
    C          -> do
        systemTempDir <- fromString <$> liftIO getTemporaryDirectory
        tmp           <- mktempdir systemTempDir "ComputerScience.test."
        let src = tmp </> "source.c"
        liftIO
            $  writeTextFile src
            $  Text.unlines
            $  [ format ("#include \"" % s % "\"") inc
               | inc <- ex ^. config . include . _Just
               ]
            ++ [ format ("#line " % d % " \"" % fp % "\"")
                        (ex ^. startLine)
                        (ex ^. filepath)
               , ex ^. content
               , ex ^. config . after . _Just
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

encodeText :: FilePath -> Text
encodeText = fromString . encodeString
