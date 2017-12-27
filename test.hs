#!/usr/bin/env stack
-- stack --resolver=lts-10.0 script
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns #-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Debug.Trace

import           Prelude                   hiding (FilePath)

import           Control.Lens
import           Control.Monad.RWS
import           Data.Aeson.TH
import           Data.Char
import qualified Data.DList                as DList
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                 as Text
import           Data.Text.Encoding
import           Data.Yaml                 as Yaml
import           Filesystem.Path.CurrentOS (encodeString)
import           Numeric.Natural
import           System.Directory
import           Turtle

data Language = NoLanguage | C
    deriving Show

newtype ExampleConfig = ExampleConfig
    { _include :: Maybe [Text]
    }
    deriving Show

deriveFromJSON defaultOptions{fieldLabelModifier = drop 1} ''ExampleConfig
makeLenses ''ExampleConfig

defaultExampleConfig :: ExampleConfig
defaultExampleConfig = ExampleConfig {_include = Nothing}

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
        Just "hs"        -> pure () -- ignore
        Just "md"        -> checkExamples file
        Just "mediawiki" -> pure () -- ignore
        Just "sh"        -> pure () -- ignore
        ext              -> error $ "I don't know what to do with " ++ repr ext

checkExamples :: FilePath -> Shell ()
checkExamples = extract >=> check

extract :: FilePath -> Shell Example
extract file = do
    fileContents        <- liftIO $ readTextFile file
    (Nothing, examples) <-
        exec $ for_ (zip [1 ..] $ Text.lines fileContents) $ \(lineNo, line) ->
            do
                currentExample <- get
                case Text.stripPrefix "```" line of
                    Nothing ->
                        when (isJust currentExample)
                            $   _Just
                            .   content
                            <>= Text.cons '\n' line
                    Just "" -> case currentExample of
                        Just example -> do
                            tell $ DList.singleton example
                            put Nothing
                        Nothing -> put $ Just Example
                            { _config    = defaultExampleConfig
                            , _content   = Text.empty
                            , _filepath  = file
                            , _language  = NoLanguage
                            , _startLine = lineNo
                            }
                    Just languageSpec ->
                        case Text.break isSpace languageSpec of
                            ("c", conf)
                                | isJust currentExample -> error
                                    "cannot start snippet inside snippet"
                                | otherwise -> put $ Just Example
                                    { _config    = either error id
                                        $ traceShowId
                                        $ Yaml.decodeEither
                                        $ encodeUtf8
                                        $ traceShowId conf
                                    , _content   = Text.empty
                                    , _filepath  = file
                                    , _language  = C
                                    , _startLine = lineNo
                                    }
                            (exLanguage, _) ->
                                error $ "unknown language: " ++ repr exLanguage
    select examples
    where exec action = execRWST action () Nothing

check :: Example -> Shell ()
check ex = case ex ^. language of
    NoLanguage -> pure () -- ok
    C          -> do
        systemTempDir <- fromString <$> liftIO getTemporaryDirectory
        tmp           <- mktempdir systemTempDir "ComputerScience.test."
        let src = tmp </> "source.c"
        liftIO
            $  writeTextFile src
            $  traceShow ex
            $  Text.unlines
            $  [ format ("#include \"" % s % "\"") inc
               | inc <- ex ^. config . include . _Just
               ]
            ++ [ format ("#line " % d % " \"" % fp % "\"")
                        (ex ^. startLine)
                        (ex ^. filepath)
               , ex ^. content
               ]
        wd <- pwd
        procs
            "gcc"
            [ "-c"
            , "-I"
            , encodeText wd
            , "-Wall"
            , "-Werror"
            , "-Wextra"
            , "-pedantic"
            , encodeText src
            ]
            empty

encodeText :: FilePath -> Text
encodeText = fromString . encodeString
