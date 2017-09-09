{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Exception (SomeException)
import Options.Generic (Generic, ParseRecord, type (<?>))
import Text.Trifecta.Delta (Delta(..))

import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy.IO
import qualified Dhall
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Options.Generic
import qualified System.Exit
import qualified System.IO

data Options = Options
    { explain :: Bool <?> "Explain error messages in detail"
    , pretty  :: Bool <?> "Pretty print generated JSON"
    } deriving (Generic, ParseRecord)

main :: IO ()
main = handle (do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    Options {..} <- Options.Generic.getRecord "Compile Dhall to JSON"

    (if Options.Generic.unHelpful explain then Dhall.detailed else id) (do
        inText <- Data.Text.Lazy.IO.getContents

        expr <- case Dhall.Parser.exprFromText (Directed "(stdin)" 0 0 0 0) inText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        expr' <- Dhall.Import.load expr
        case Dhall.TypeCheck.typeOf expr' of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()

        json <- case Dhall.JSON.dhallToJSON expr' of
            Left err  -> Control.Exception.throwIO err
            Right json -> return json

        let encode =
                if   Options.Generic.unHelpful pretty
                then Data.Aeson.Encode.Pretty.encodePretty
                else Data.Aeson.encode
        Data.ByteString.Lazy.putStr (encode json) ))

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
