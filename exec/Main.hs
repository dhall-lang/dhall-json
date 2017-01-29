{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException)
import Text.Trifecta.Delta (Delta(..))

import qualified Control.Exception
import qualified Data.Aeson
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

main :: IO ()
main = handle (Dhall.detailed (do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    () <- Options.Generic.getRecord "Compile Dhall to JSON"

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
    Data.ByteString.Lazy.putStr (Data.Aeson.encode json) ))

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
