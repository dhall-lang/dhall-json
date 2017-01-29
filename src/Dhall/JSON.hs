{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

module Dhall.JSON where

import Control.Exception (Exception)
import Data.Aeson (Value)
import Data.Text.Buildable (Buildable)
import Data.Typeable (Typeable)
import Dhall.Core (Expr)

import qualified Data.Aeson
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified Dhall.Core
import qualified NeatInterpolation

data CompileError s a = Unsupported (Expr s a) deriving (Typeable)

instance Buildable a => Show (CompileError s a) where
    show (Unsupported e) =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot translate to JSON

Explanation: Only primitive values, records, ❰List❱s, and ❰Optional❱ values can
be translated from Dhall to JSON

The following Dhall expression could not be translated to JSON:

↳ $txt
|]
      where
        txt = Data.Text.Lazy.toStrict (Dhall.Core.pretty e)

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance (Buildable a, Typeable a, Typeable s) => Exception (CompileError s a)

dhallToJSON :: Expr s a -> Either (CompileError s a) Value
dhallToJSON e0 = loop (Dhall.Core.normalize e0)
  where
    loop :: Expr s a -> Either (CompileError s a) Value
    loop e = case e of 
        Dhall.Core.BoolLit a -> return (Data.Aeson.toJSON a)
        Dhall.Core.NaturalLit a -> return (Data.Aeson.toJSON a)
        Dhall.Core.IntegerLit a -> return (Data.Aeson.toJSON a)
        Dhall.Core.DoubleLit a -> return (Data.Aeson.toJSON a)
        Dhall.Core.TextLit a -> do
            return (Data.Aeson.toJSON (Data.Text.Lazy.Builder.toLazyText a))
        Dhall.Core.ListLit _ a -> do
            a' <- traverse loop a
            return (Data.Aeson.toJSON a')
        Dhall.Core.OptionalLit _ a ->
            if Data.Vector.null a
                then return (Data.Aeson.toJSON (Nothing :: Maybe ()))
                else do
                    b <- dhallToJSON (Data.Vector.head a)
                    return (Data.Aeson.toJSON (Just b))
        Dhall.Core.RecordLit a -> do
            a' <- traverse loop a
            return (Data.Aeson.toJSON a')
        _ -> Left (Unsupported e)
