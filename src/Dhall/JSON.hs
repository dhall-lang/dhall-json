{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This library only exports a single `dhallToJSON` function for translating a
    Dhall syntax tree to a JSON syntax tree (i.e. a `Value`) for the @aeson@
    library

    NOTE: The @yaml@ library uses the same `Value` type to represent YAML
    files, so you can use this to convert Dhall expressions to YAML, too

    See the @dhall@ package if you would like to transform Dhall source code
    into a Dhall syntax tree.  Similarly, see the @aeson@ package if you would
    like to translate a JSON syntax tree into JSON.

    This package also provides @dhall-to-json@ and @dhall-to-yaml@ executables
    which you can use to compile Dhall source code directly to JSON or YAML for
    your convenience

    Not all Dhall expressions can be converted to JSON since JSON is not a
    programming language.  The only things you can convert are:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@
    * @List@s
    * @Optional@ values
    * unions
    * records

    Dhall @Bool@s translate to JSON bools:

> $ dhall-to-json <<< 'True'
> true
> $ dhall-to-json <<< 'False'
> false

    Dhall numbers translate to JSON numbers:

> $ dhall-to-json <<< '+2'
> 2
> $ dhall-to-json <<< '2'
> 2
> $ dhall-to-json <<< '2.3'
> 2.3

    Dhall @Text@ translates to JSON text:

> $ dhall-to-json <<< '"ABC"'
> "ABC"

    Dhall @List@s translate to JSON lists:

> $ dhall-to-json <<< '[1, 2, 3] : List Integer'
> [1,2,3]

    Dhall @Optional@ values translate to @null@ if absent and the unwrapped
    value otherwise:

> $ dhall-to-json <<< '[] : Optional Integer'
> null
> $ dhall-to-json <<< '[1] : Optional Integer'
> 1

    Dhall records translate to JSON records:

> $ dhall-to-json <<< '{ foo = 1, bar = True }'
> {"foo":1,"bar":true}

    Dhall unions translate to the wrapped value:

> $ dhall-to-json <<< "< Left = +2 | Right : Natural>"
> 2
> $ cat config
> [ < Person = { age = +47, name = "John" }
>   | Place  : { location : Text }
>   >
> , < Place  = { location = "North Pole" }
>   | Person : { age : Natural, name : Text }
>   >
> , < Place  = { location = "Sahara Desert" }
>   | Person : { age : Natural, name : Text }
>   >
> , < Person = { age = +35, name = "Alice" }
>   | Place  : { location : Text }
>   >
> ]
> $ dhall-to-json <<< "./config"
> [{"age":47,"name":"John"},{"location":"North Pole"},{"location":"Sahara Desert"},{"age":35,"name":"Alice"}]

    Also, all Dhall expressions are normalized before translation to JSON:

> $ dhall-to-json <<< "True == False"
> false

-}

module Dhall.JSON (
    -- * Dhall to JSON
      dhallToJSON
    , omitNull

    -- * Exceptions
    , CompileError(..)
    ) where

import Control.Exception (Exception)
import Data.Aeson (Value(..))
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)

import qualified Data.Aeson
import qualified Data.HashMap.Strict
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Dhall.Core

{-| This is the exception type for errors that might arise when translating
    Dhall to JSON

    Because the majority of Dhall language features do not translate to JSON
    this just returns the expression that failed
-}
data CompileError = Unsupported (Expr X X) deriving (Typeable)

instance Show CompileError where
    show (Unsupported e) =
        Data.Text.unpack $
            "" <> _ERROR <> ": Cannot translate to JSON                                     \n\
            \                                                                               \n\
            \Explanation: Only primitive values, records, unions, ❰List❱s, and ❰Optional❱   \n\
            \values can be translated from Dhall to JSON                                    \n\
            \                                                                               \n\
            \The following Dhall expression could not be translated to JSON:                \n\
            \                                                                               \n\
            \↳ " <> txt <> "                                                                "
      where
        txt = Data.Text.Lazy.toStrict (Dhall.Core.pretty e)

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Exception CompileError

{-| Convert a Dhall expression to the equivalent Nix expression

>>> :set -XOverloadedStrings
>>> :set -XOverloadedLists
>>> import Dhall.Core
>>> dhallToJSON (RecordLit [("foo", IntegerLit 1), ("bar", TextLit "ABC")])
Right (Object (fromList [("foo",Number 1.0),("bar",String "ABC")]))
>>> fmap Data.Aeson.encode it
Right "{\"foo\":1,\"bar\":\"ABC\"}"
-}
dhallToJSON :: Expr s X -> Either CompileError Value
dhallToJSON e0 = loop (Dhall.Core.normalize e0)
  where
    loop e = case e of 
        Dhall.Core.BoolLit a -> return (Data.Aeson.toJSON a)
        Dhall.Core.NaturalLit a -> return (Data.Aeson.toJSON a)
        Dhall.Core.IntegerLit a -> return (Data.Aeson.toJSON a)
        Dhall.Core.DoubleLit a -> return (Data.Aeson.toJSON a)
        Dhall.Core.TextLit (Dhall.Core.Chunks [] a) -> do
            return (Data.Aeson.toJSON (Data.Text.Lazy.Builder.toLazyText a))
        Dhall.Core.ListLit _ a -> do
            a' <- traverse loop a
            return (Data.Aeson.toJSON a')
        Dhall.Core.OptionalLit _ a -> do
            a' <- traverse loop a
            return (Data.Aeson.toJSON a')
        Dhall.Core.RecordLit a -> do
            a' <- traverse loop a
            return (Data.Aeson.toJSON a')
        Dhall.Core.UnionLit _ b _ -> loop b
        _ -> Left (Unsupported e)

-- | Omit record fields that are @null@
omitNull :: Value -> Value
omitNull (Object object) =
    Object (fmap omitNull (Data.HashMap.Strict.filter (/= Null) object))
omitNull (Array array) =
    Array (fmap omitNull array)
omitNull (String string) =
    String string
omitNull (Number number) =
    Number number
omitNull (Bool bool) =
    Bool bool
omitNull Null =
    Null
