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
    , codeToValue

    -- * Exceptions
    , CompileError(..)
    ) where

import Control.Applicative (empty)
import Control.Monad (guard)
import Control.Exception (Exception, throwIO)
import Data.Aeson (Value(..))
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Typeable (Typeable)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)

import qualified Data.Aeson
import qualified Data.Foldable
import qualified Data.HashMap.Strict
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck

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

{-| Convert a Dhall expression to the equivalent JSON expression

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

convertToHomogeneousMaps :: Text -> Text -> Expr s X -> Expr s X
convertToHomogeneousMaps keyField valueField e0 = loop (Dhall.Core.normalize e0)
  where
    loop e = case e of
        Dhall.Core.Const a ->
            Dhall.Core.Const a

        Dhall.Core.Var v ->
            Dhall.Core.Var v

        Dhall.Core.Lam a b c ->
            Dhall.Core.Lam a b' c'
          where
            b' = loop b
            c' = loop c

        Dhall.Core.Pi a b c ->
            Dhall.Core.Pi a b' c'
          where
            b' = loop b
            c' = loop c

        Dhall.Core.App a b ->
            Dhall.Core.App a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Let a b c d ->
            Dhall.Core.Let a b' c' d'
          where
            b' = fmap loop b
            c' =      loop c
            d' =      loop d

        Dhall.Core.Annot a b ->
            Dhall.Core.Annot a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Bool ->
            Dhall.Core.Bool

        Dhall.Core.BoolLit a ->
            Dhall.Core.BoolLit a

        Dhall.Core.BoolAnd a b ->
            Dhall.Core.BoolAnd a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolOr a b ->
            Dhall.Core.BoolOr a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolEQ a b ->
            Dhall.Core.BoolEQ a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolNE a b ->
            Dhall.Core.BoolNE a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.BoolIf a b c ->
            Dhall.Core.BoolIf a' b' c'
          where
            a' = loop a
            b' = loop b
            c' = loop c

        Dhall.Core.Natural ->
            Dhall.Core.Natural

        Dhall.Core.NaturalLit a ->
            Dhall.Core.NaturalLit a

        Dhall.Core.NaturalFold ->
            Dhall.Core.NaturalFold

        Dhall.Core.NaturalBuild ->
            Dhall.Core.NaturalBuild

        Dhall.Core.NaturalIsZero ->
            Dhall.Core.NaturalIsZero

        Dhall.Core.NaturalEven ->
            Dhall.Core.NaturalEven

        Dhall.Core.NaturalOdd ->
            Dhall.Core.NaturalOdd

        Dhall.Core.NaturalToInteger ->
            Dhall.Core.NaturalToInteger

        Dhall.Core.NaturalShow ->
            Dhall.Core.NaturalShow

        Dhall.Core.NaturalPlus a b ->
            Dhall.Core.NaturalPlus a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.NaturalTimes a b ->
            Dhall.Core.NaturalTimes a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Integer ->
            Dhall.Core.Integer

        Dhall.Core.IntegerLit a ->
            Dhall.Core.IntegerLit a

        Dhall.Core.IntegerShow ->
            Dhall.Core.IntegerShow

        Dhall.Core.Double ->
            Dhall.Core.Double

        Dhall.Core.DoubleLit a ->
            Dhall.Core.DoubleLit a

        Dhall.Core.DoubleShow ->
            Dhall.Core.DoubleShow

        Dhall.Core.Text ->
            Dhall.Core.Text

        Dhall.Core.TextLit (Dhall.Core.Chunks a b) ->
            Dhall.Core.TextLit (Dhall.Core.Chunks a' b)
          where
            a' = fmap (fmap loop) a

        Dhall.Core.TextAppend a b ->
            Dhall.Core.TextAppend a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.List ->
            Dhall.Core.List

        Dhall.Core.ListLit a b ->
            case transform of
                Just c  -> c
                Nothing -> Dhall.Core.ListLit a' b'
          where
            elements = Data.Foldable.toList b

            toKeyValue :: Expr s X -> Maybe (Text, Expr s X)
            toKeyValue (Dhall.Core.RecordLit m) = do
                guard (Data.HashMap.Strict.InsOrd.size m == 2)

                key   <- Data.HashMap.Strict.InsOrd.lookup keyField   m
                value <- Data.HashMap.Strict.InsOrd.lookup valueField m

                keyText <- case key of
                    Dhall.Core.TextLit (Dhall.Core.Chunks [] keyText) ->
                        return keyText

                    _ ->
                        empty

                return (Data.Text.Lazy.Builder.toLazyText keyText, value)
            toKeyValue _ = do
                empty

            transform =
                case elements of
                    [] ->
                        case a of
                            Just (Dhall.Core.Record m) -> do
                                guard (Data.HashMap.Strict.InsOrd.size m == 2)
                                guard (Data.HashMap.Strict.InsOrd.member keyField   m)
                                guard (Data.HashMap.Strict.InsOrd.member valueField m)
                                return (Dhall.Core.RecordLit Data.HashMap.Strict.InsOrd.empty)
                            _ -> do
                                empty

                    _  -> do
                        keyValues <- traverse toKeyValue elements

                        let recordLiteral =
                                Data.HashMap.Strict.InsOrd.fromList keyValues

                        return (Dhall.Core.RecordLit recordLiteral)

            a' = fmap loop a
            b' = fmap loop b

        Dhall.Core.ListAppend a b ->
            Dhall.Core.ListAppend a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.ListBuild ->
            Dhall.Core.ListBuild

        Dhall.Core.ListFold ->
            Dhall.Core.ListFold

        Dhall.Core.ListLength ->
            Dhall.Core.ListLength

        Dhall.Core.ListHead ->
            Dhall.Core.ListHead

        Dhall.Core.ListLast ->
            Dhall.Core.ListLast

        Dhall.Core.ListIndexed ->
            Dhall.Core.ListIndexed

        Dhall.Core.ListReverse ->
            Dhall.Core.ListReverse

        Dhall.Core.Optional ->
            Dhall.Core.Optional

        Dhall.Core.OptionalLit a b ->
            Dhall.Core.OptionalLit a' b'
          where
            a' =      loop a
            b' = fmap loop b

        Dhall.Core.OptionalFold ->
            Dhall.Core.OptionalFold

        Dhall.Core.OptionalBuild ->
            Dhall.Core.OptionalBuild

        Dhall.Core.Record a ->
            Dhall.Core.Record a'
          where
            a' = fmap loop a

        Dhall.Core.RecordLit a ->
            Dhall.Core.RecordLit a'
          where
            a' = fmap loop a

        Dhall.Core.Union a ->
            Dhall.Core.Union a'
          where
            a' = fmap loop a

        Dhall.Core.UnionLit a b c ->
            Dhall.Core.UnionLit a b' c'
          where
            b' =      loop b
            c' = fmap loop c

        Dhall.Core.Combine a b ->
            Dhall.Core.Combine a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.CombineTypes a b ->
            Dhall.Core.CombineTypes a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Prefer a b ->
            Dhall.Core.Prefer a' b'
          where
            a' = loop a
            b' = loop b

        Dhall.Core.Merge a b c ->
            Dhall.Core.Merge a' b' c'
          where
            a' =      loop a
            b' =      loop b
            c' = fmap loop c

        Dhall.Core.Constructors a ->
            Dhall.Core.Constructors a'
          where
            a' = loop a

        Dhall.Core.Field a b ->
            Dhall.Core.Field a' b
          where
            a' = loop a

        Dhall.Core.Project a b ->
            Dhall.Core.Project a' b
          where
            a' = loop a

        Dhall.Core.Note a b ->
            Dhall.Core.Note a b'
          where
            b' = loop b

        Dhall.Core.Embed a ->
            Dhall.Core.Embed a

{-| Convert a piece of Text carrying a Dhall inscription to an equivalent JSON Value

>>> :set -XOverloadedStrings
>>> import Dhall.Core
>>> Dhall.JSON.codeToValue "(stdin)" "{ a = 1 }"
>>> Object (fromList [("a",Number 1.0)])
-}
codeToValue
  :: Data.Text.Text  -- ^ Describe the input for the sake of error location.
  -> Data.Text.Text  -- ^ Input text.
  -> IO Value
codeToValue name code = do
    parsedExpression <- case Dhall.Parser.exprFromText (Data.Text.unpack name) (Data.Text.Lazy.fromStrict code) of
      Left  err              -> Control.Exception.throwIO err
      Right parsedExpression -> return parsedExpression

    resolvedExpression <- Dhall.Import.load parsedExpression

    case Dhall.TypeCheck.typeOf resolvedExpression  of
      Left  err -> Control.Exception.throwIO err
      Right _   -> return ()

    let convertedExpression =
            convertToHomogeneousMaps "mapKey" "mapValue" resolvedExpression

    case dhallToJSON convertedExpression of
      Left  err  -> Control.Exception.throwIO err
      Right json -> return json
