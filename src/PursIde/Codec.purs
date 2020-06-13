module PursIde.Codec where

import Prelude
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer (Writer, writer)
import Data.Argonaut.Core as J
import Data.Bifunctor (lmap)
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))
import PursIde (CodegenTarget(..), DeclarationType(..), Namespace(..))

-- | A helper for defining JSON codecs for "enum" sum types, where every
-- | constructor is nullary, and the type will be encoded as a string.
enumSum ∷
  ∀ a.
  String →
  (a → String) →
  (String → Maybe a) →
  CA.JsonCodec a
enumSum name printTag parseTag = C.GCodec dec enc
  where
  dec ∷ ReaderT J.Json (Either CA.JsonDecodeError) a
  dec =
    ReaderT \j →
      lmap (CA.Named name) do
        value ← CA.decode CA.string j
        case parseTag value of
          Just a → Right a
          Nothing → Left (CA.UnexpectedValue j)

  enc ∷ Star (Writer J.Json) a a
  enc = Star \a → writer $ Tuple a (CA.encode CA.string (printTag a))

namespaceCodec :: CA.JsonCodec Namespace
namespaceCodec = enumSum "Namespace" encode decode
  where
  decode = case _ of
    "value" -> Just NSValue
    "type" -> Just NSType
    "module" -> Just NSModule
    _ -> Nothing

  encode = case _ of
    NSValue -> "value"
    NSType -> "type"
    NSModule -> "module"

codegenTargetCodec :: CA.JsonCodec CodegenTarget
codegenTargetCodec = enumSum "CodegenTarget" encode decode
  where
  encode = case _ of
    JS -> "js"
    JSSourceMap -> "sourcemaps"
    CoreFn -> "corefn"
    Docs -> "docs"
    Other o -> o

  decode s =
    Just case s of
      "js" -> JS
      "sourcemaps" -> JSSourceMap
      "corefn" -> CoreFn
      "docs" -> Docs
      o -> Other o

declarationTypeCodec :: CA.JsonCodec DeclarationType
declarationTypeCodec = enumSum "DeclarationType" encode decode
  where
  encode = case _ of
    ValueDT -> "value"
    TypeDT -> "type"
    SynonymDT -> "synonym"
    DataConstructorDT -> "dataconstructor"
    TypeClassDT -> "typeclass"
    ValueOperatorDT -> "valueoperator"
    TypeOperatorDT -> "typeoperator"
    ModuleDT -> "module"

  decode = case _ of
    "value" -> Just ValueDT
    "type" -> Just TypeDT
    "synonym" -> Just SynonymDT
    "dataconstructor" -> Just DataConstructorDT
    "typeclass" -> Just TypeClassDT
    "valueoperator" -> Just ValueOperatorDT
    "typeoperator" -> Just TypeOperatorDT
    "module" -> Just ModuleDT
    _ -> Nothing
