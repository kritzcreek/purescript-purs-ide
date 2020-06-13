module PursIde.Codec where

import Prelude

import CodecExt as CExt
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer (Writer, writer)
import Data.Argonaut.Core as J
import Data.Bifunctor (lmap)
import Data.Codec as C
import Data.Codec.Argonaut (JsonDecodeError(..), JsonCodec)
import Data.Codec.Argonaut as JA
import Data.Codec.Argonaut.Compat as JAC
import Data.Codec.Argonaut.Record as JAR
import Data.Codec.Argonaut.Variant as JAV
import Data.Either (Either(..))
import Data.Functor.Variant (SProxy(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (dimap)
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant as V
import Foreign.Object as FO
import PursIde (CodegenTarget(..), DeclarationType(..), ImportCommand(..), ImportType(..), Namespace(..))

-- | A helper for defining JSON codecs for "enum" sum types, where every
-- | constructor is nullary, and the type will be encoded as a string.
enumSum ∷
  ∀ a.
  String →
  (a → String) →
  (String → Maybe a) →
  JsonCodec a
enumSum name printTag parseTag = C.GCodec dec enc
  where
  dec ∷ ReaderT J.Json (Either JsonDecodeError) a
  dec =
    ReaderT \j →
      lmap (Named name) do
        value ← JA.decode JA.string j
        case parseTag value of
          Just a → Right a
          Nothing → Left (UnexpectedValue j)

  enc ∷ Star (Writer J.Json) a a
  enc = Star \a → writer $ Tuple a (JA.encode JA.string (printTag a))

namespaceCodec :: JsonCodec Namespace
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

codegenTargetCodec :: JsonCodec CodegenTarget
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

declarationTypeCodec :: JsonCodec DeclarationType
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

importCommandCodec :: JsonCodec ImportCommand
importCommandCodec = 
  CExt.flatTagged "importCommand" C.>~> dimap toVariant fromVariant
    (JAV.variantMatch
      { addImplicitImport: Right (JAR.object "implicitImport" { module: JA.string })
      , addQualifiedImport: Right (JAR.object "qualifiedImport" { module: JA.string, qualifier: JA.string })
      , addImport: Right (CExt.withNullables ["qualifier"] C.>~> JAR.object "addImport" { identifier: JA.string, qualifier: JAC.maybe JA.string})
      })
  where
  toVariant = case _ of
     AddImplicitImport name ->
      V.inj (SProxy ∷ _ "addImplicitImport") { module: name }
     AddQualifiedImport name qualifier ->
      V.inj (SProxy ∷ _ "addQualifiedImport") { module: name, qualifier }
     AddImport identifier qualifier ->
       V.inj (SProxy ∷ _ "addImport") { identifier, qualifier }
     
  fromVariant = V.match
    { addImplicitImport: AddImplicitImport <<< _.module
    , addQualifiedImport: \{ module: name, qualifier } -> AddQualifiedImport name qualifier
    , addImport: \{ identifier, qualifier } -> AddImport identifier qualifier
    }