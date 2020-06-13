module PursIde.Codec where

import Prelude

import CodecExt as CExt
import Data.Codec as C
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as JA
import Data.Codec.Argonaut.Compat as JAC
import Data.Codec.Argonaut.Record as JAR
import Data.Codec.Argonaut.Variant as JAV
import Data.Either (Either(..))
import Data.Functor.Variant (SProxy(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Variant as V
import PursIde (CodegenTarget(..), DeclarationType(..), ImportCommand(..), Namespace(..))

namespaceCodec :: JsonCodec Namespace
namespaceCodec = CExt.enumSum "Namespace" encode decode
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
codegenTargetCodec = CExt.enumSum "CodegenTarget" encode decode
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
declarationTypeCodec = CExt.enumSum "DeclarationType" encode decode
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
  CExt.flatTagged "importCommand"
    C.>~> dimap toVariant fromVariant
        ( JAV.variantMatch
            { addImplicitImport: Right (JAR.object "implicitImport" { module: JA.string })
            , addQualifiedImport: Right (JAR.object "qualifiedImport" { module: JA.string, qualifier: JA.string })
            , addImport: Right (CExt.withNullables [ "qualifier" ] C.>~> JAR.object "addImport" { identifier: JA.string, qualifier: JAC.maybe JA.string })
            }
        )
  where
  toVariant = case _ of
    AddImplicitImport name -> V.inj (SProxy ∷ _ "addImplicitImport") { module: name }
    AddQualifiedImport name qualifier -> V.inj (SProxy ∷ _ "addQualifiedImport") { module: name, qualifier }
    AddImport identifier qualifier -> V.inj (SProxy ∷ _ "addImport") { identifier, qualifier }

  fromVariant =
    V.match
      { addImplicitImport: AddImplicitImport <<< _.module
      , addQualifiedImport: \{ module: name, qualifier } -> AddQualifiedImport name qualifier
      , addImport: \{ identifier, qualifier } -> AddImport identifier qualifier
      }
