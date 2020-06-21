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
import PursIde (CodegenTarget(..), Command(..), Completion, CompletionOptions, DeclarationType(..), Filter(..), HoleFits, Import, ImportCommand(..), ImportList, ImportType(..), ListType(..), Matcher(..), Namespace(..), Position, Range, RangePosition, RebuildError, Suggestion)
import Record as Record

matcherCodec :: JsonCodec Matcher
matcherCodec =
  CExt.remapField "matcher" "tag"
    C.>~> CExt.remapField "params" "value"
    C.>~> dimap toVariant fromVariant
        ( JAV.variantMatch
            { flex: Right (JAR.object "flex" { search: JA.string })
            , distance: Right (JAR.object "distance" { search: JA.string, maximumDistance: JA.int })
            }
        )
  where
  toVariant = case _ of
    Flex flex -> V.inj (SProxy :: _ "flex") flex
    Distance dist -> V.inj (SProxy :: _ "distance") dist

  fromVariant =
    V.match
      { flex: Flex
      , distance: Distance
      }

filterCodec :: JsonCodec Filter
filterCodec =
  CExt.remapField "filter" "tag"
    C.>~> CExt.remapField "params" "value"
    C.>~> dimap toVariant fromVariant
        ( JAV.variantMatch
            { exact: Right (JAR.object "exactFilter" { search: JA.string })
            , prefix: Right (JAR.object "prefixFilter" { search: JA.string })
            , modules: Right (JAR.object "moduleFilter" { modules: JA.array JA.string })
            , namespace: Right (JAR.object "namespaceFilter" { namespaces: JA.array namespaceCodec })
            , declarations: Right (JA.array declarationTypeCodec)
            }
        )
  where
  toVariant = case _ of
    ExactFilter search -> V.inj (SProxy :: _ "exact") { search }
    PrefixFilter search -> V.inj (SProxy :: _ "prefix") { search }
    ModuleFilter modules -> V.inj (SProxy :: _ "modules") { modules }
    NamespaceFilter namespaces -> V.inj (SProxy :: _ "namespace") { namespaces }
    DeclarationTypeFilter declTypes -> V.inj (SProxy :: _ "declarations") declTypes

  fromVariant =
    V.match
      { exact: ExactFilter <<< _.search
      , prefix: PrefixFilter <<< _.search
      , modules: ModuleFilter <<< _.modules
      , namespace: NamespaceFilter <<< _.namespaces
      , declarations: DeclarationTypeFilter
      }


completionOptionsCodec :: JsonCodec CompletionOptions
completionOptionsCodec =
  JAR.object "CompletionOptions"
    { groupReexports: JA.boolean
    , maxResults: JAC.maybe JA.int
    }

commandCodec :: JsonCodec Command
commandCodec =
  CExt.remapField "command" "tag"
    C.>~> CExt.remapField "params" "value"
    C.>~> dimap toVariant fromVariant
        ( JAV.variantMatch
            { cwd: Left unit
            , list: Right listTypeCodec
            , quit: Left unit
            , reset: Left unit
            , load: Right (JAR.object "LoadCommand" { modules: JA.array JA.string })
            , complete:
                Right
                  ( JAR.object "CompleteCommand"
                      { filters: JA.array filterCodec
                      , matcher: JAC.maybe matcherCodec
                      , currentModule: JAC.maybe JA.string
                      , completionOptions: completionOptionsCodec
                      }
                  )
            , type:
                Right
                  ( JAR.object "TypeCommand"
                      { search: JA.string
                      , filters: JA.array filterCodec
                      , currentModule: JAC.maybe JA.string
                      }
                  )
            , usages:
                Right
                  ( JAR.object "UsagesCommand"
                      { module: JA.string
                      , namespace: namespaceCodec
                      , identifier: JAC.string
                      }
                  )
            , import:
                Right
                  ( JAR.object "ImportCommand"
                      { filepath: JA.string
                      , actualFilepath: JAC.maybe JA.string
                      , filters: JA.array filterCodec
                      , importCommand: importCommandCodec
                      }
                  )
            , rebuild:
                Right
                  ( JAR.object "RebuildCommand"
                      { file: JA.string
                      , actualFile: JAC.maybe JA.string
                      , codegen: JA.array codegenTargetCodec
                      }
                  )
            }
        )
  where
  toVariant = case _ of
    Cwd -> V.inj (SProxy :: _ "cwd") unit
    Ls ty -> V.inj (SProxy :: _ "list") ty
    Quit -> V.inj (SProxy :: _ "quit") unit
    Reset -> V.inj (SProxy :: _ "reset") unit
    Load modules -> V.inj (SProxy :: _ "load") modules
    Complete complete -> V.inj (SProxy :: _ "complete") complete
    Type ty -> V.inj (SProxy :: _ "type") ty
    Usages params -> V.inj (SProxy :: _ "usages") params
    ImportCmd params -> V.inj (SProxy :: _ "import") params
    RebuildCmd params -> V.inj (SProxy :: _ "rebuild") params

  fromVariant =
    V.match
      { cwd: \_ -> Cwd
      , list: Ls
      , quit: \_ -> Quit
      , reset: \_ -> Reset
      , load: Load
      , complete: Complete
      , type: Type
      , usages: Usages
      , import: ImportCmd
      , rebuild: RebuildCmd
      }

listTypeCodec :: JsonCodec ListType
listTypeCodec =
  CExt.flatTagged "type"
    C.>~> dimap toVariant fromVariant
        ( JAV.variantMatch
            { availableModules: Left unit
            , imports: Right JA.string
            }
        )
  where
  toVariant = case _ of
    AvailableModules -> V.inj (SProxy :: _ "availableModules") unit
    Imports file -> V.inj (SProxy :: _ "imports") file

  fromVariant =
    V.match
      { availableModules: \_ -> AvailableModules
      , imports: Imports
      }

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

importCommandCodec :: JsonCodec ImportCommand
importCommandCodec =
  CExt.flatTagged "importCommand"
    C.>~> dimap toVariant fromVariant
        ( JAV.variantMatch
            { addImplicitImport: Right (JAR.object "implicitImport" { module: JA.string })
            , addQualifiedImport: Right (JAR.object "qualifiedImport" { module: JA.string, qualifier: JA.string })
            , addImport:
                Right
                  ( CExt.withNullables [ "qualifier" ]
                      C.>~> JAR.object "addImport"
                          { identifier: JA.string
                          , qualifier: JAC.maybe JA.string
                          }
                  )
            }
        )
  where
  toVariant = case _ of
    AddImplicitImport imp -> V.inj (SProxy ∷ _ "addImplicitImport") imp
    AddQualifiedImport imp -> V.inj (SProxy ∷ _ "addQualifiedImport") imp
    AddImport imp -> V.inj (SProxy ∷ _ "addImport") imp

  fromVariant =
    V.match
      { addImplicitImport: AddImplicitImport
      , addQualifiedImport: AddQualifiedImport
      , addImport: AddImport
      }

positionCodec :: JsonCodec Position
positionCodec =
  JAR.object "Position"
    { line: JA.int
    , column: JA.int
    }

rangeCodec :: JsonCodec Range
rangeCodec =
  JAR.object "Range"
    { name: JA.string
    , start: positionCodec
    , end: positionCodec
    }

completionCodec :: JsonCodec Completion
completionCodec =
  JAR.object "Completion"
    { type: JA.string
    , identifier: JA.string
    , module: JA.string
    , definedAt: JAC.maybe rangeCodec
    , expandedType: JAC.maybe JA.string
    , documentation: JAC.maybe JA.string
    , exportedFrom: JA.array JA.string
    }

importListCodec :: JsonCodec ImportList
importListCodec = JAR.object "ImportList" { moduleName: JA.string, imports: JA.array importCodec}

importCodec :: JsonCodec Import
importCodec =
  CExt.withNullables ["qualifier"] C.>~>
  CExt.flatTagged "importType" C.>~>
  dimap toVariant fromVariant
    ( JAV.variantMatch
        { implicit: Right (JAR.object "Implicit Import" { module: JA.string, qualifier: JAC.maybe JA.string})
        , explicit: Right (JAR.object "Explicit Import" { module: JA.string, qualifier: JAC.maybe JA.string, identifiers: JA.array JA.string })
        , hiding: Right (JAR.object "Hiding Import" { module: JA.string, qualifier: JAC.maybe JA.string, identifiers: JA.array JA.string })
        }
    )
  where
  toVariant { module: mn, qualifier, importType} = case importType of
    Implicit -> V.inj (SProxy ∷ _ "implicit") { module: mn, qualifier }
    Explicit identifiers -> V.inj (SProxy ∷ _ "explicit") { module: mn, qualifier, identifiers }
    Hiding identifiers -> V.inj (SProxy ∷ _ "hiding") { module: mn, qualifier, identifiers }

  fromVariant =
    V.match
      { implicit: Record.insert (SProxy :: _"importType") Implicit
      , explicit: \{ module: name, qualifier, identifiers } -> {module: name, qualifier, importType: Explicit identifiers}
      , hiding: \{ module: name, qualifier, identifiers } -> {module: name, qualifier, importType: Hiding identifiers}
      }

rangePositionCodec :: JsonCodec RangePosition
rangePositionCodec =
  JAR.object "RangePosition"
    { startLine: JA.int
    , startColumn: JA.int
    , endLine: JA.int
    , endColumn: JA.int
    }

rebuildErrorCodec :: JsonCodec RebuildError
rebuildErrorCodec =
  JAR.object "RebuildError"
    { position: JAC.maybe rangePositionCodec
    , moduleName: JAC.maybe JA.string
    , filename: JAC.maybe JA.string
    , errorCode: JA.string
    , message: JA.string
    , errorLink: JA.string
    , pursIde: JAC.maybe holeFitsCodec
    , suggestion: JAC.maybe suggestionCodec
    }

suggestionCodec :: JsonCodec Suggestion
suggestionCodec =
  JAR.object "Suggestion"
    { replacement: JA.string
    , replaceRange: JAC.maybe rangePositionCodec
    }

holeFitsCodec :: JsonCodec HoleFits
holeFitsCodec =
  JAR.object "HoleFits"
    { name: JA.string
    , completions: JA.array completionCodec
    }
