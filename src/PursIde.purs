module PursIde where

import Data.Maybe (Maybe)

type ModuleName
  = String

data Matcher
  = Flex String
  | Distance String Int

data Filter
  = ExactFilter String
  | PrefixFilter String
  | ModuleFilter (Array String)
  | NamespaceFilter (Array Namespace)
  | DeclarationTypeFilter (Array DeclarationType)

type CompletionOptions
  = { maxResults :: Maybe Int
    , groupReexports :: Boolean
    }

data Command
  = Cwd
  | Ls ListType
  | Quit
  | Reset
  | Load (Array ModuleName)
  | Complete
    { filters :: Array Filter
    , matcher :: Maybe Matcher
    , currentModule :: Maybe ModuleName
    , completionOptions :: CompletionOptions
    }
  | Type
    { search :: String
    , filters :: Array Filter
    , currentModule :: Maybe String
    }
  | Usages
    { module :: ModuleName
    , identifier :: String
    , namespace :: Namespace
    }
  | ImportCmd
    { filepath :: FileName
    , actualFilepath :: Maybe FileName
    , filters :: Array Filter
    , importCommand :: ImportCommand
    }
  | RebuildCmd
    { file :: String
    , actualFile :: Maybe FileName
    , codegen :: Maybe (Array CodegenTarget)
    }

data ListType
  = Imports String
  | AvailableModules

data Namespace
  = NSValue
  | NSType
  | NSModule

data DeclarationType
  = ValueDT
  | TypeDT
  | SynonymDT
  | DataConstructorDT
  | TypeClassDT
  | ValueOperatorDT
  | TypeOperatorDT
  | ModuleDT

data CodegenTarget
  = JS
  | JSSourceMap
  | CoreFn
  | Docs
  | Other String

type FileName
  = String

data ImportCommand
  = AddImplicitImport String
  | AddQualifiedImport String String
  | AddImport String (Maybe String)

type GenCompletion a
  = { type :: String
    , identifier :: String
    , module :: String
    | a
    }

type TypePosition
  = { name :: String
    , start :: Position
    , end :: Position
    }

type TypeInfo
  = ( GenCompletion
        ( definedAt :: Maybe TypePosition
        , expandedType :: Maybe String
        , documentation :: Maybe String
        , exportedFrom :: Array String
        )
    )

type ModuleList
  = Array String

type ImportList
  = { moduleName :: Maybe String, imports :: Array Import }

type Import
  = { moduleName :: String
    , importType :: ImportType
    , qualifier :: Maybe String
    }

data ImportType
  = Implicit
  | Explicit (Array String)
  | Hiding (Array String)

type Position
  = { line :: Int, column :: Int }

type RangePosition
  = { startLine :: Int
    , startColumn :: Int
    , endLine :: Int
    , endColumn :: Int
    }

type RebuildError
  = { position :: Maybe RangePosition
    , moduleName :: Maybe String
    , filename :: Maybe String
    , errorCode :: String
    , message :: String
    , errorLink :: String
    , pursIde :: Maybe PursIdeInfo
    , suggestion :: Maybe Suggestion
    }

type Suggestion
  = { replacement :: String
    , replaceRange :: Maybe RangePosition
    }

type PursIdeInfo
  = { name :: String
    , completions :: Array TypeInfo
    }

data ImportResult
  = SuccessFile String
  | SuccessText (Array String)
  | MultipleResults (Array TypeInfo)
