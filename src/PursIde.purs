module PursIde where

import Data.Maybe (Maybe)

type ModuleName
  = String

type FileName
  = String

data Matcher
  = Flex { search :: String }
  | Distance { search :: String, maximumDistance :: Int }

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
  | Load { modules :: Array ModuleName }
  | Complete
    { filters :: Array Filter
    , matcher :: Maybe Matcher
    , currentModule :: Maybe ModuleName
    , completionOptions :: CompletionOptions
    }
  | Type
    { search :: String
    , filters :: Array Filter
    , currentModule :: Maybe ModuleName
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
    { file :: FileName
    , actualFile :: Maybe FileName
    , codegen :: Array CodegenTarget
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

data ImportCommand
  = AddImplicitImport { module :: ModuleName }
  | AddQualifiedImport { module :: ModuleName, qualifier :: String }
  | AddImport { identifier :: String, qualifier :: Maybe String }

type Position
  = { line :: Int, column :: Int }

type Range
  = { name :: FileName
    , start :: Position
    , end :: Position
    }

type Completion
  = { type :: String
    , identifier :: String
    , module :: String
    , definedAt :: Maybe Range
    , expandedType :: Maybe String
    , documentation :: Maybe String
    , exportedFrom :: Array String
    }

type ImportList
  = { moduleName :: ModuleName, imports :: Array Import }

type Import
  = { module :: ModuleName
    , importType :: ImportType
    , qualifier :: Maybe String
    }

data ImportType
  = Implicit
  | Explicit (Array String)
  | Hiding (Array String)

type RangePosition
  = { startLine :: Int
    , startColumn :: Int
    , endLine :: Int
    , endColumn :: Int
    }

type RebuildError
  = { position :: Maybe RangePosition
    , moduleName :: Maybe ModuleName
    , filename :: Maybe FileName
    , errorCode :: String
    , message :: String
    , errorLink :: String
    , pursIde :: Maybe HoleFits
    , suggestion :: Maybe Suggestion
    }

type Suggestion
  = { replacement :: String
    , replaceRange :: Maybe RangePosition
    }

type HoleFits
  = { name :: String
    , completions :: Array Completion
    }

data ImportResult
  = SuccessFile String
  | SuccessText (Array String)
  | MultipleResults (Array Completion)
