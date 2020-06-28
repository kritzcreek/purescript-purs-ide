module Main where

import Prelude

import Data.Argonaut.Core as J
import Data.Codec as C
import Data.Maybe (Maybe(..))
import Debug.Trace as Debug
import Effect (Effect)
import PursIde (ImportCommand(..))
import PursIde.Codec (importCommandCodec)
import Unsafe.Coerce (unsafeCoerce)

importCommand :: J.Json
importCommand =
  unsafeCoerce
    { module: "Data.List"
    , qualifier: "List"
    , importCommand: "addQualifiedImport"
    }

importCommand2 :: J.Json
importCommand2 = unsafeCoerce { identifier: "filter", importCommand: "addImport" }

main :: Effect Unit
main = do
  Debug.traceM (C.encode importCommandCodec (AddQualifiedImport { module: "Data.List", qualifier: "List" }))
  Debug.traceM (C.encode importCommandCodec (AddImport { identifier: "filter", qualifier: Nothing}))
  Debug.traceM (C.decode importCommandCodec importCommand)
  Debug.traceM (C.decode importCommandCodec importCommand2)
