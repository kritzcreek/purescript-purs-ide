module CodecExt where

import Prelude

import Control.Monad.ST as ST
import Data.Argonaut.Core as J
import Data.Codec as C
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Foreign.Object.ST as FOST

withNullables :: Array String -> JsonCodec J.Json
withNullables fields = C.basicCodec (pure <<< dec) enc
  where
  dec :: J.Json -> J.Json
  dec j = J.caseJsonObject j (\o -> ST.run do
    obj <- FO.thawST o
    ST.foreach fields \field -> do
      FOST.peek field obj >>= case _ of
        Nothing -> 
          void (FOST.poke field J.jsonNull obj)
        Just _ ->
          pure unit
    map J.fromObject (FO.freezeST obj)
  ) j

  enc :: J.Json -> J.Json
  enc j =  J.caseJsonObject j (\o -> ST.run do
    obj <- FO.thawST o
    ST.foreach fields \field ->
      FOST.peek field obj >>= traverse_ \v ->
          when (J.isNull v) (void (FOST.delete field obj))
    map J.fromObject (FO.freezeST obj)
  ) j


flatTagged :: String -> CA.JsonCodec J.Json
flatTagged tagName = C.basicCodec dec enc
  where
  dec :: J.Json -> Either CA.JsonDecodeError J.Json
  dec = J.caseJsonObject (Left (CA.TypeMismatch "Object")) (map J.fromObject <<< rewriteDec)

  rewriteDec :: FO.Object J.Json -> Either CA.JsonDecodeError (FO.Object J.Json)
  rewriteDec obj = case FO.pop tagName obj of
    Nothing -> Left (CA.AtKey tagName CA.MissingValue)
    Just (Tuple tagValue obj') ->
      Right
        $ FO.runST do
            result ← FOST.new
            _ <- FOST.poke "tag" tagValue result
            FOST.poke "value" (J.fromObject obj') result

  enc :: J.Json -> J.Json
  enc j = J.caseJsonObject j (J.fromObject <<< rewriteEnc) j

  rewriteEnc :: FO.Object J.Json -> FO.Object J.Json
  rewriteEnc obj = case FO.lookup "tag" obj, J.toObject =<< FO.lookup "value" obj of
    Just tagValue, Just valueObj -> FO.insert tagName tagValue valueObj
    _, _ -> obj
