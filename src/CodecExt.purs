module CodecExt where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.ST as ST
import Control.Monad.Writer (Writer, writer)
import Data.Argonaut.Core as J
import Data.Bifunctor (lmap)
import Data.Codec as C
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as JA
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Foreign.Object.ST as FOST

withNullables :: Array String -> JsonCodec J.Json
withNullables fields = C.basicCodec (pure <<< dec) enc
  where
  dec :: J.Json -> J.Json
  dec j =
    J.caseJsonObject j
      ( \o ->
          ST.run do
            obj <- FO.thawST o
            ST.foreach fields \field -> do
              FOST.peek field obj
                >>= case _ of
                    Nothing -> void (FOST.poke field J.jsonNull obj)
                    Just _ -> pure unit
            map J.fromObject (FO.freezeST obj)
      )
      j

  enc :: J.Json -> J.Json
  enc j =
    J.caseJsonObject j
      ( \o ->
          ST.run do
            obj <- FO.thawST o
            ST.foreach fields \field ->
              FOST.peek field obj
                >>= traverse_ \v ->
                    when (J.isNull v) (void (FOST.delete field obj))
            map J.fromObject (FO.freezeST obj)
      )
      j

flatTagged :: String -> JA.JsonCodec J.Json
flatTagged tagName = C.basicCodec dec enc
  where
  dec :: J.Json -> Either JA.JsonDecodeError J.Json
  dec = J.caseJsonObject (Left (TypeMismatch "Object")) (map J.fromObject <<< rewriteDec)

  rewriteDec :: FO.Object J.Json -> Either JA.JsonDecodeError (FO.Object J.Json)
  rewriteDec obj = case FO.pop tagName obj of
    Nothing -> Left (AtKey tagName MissingValue)
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
