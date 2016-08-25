module Control.Monad.Aff.CouchDB.ChangeNotification where

import Control.Bind ((=<<))
import Control.Coroutine (Producer, transform, ($~))
import Control.Coroutine.Aff.Seq (produceSeq)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Control.Parallel.Class (class MonadPar)
import Data.Argonaut ((.?))
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Array (head, catMaybes)
import Data.Either (either)
import Data.Functor (($>))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Network.HTTP.Affjax (AJAX, get)
import Prelude

newtype ChangeNotification =
  ChangeNotification
    { results :: Array Result
    , lastSeq :: Int
    }

newtype Result =
  Result
    { seq :: Int
    , id :: String
    , changes :: Array Change
    , deleted :: Maybe Boolean
    }

type ChangeNotificationR =
  { results :: Array Result
  , lastSeq :: Int
  }

type ResultR =
  { seq :: Int
  , id :: String
  , changes :: Array Change
  , deleted :: Maybe Boolean
  }

type ChangeR = { rev :: String }

newtype Change =
  Change { rev :: String }

instance decodeChangeNotification :: DecodeJson ChangeNotification where
  decodeJson json = do
    obj <- decodeJson json
    results <- obj .? "results"
    lastSeq <- obj .? "last_seq"
    pure $ ChangeNotification { results, lastSeq }

instance decodeResult :: DecodeJson Result where
  decodeJson json = do
    obj <- decodeJson json
    seq <- obj .? "seq"
    id <- obj .? "id"
    changes <- obj .? "changes"
    let deleted = eitherToMaybe $ obj .? "deleted"
    pure $ Result { seq, id, changes, deleted }
    where
    eitherToMaybe = either (const Nothing) Just

instance decodeChange :: DecodeJson Change where
  decodeJson json = do
    obj <- decodeJson json
    rev <- obj .? "rev"
    pure $ Change { rev }

derive instance genericChange :: Generic Change
derive instance genericResult :: Generic Result
derive instance genericChangeNotification :: Generic ChangeNotification

instance showChangeNotification :: Show ChangeNotification where
  show = gShow

instance showResult :: Show Result where
  show = gShow

instance showChange :: Show Change where
  show = gShow

run :: ChangeNotification -> ChangeNotificationR
run (ChangeNotification obj) = obj

runResult :: Result -> ResultR
runResult (Result obj) = obj

runChange :: Change -> ChangeR
runChange (Change obj) = obj

docUri :: String -> String -> String -> String
docUri dbUri docId docRev = dbUri <> "/" <> docId <> "?rev=" <> docRev

resultToDocUri :: String -> Result -> Maybe String
resultToDocUri dbUri (Result obj) = (docUri dbUri obj.id <<< _.rev <<< runChange) <$> head obj.changes

toDocUris :: String -> ChangeNotification -> Array String
toDocUris dbUri (ChangeNotification obj) = catMaybes $ resultToDocUri dbUri <$> obj.results

toDocs :: forall a eff. (DecodeJson a) => String -> ChangeNotification -> Aff (ajax :: AJAX | eff) (Array a)
toDocs dbUri changeNotification = sequence $ getDecodeJson <$> toDocUris dbUri changeNotification

pluckSeq :: ChangeNotification -> Int
pluckSeq (ChangeNotification obj) = obj.lastSeq

changeNotificationUri :: String -> Int -> String
changeNotificationUri dbUri i = dbUri <> "/_changes?feed=longpoll&since=" <> show i

getDecodeJson :: forall a eff. (DecodeJson a) => String -> Aff (ajax :: AJAX | eff) a
getDecodeJson uri = (decode <<< _.response) =<< get uri
  where
  decode = (either (throwError <<< error) pure) <<< decodeJson

getChangeNotification :: forall eff. String -> Int -> Aff (ajax :: AJAX | eff) ChangeNotification
getChangeNotification dbUri = getDecodeJson <<< changeNotificationUri dbUri

toChangeNotificationWithDocs :: forall a eff. (DecodeJson a) => String -> ChangeNotification -> Aff (ajax :: AJAX | eff) (Tuple ChangeNotification (Array a))
toChangeNotificationWithDocs dbUri changeNotification = Tuple changeNotification <$> toDocs dbUri changeNotification

getChangeNotificationWithDocs :: forall a eff. (DecodeJson a) => String -> Int -> Aff (ajax :: AJAX | eff) (Tuple ChangeNotification (Array a))
getChangeNotificationWithDocs dbUri = toChangeNotificationWithDocs dbUri <=< getChangeNotification dbUri

produceChangeNotifications :: forall m eff. (Affable (avar :: AVAR, ajax :: AJAX | eff) m, Functor m) => String -> Int -> Producer ChangeNotification m Unit
produceChangeNotifications dbUri = produceSeq (getChangeNotification dbUri) (pluckSeq)

produceChangedDocs :: forall a m eff. (DecodeJson a, Affable (avar :: AVAR | eff) m, Functor m, MonadRec m, MonadPar m) => String -> Int -> Producer (Array a) m Unit
produceChangedDocs dbUri seq = produceSeq (fromAff <<< getChangeNotificationWithDocs dbUri) (pluckSeq <<< fst) seq $~ forever (transform snd)
