module Main where

import Debug.Trace (traceAny)
import Control.Coroutine
import Control.Bind ((=<<))
import Control.Coroutine.Aff.Seq
import Control.Coroutine.Aff
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut ((.?))
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Functor (($>))
import Data.Maybe
import Data.Either (Either, either)
import Network.HTTP.Affjax
import Data.Generic (class Generic, gShow)
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

changeNotificationUri :: Int -> String
changeNotificationUri i = "http://127.0.0.1:5984/preserves/_changes?feed=longpoll&since=" ++ show i

getChangeNotification :: forall eff. Int -> Aff (console :: CONSOLE, ajax :: AJAX | eff) ChangeNotification
getChangeNotification i =
  (decode <<< _.response) =<< get (changeNotificationUri i)
  where
  decode = (either (throwError <<< error) pure) <<< decodeJson

pluckSeq :: ChangeNotification -> Int
pluckSeq (ChangeNotification obj) = obj.lastSeq

consumeLog :: forall a. (Show a) => Consumer a (Aff (console :: CONSOLE | _)) Error
consumeLog = consumer \s -> liftEff (log $ show s) $> Nothing

-- i should have got an error with bad url
main :: _
main = launchAff $ runProcess (produceSeq getChangeNotification pluckSeq 0 $$ consumeLog)
