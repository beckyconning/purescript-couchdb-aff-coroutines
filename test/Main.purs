module Test.Main where

import Control.Coroutine (Consumer, Producer,  runProcess, consumer, ($$))
import Control.Bind ((=<<))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Control.Monad.Aff.CouchDB.ChangeNotification (produceChangedDocs)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic (class Generic, gShow)
import Data.Argonaut ((.?))
import Data.Functor (($>))
import Data.Maybe (Maybe(..))
import Data.Either (either)
import Network.HTTP.Affjax (AJAX)
import Prelude

newtype Preserve =
  Preserve
    { _id :: String, _rev :: String, _deleted :: Maybe Boolean, fibre :: Number, name :: String }

derive instance genericPreserve :: Generic Preserve

instance decodePreserve :: DecodeJson Preserve where
  decodeJson json = do
    obj <- decodeJson json
    _id <- obj .? "_id"
    _rev <- obj .? "_rev"
    name <- obj .? "name"
    fibre <- obj .? "fibre"
    let _deleted = eitherToMaybe $ obj .? "_deleted"
    pure $ Preserve { _id, _rev, _deleted, name, fibre }
    where
    eitherToMaybe = either (const Nothing) Just

instance showPreserve :: Show Preserve where
  show = gShow

exampleDbUri :: String
exampleDbUri = "http://127.0.0.1:5984/preserves"

consumeLog :: forall a eff. (Show a) => Consumer a (Aff (console :: CONSOLE | eff)) Error
consumeLog = consumer \s -> liftEff (log $ show s) $> Nothing

produceChangedPreserves :: forall eff. Producer (Array Preserve) (Aff (avar :: AVAR, ajax :: AJAX | eff)) Error
produceChangedPreserves = produceChangedDocs exampleDbUri 0

main :: forall eff. Eff (err :: EXCEPTION, avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff) Unit
main = void $ launchAff $ runProcess $ produceChangedPreserves $$ consumeLog
