## Module Control.Monad.Aff.CouchDB.ChangeNotification

#### `ChangeNotification`

``` purescript
newtype ChangeNotification
  = ChangeNotification { results :: Array Result, lastSeq :: Int }
```

##### Instances
``` purescript
DecodeJson ChangeNotification
Generic ChangeNotification
Show ChangeNotification
```

#### `Result`

``` purescript
newtype Result
  = Result { seq :: Int, id :: String, changes :: Array Change, deleted :: Maybe Boolean }
```

##### Instances
``` purescript
DecodeJson Result
Generic Result
Show Result
```

#### `ChangeNotificationR`

``` purescript
type ChangeNotificationR = { results :: Array Result, lastSeq :: Int }
```

#### `ResultR`

``` purescript
type ResultR = { seq :: Int, id :: String, changes :: Array Change, deleted :: Maybe Boolean }
```

#### `ChangeR`

``` purescript
type ChangeR = { rev :: String }
```

#### `Change`

``` purescript
newtype Change
  = Change { rev :: String }
```

##### Instances
``` purescript
DecodeJson Change
Generic Change
Show Change
```

#### `run`

``` purescript
run :: ChangeNotification -> ChangeNotificationR
```

#### `runResult`

``` purescript
runResult :: Result -> ResultR
```

#### `runChange`

``` purescript
runChange :: Change -> ChangeR
```

#### `docUri`

``` purescript
docUri :: String -> String -> String -> String
```

#### `resultToDocUri`

``` purescript
resultToDocUri :: String -> Result -> Maybe String
```

#### `toDocUris`

``` purescript
toDocUris :: String -> ChangeNotification -> Array String
```

#### `toDocs`

``` purescript
toDocs :: forall a eff. DecodeJson a => String -> ChangeNotification -> Aff (ajax :: AJAX | eff) (Array a)
```

#### `pluckSeq`

``` purescript
pluckSeq :: ChangeNotification -> Int
```

#### `changeNotificationUri`

``` purescript
changeNotificationUri :: String -> Int -> String
```

#### `getDecodeJson`

``` purescript
getDecodeJson :: forall a eff. DecodeJson a => String -> Aff (ajax :: AJAX | eff) a
```

#### `getChangeNotification`

``` purescript
getChangeNotification :: forall a eff. DecodeJson a => String -> Int -> Aff (ajax :: AJAX | eff) a
```

#### `toChangeNotificationWithDocs`

``` purescript
toChangeNotificationWithDocs :: forall a eff. DecodeJson a => String -> ChangeNotification -> Aff (ajax :: AJAX | eff) (Tuple ChangeNotification (Array a))
```

#### `getChangeNotificationWithDocs`

``` purescript
getChangeNotificationWithDocs :: forall a eff. DecodeJson a => String -> Int -> Aff (ajax :: AJAX | eff) (Tuple ChangeNotification (Array a))
```

#### `produceChangeNotifications`

``` purescript
produceChangeNotifications :: forall eff. String -> Int -> Producer ChangeNotification (Aff (avar :: AVAR, ajax :: AJAX | eff)) Error
```

#### `produceChangedDocs`

``` purescript
produceChangedDocs :: forall a eff. DecodeJson a => String -> Int -> Producer (Array a) (Aff (avar :: AVAR, ajax :: AJAX | eff)) Error
```


