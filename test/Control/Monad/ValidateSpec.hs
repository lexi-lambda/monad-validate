{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.ValidateSpec (spec) where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Reader
import Data.Aeson (Object, Value(..))
import Data.Aeson.QQ (aesonQQ)
import Data.Foldable
import Data.Functor
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Typeable
import Test.Hspec

import Control.Monad.Validate

data AuthToken = AuthToken { atBearer :: Text, atIsAdmin :: Bool }
  deriving (Show, Eq)

data TableName = TableName { tnSchema :: Text, tnName :: Text }
  deriving (Show, Eq)
newtype ColumnName = ColumnName { cnName :: Text }
  deriving (Show, Eq)

type TableInfo = [(ColumnName, ColumnInfo)]
newtype ColumnInfo = ColumnInfo { ciAdminOnly :: Bool }
  deriving (Show, Eq)

data Env = Env
  { envTables :: [(TableName, TableInfo)]
  , envPath :: [Text] }
  deriving (Show, Eq)

data Query a where
  QLit :: Integer -> Query Integer
  QSelect :: ColumnName -> Query Integer
  QAdd :: Query Integer -> Query Integer -> Query Integer
  QEqual :: Query Integer -> Query Integer -> Query Bool
  QIf :: Query Bool -> Query a -> Query a -> Query a
deriving instance Show (Query a)
deriving instance Eq (Query a)

data QueryRequest = QueryRequest
  { qrAuth :: AuthToken
  , qrTable :: TableName
  , qrQuery :: Query Integer }
  deriving (Show, Eq)

data Error = Error { errPath :: [Text], errInfo :: ErrorInfo }
  deriving (Show, Eq)
data ErrorInfo
  = JSONBadValue Text Value
  | JSONMissingKey Text
  | InvalidAuthToken Text
  | UnknownTableName TableName
  | UnknownQueryOperator Text
  | TypeError TypeRep TypeRep
  | UnknownColumnName TableName ColumnName
  | InsufficientPermissions TableName ColumnName
  deriving (Show, Eq)

validateQueryRequest :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> m QueryRequest
validateQueryRequest req = withObject "request" req $ \o -> do
  qrAuth           <- withKey o "auth_token" parseAuthToken
  ~(qrTable, info) <- withKey o "table" parseTableName
  qrQuery          <- withKey o "query" parseQuery
  for_ info $ \tableInfo -> pushPath "query" $
    validateQuery qrTable tableInfo (atIsAdmin qrAuth) qrQuery
  pure QueryRequest { qrAuth, qrTable, qrQuery }
  where
    parseAuthToken v = do
      str <- asString v
      case T.splitOn ":" str of
        [bearer]                                -> pure $ AuthToken bearer False
        [bearer, "super_secret_admin_password"] -> pure $ AuthToken bearer True
        _                                       -> refuteErr $ InvalidAuthToken str

    parseTableName v = withObject "table name" v $ \o -> do
      name <- TableName <$> withKey o "schema" asString <*> withKey o "name" asString
      info <- tolerate $ validateTableName name
      pure (name, info)

    validateTableName name = do
      info <- lookup name <$> asks envTables
      maybe (refuteErr $ UnknownTableName name) pure info

    parseQuery :: forall a. (Typeable a) => Value -> m (Query a)
    parseQuery q = withSingleKeyObject "query expression" q $ \k v -> case k of
      "lit"    -> withType $ QLit <$> asInteger v
      "select" -> withType $ QSelect <$> parseColumnName v
      "add"    -> withType $ asPair v >>= \(a, b) -> QAdd <$> parseQuery a <*> parseQuery b
      "equal"  -> withType $ asPair v >>= \(a, b) -> QEqual <$> parseQuery a <*> parseQuery b
      "if"     -> withType @a $ asTriple v >>= \(a, b, c) ->
                    QIf <$> parseQuery a <*> parseQuery b <*> parseQuery c
      _        -> refuteErr $ UnknownQueryOperator k

    validateQuery tableName tableInfo isAdmin = loop where
      loop :: Query a -> m ()
      loop = \case
        QLit _ -> pure ()
        QSelect colName -> pushPath "select" $ case lookup colName tableInfo of
          Just colInfo
            | ciAdminOnly colInfo && not isAdmin
            -> disputeErr $ InsufficientPermissions tableName colName
            | otherwise -> pure ()
          Nothing -> disputeErr $ UnknownColumnName tableName colName
        QAdd a b -> pushPath "add" $ loop a *> loop b
        QEqual a b -> pushPath "equal" $ loop a *> loop b
        QIf a b c -> pushPath "if" $ loop a *> loop b *> loop c

    parseColumnName = fmap ColumnName . asString

    pushPath :: Text -> m a -> m a
    pushPath path = local (\env -> env { envPath = path : envPath env })
    mkErr info = asks envPath <&> \path -> Error (reverse path) info
    refuteErr = mkErr >=> \err -> refute [err]
    disputeErr :: ErrorInfo -> m ()
    disputeErr = mkErr >=> \err -> dispute [err]

    withType :: forall a b. (Typeable a, Typeable b) => m (Query a) -> m (Query b)
    withType m = case eqT @a @b of
      Just Refl -> m
      Nothing -> refuteErr $ TypeError (typeRep (Proxy @a)) (typeRep (Proxy @b))

    asString = \case { String s -> pure s; v -> refuteErr $ JSONBadValue "string" v }
    asNumber = \case { Number n -> pure n; v -> refuteErr $ JSONBadValue "number" v }
    asInteger v = asNumber v >>=
      maybe (refuteErr $ JSONBadValue "integer" v) (pure . toInteger) . toBoundedInteger @Int
    asArray = \case { Array v -> pure $ V.toList v; v -> refuteErr $ JSONBadValue "array" v }
    asPair v = asArray v >>= \case { [a, b] -> pure (a, b); _ -> refuteErr $ JSONBadValue "pair" v }
    asTriple v = asArray v >>= \case { [a, b, c] -> pure (a, b, c); _ -> refuteErr $ JSONBadValue "triple" v }

    withObject :: Text -> Value -> (Object -> m a) -> m a
    withObject name v f = case v of { Object o -> f o; _ -> refuteErr $ JSONBadValue name v }

    withKey :: Object -> Text -> (Value -> m a) -> m a
    withKey o k f = maybe (refuteErr $ JSONMissingKey k) (pushPath k . f) $ M.lookup k o

    withSingleKeyObject :: Text -> Value -> (Text -> Value -> m a) -> m a
    withSingleKeyObject name i f = withObject name i $ \o -> case M.toList o of
      { [(k, v)] -> pushPath k $ f k v; _ -> refuteErr $ JSONBadValue name i }

spec :: Spec
spec = describe "ValidateT" $ do
  describe "tolerate" $ do
    it "has no effect on computations without fatal errors" $ do
      runValidate ((dispute ["boom"] $> ["bang"]) >>= dispute)
        `shouldBe` Left (["boom", "bang"] :: [Text])
      runValidate (tolerate (dispute ["boom"] $> ["bang"]) >>= traverse_ dispute)
        `shouldBe` Left (["boom", "bang"] :: [Text])

    it "converts fatal errors to non-fatal errors" $ do
      runValidate (refute ["boom"] >> dispute ["bang"])
        `shouldBe` Left (["boom"] :: [Text])
      runValidate (tolerate (refute ["boom"]) >> dispute ["bang"])
        `shouldBe` Left (["boom", "bang"] :: [Text])

  describe "mapErrors" $ do
    it "applies a function to all validation errors" $
      runValidate (mapErrors (map show) (refute [True] *> dispute [False]))
        `shouldBe` Left ["True", "False"]

    it "can be used with embedValidateT to locally change the type of errors" $ do
      let foo :: (MonadValidate [Integer] m) => m ()
          foo = dispute [42]
          bar :: (MonadValidate [Bool] m) => m ()
          bar = dispute [False]
          baz :: (MonadValidate [Either Integer Bool] m) => m ()
          baz = do
            embedValidateT $ mapErrors (map Left) foo
            embedValidateT $ mapErrors (map Right) bar
      runValidate baz `shouldBe` Left [Left 42, Right False]

  it "collects validation information from all sub-branches of <*>" $ do
    let tables =
          [ (TableName "public" "users",
            [ (ColumnName "name", ColumnInfo False)
            , (ColumnName "password", ColumnInfo True)
            , (ColumnName "points", ColumnInfo False) ])
          , (TableName "private" "tables",
            [ (ColumnName "id", ColumnInfo False)
            , (ColumnName "schema", ColumnInfo False) ]) ]
        env = Env tables []
        testCase input = runReader (runValidateT (validateQueryRequest input)) env

    testCase [aesonQQ| {} |] `shouldBe` Left
      [ Error [] $ JSONMissingKey "auth_token"
      , Error [] $ JSONMissingKey "table"
      , Error [] $ JSONMissingKey "query" ]

    testCase [aesonQQ| { "auth_token": null, "table": null, "query": null } |] `shouldBe` Left
      [ Error ["auth_token"] $ JSONBadValue "string" Null
      , Error ["table"] $ JSONBadValue "table name" Null
      , Error ["query"] $ JSONBadValue "query expression" Null ]

    testCase [aesonQQ|
      { "auth_token": "abc123"
      , "table": { "schema": "public", "name": "people" }
      , "query": { "lit": "42" }
      } |] `shouldBe` Left
      [ Error ["table"] $ UnknownTableName (TableName "public" "people")
      , Error ["query", "lit"] $ JSONBadValue "number" (String "42") ]

    testCase [aesonQQ|
      { "auth_token": "abc123"
      , "table": { "schema": "public", "name": "users" }
      , "query": { "lit": 42 }
      } |] `shouldBe` Right QueryRequest
      { qrAuth = AuthToken "abc123" False
      , qrTable = TableName "public" "users"
      , qrQuery = QLit 42 }

    testCase [aesonQQ|
      { "auth_token": "abc123"
      , "table": { "schema": "public", "name": "users" }
      , "query": { "add":
        [ { "select": "password" }
        , { "select": "email" } ]}
      } |] `shouldBe` Left
      [ Error ["query", "add", "select"] $ InsufficientPermissions
          (TableName "public" "users")
          (ColumnName "password")
      , Error ["query", "add", "select"] $ UnknownColumnName
          (TableName "public" "users")
          (ColumnName "email") ]

    testCase [aesonQQ|
      { "auth_token": "abc123:super_secret_admin_password"
      , "table": { "schema": "public", "name": "users" }
      , "query": { "add":
        [ { "select": "name" }
        , { "select": "password" } ]}
      } |] `shouldBe` Right QueryRequest
      { qrAuth = AuthToken "abc123" True
      , qrTable = TableName "public" "users"
      , qrQuery = QAdd (QSelect (ColumnName "name")) (QSelect (ColumnName "password")) }

    testCase [aesonQQ|
      { "auth_token": 123
      , "table": { "name": "users" }
      , "query": { "add":
        [ { "lit": "42" }
        , { "select": "points" } ]}
      } |] `shouldBe` Left
      [ Error ["auth_token"] (JSONBadValue "string" (Number 123))
      , Error ["table"] (JSONMissingKey "schema")
      , Error ["query", "add", "lit"] (JSONBadValue "number" (String "42")) ]
