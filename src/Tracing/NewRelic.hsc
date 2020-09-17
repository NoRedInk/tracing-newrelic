{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
-- Our default `Strict` policy will cause runtime errors for this module. See:
-- https://gitlab.haskell.org/ghc/ghc/issues/16810

-- |
-- The Tracing.NewRelic module allows reporting to NewRelic by binding to its
-- [C SDK](https://newrelic.github.io/c-sdk/libnewrelic_8h.html).
--
-- The functions provided here match the ones provided by the
-- [C SDK](https://newrelic.github.io/c-sdk/libnewrelic_8h.html).
-- Their docs have been copied over, but if in doubt you can refer to
-- the original documentation.
--
-- = Example usage
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Tracing.NewRelic
-- >
-- > main :: IO ()
-- > main = do
-- >   appConfig <- createAppConfig (AppName "My app") (LicenseKey "*****")
-- >   app <- createApp appConfig (TimeoutMs 10000)
-- >
-- >   tx <- startWebTransaction app "Here comes a request"
-- >   segment <- startSegment tx (Just "Some name") (Just "Some category")
-- >
-- >   -- Some expensive computation...
-- >
-- >   _ <- endSegment segment
-- >   _ <- endTransaction tx
--
-- In a real application transactions and segments should be probably ended
-- as part of a "try-finally" block, for example using
-- [Conduit.Acquire](https://www.stackage.org/haddock/lts-14.13/conduit-1.3.1.1/Conduit.html#t:Acquire).
--
-- It's very important that each @start*@ has a matching @end*@ to avoid memory leaks.
--
-- Also note that `createApp` and `createAppConfig` can
-- [throwIO](https://www.stackage.org/haddock/lts-14.13/base-4.12.0.0/Control-Exception.html#v:throwIO)
-- and this should be handled somehow.
module Tracing.NewRelic
  ( init,
    configureLog,
    createAppConfig,
    setDistributedTracingEnabled,
    setDatastoreTracerInstanceReportingEnabled,
    setDatastoreTracerDatabaseNameReportingEnabled,
    setSpanEventsEnabled,
    setTransactionTracerEnabled,
    setTransactionTracerDatastoreReportingSlowQueriesEnabled,
    setTransactionTracerRecordSql,
    createApp,
    startWebTransaction,
    startNonWebTransaction,
    ignoreTransaction,
    setTransactionTiming,
    endTransaction,
    startSegment,
    startExternalSegment,
    startDatastoreSegment,
    setSegmentParentRoot,
    setSegmentParent,
    setSegmentTiming,
    endSegment,
    addAttributeInt,
    addAttributeLong,
    addAttributeDouble,
    addAttributeText,
    recordCustomMetric,
    noticeError,
    createCustomEvent,
    recordCustomEvent,
    discardCustomEvent,
    customEventAddAttributeInt,
    customEventAddAttributeLong,
    customEventAddAttributeDouble,
    customEventAddAttributeText,
    createRootDistributedTracePayload,
    createDistributedTracePayload,
    createRootDistributedTracePayloadHttpSafe,
    createDistributedTracePayloadHttpSafe,
    acceptDistributedTracePayload,
    acceptDistributedTracePayloadHttpSafe,
    version,
    App,
    AppConfig,
    Transaction,
    Segment,
    LogLevel(..),
    ExternalSegment (..),
    DatastoreSegment (..),
    DatastoreSegmentProduct (..),
    LicenseKey(..),
    AppName(..),
    TimeoutMs(..),
    NewRelicException(..),
    TransactionTracerRecordSql(..),
    DaemonSocket(..),
    TimeLimitMs(..),
    StartTimeUsSinceUnixEpoch(..),
    DurationUs(..)
    )
where

import Data.Int (Int, Int16, Int32, Int64)
import Data.Word (Word64)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as ByteString
import Foreign.C.String
import Foreign.C.Types
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (init)
import Control.Exception (Exception, throwIO)
import Data.Text (Text)

#include "libnewrelic.h"

foreign import ccall "newrelic_init" newrelic_init :: CString -> CInt -> IO Bool

foreign import ccall "newrelic_configure_log" newrelic_configure_log :: CString -> CInt -> IO Bool

foreign import ccall "newrelic_create_app_config" newrelic_create_app_config :: CString -> CString -> IO (Ptr AppConfig)

foreign import ccall "newrelic_destroy_app_config" newrelic_destroy_app_config :: Ptr (Ptr AppConfig) -> IO ()

foreign import ccall "newrelic_create_app" newrelic_create_app :: Ptr AppConfig -> CShort -> IO (Ptr App)

foreign import ccall "newrelic_destroy_app" newrelic_destroy_app :: Ptr (Ptr App) -> IO ()

foreign import ccall "newrelic_start_web_transaction" newrelic_start_web_transaction :: Ptr App -> CString -> IO (Ptr Transaction)

foreign import ccall "newrelic_start_non_web_transaction" newrelic_start_non_web_transaction :: Ptr App -> CString -> IO (Ptr Transaction)

foreign import ccall "newrelic_ignore_transaction" newrelic_ignore_transaction :: Ptr Transaction -> IO Bool

foreign import ccall "newrelic_set_transaction_timing" newrelic_set_transaction_timing :: Ptr Transaction -> CULong -> CULong -> IO Bool

foreign import ccall "newrelic_end_transaction" newrelic_end_transaction :: Ptr (Ptr Transaction) -> IO Bool

foreign import ccall "newrelic_start_segment" newrelic_start_segment :: Ptr Transaction -> CString -> CString -> IO (Ptr Segment)

foreign import ccall "newrelic_start_external_segment" newrelic_start_external_segment :: Ptr Transaction -> Ptr ExternalSegment -> IO (Ptr Segment)

foreign import ccall "newrelic_start_datastore_segment" newrelic_start_datastore_segment :: Ptr Transaction -> Ptr DatastoreSegment -> IO (Ptr Segment)

foreign import ccall "newrelic_set_segment_parent_root" newrelic_set_segment_parent_root :: Ptr Segment -> IO Bool

foreign import ccall "newrelic_set_segment_parent" newrelic_set_segment_parent :: Ptr Segment -> Ptr Segment -> IO Bool

foreign import ccall "newrelic_set_segment_timing" newrelic_set_segment_timing :: Ptr Segment -> CULong -> CULong -> IO Bool

foreign import ccall "newrelic_end_segment" newrelic_end_segment :: Ptr Transaction -> Ptr (Ptr Segment) -> IO Bool

foreign import ccall "newrelic_add_attribute_int" newrelic_add_attribute_int :: Ptr Transaction -> CString -> CInt -> IO Bool

foreign import ccall "newrelic_add_attribute_long" newrelic_add_attribute_long :: Ptr Transaction -> CString -> CLong -> IO Bool

foreign import ccall "newrelic_add_attribute_double" newrelic_add_attribute_double :: Ptr Transaction -> CString -> CDouble -> IO Bool

foreign import ccall "newrelic_add_attribute_string" newrelic_add_attribute_string :: Ptr Transaction -> CString -> CString -> IO Bool

foreign import ccall "newrelic_record_custom_metric" newrelic_record_custom_metric :: Ptr Transaction -> CString -> CDouble -> IO Bool

foreign import ccall "newrelic_notice_error" newrelic_notice_error :: Ptr Transaction -> CInt -> CString -> CString -> IO ()

foreign import ccall "newrelic_create_custom_event" newrelic_create_custom_event :: CString -> IO (Ptr CustomEvent)

foreign import ccall "newrelic_record_custom_event" newrelic_record_custom_event :: Ptr Transaction -> Ptr (Ptr CustomEvent) -> IO ()

foreign import ccall "newrelic_custom_event_add_attribute_int" newrelic_custom_event_add_attribute_int :: Ptr CustomEvent -> CString -> CInt -> IO Bool

foreign import ccall "newrelic_custom_event_add_attribute_long" newrelic_custom_event_add_attribute_long :: Ptr CustomEvent -> CString -> CLong -> IO Bool

foreign import ccall "newrelic_custom_event_add_attribute_double" newrelic_custom_event_add_attribute_double :: Ptr CustomEvent -> CString -> CDouble -> IO Bool

foreign import ccall "newrelic_custom_event_add_attribute_string" newrelic_custom_event_add_attribute_string :: Ptr CustomEvent -> CString -> CString -> IO Bool

foreign import ccall "newrelic_discard_custom_event" newrelic_discard_custom_event :: Ptr (Ptr CustomEvent) -> IO ()

foreign import ccall "newrelic_create_distributed_trace_payload" newrelic_create_distributed_trace_payload :: Ptr Transaction -> Ptr Segment -> IO CString

foreign import ccall "newrelic_create_distributed_trace_payload_httpsafe" newrelic_create_distributed_trace_payload_httpsafe :: Ptr Transaction -> Ptr Segment -> IO CString

foreign import ccall "newrelic_accept_distributed_trace_payload" newrelic_accept_distributed_trace_payload :: Ptr Transaction -> CString -> CString -> IO Bool

foreign import ccall "newrelic_accept_distributed_trace_payload_httpsafe" newrelic_accept_distributed_trace_payload_httpsafe :: Ptr Transaction -> CString -> CString -> IO Bool

foreign import ccall "newrelic_version" newrelic_version :: IO CString

-- |
-- Exceptions thrown when an `App` or an `AppConfig` can't be created.
data NewRelicException =
  CantCreateNewRelicAppConfig | -- ^ Exception thrown when an `AppConfig` can't be created.
  CantCreateNewRelicApp         -- ^ Exception thrown when an `App` can't be created.
  deriving (Show)

instance Exception NewRelicException

-- |
-- A daemon socket location.
-- `Nothing` means the default @\/tmp\/.newrelic.sock@ location.
newtype DaemonSocket = DaemonSocket (Maybe Text)
  deriving (Eq, Show)

-- |
-- The amount of time, in milliseconds, that the C SDK will wait for a
-- response from the daemon before considering initialization to have failed.
-- If this is 0, then a default value will be used.
newtype TimeLimitMs = TimeLimitMs Int32
  deriving (Eq, Show)


-- |
-- An application name to create an `AppConfig`.
newtype AppName = AppName Text deriving (Eq, Show)

-- |
-- A license key.
-- You can find your license key by logging in NewRelic, clicking on your username
-- on the top-right part of the page and clicking "Account Settings". The license
-- key will be shown on the right-side pane.
newtype LicenseKey = LicenseKey Text

-- |
-- A timeout value in milliseconds.
newtype TimeoutMs = TimeoutMs Int16 deriving (Eq, Show)

-- |
-- Application configuration used to create an `App`.
newtype AppConfig = AppConfig (ForeignPtr AppConfig) deriving (Eq, Show)

-- |
-- Transaction tracer record SQL options specified in an `AppConfig`.
data TransactionTracerRecordSql =
  TransactionTracerRecordSqlOff |       -- ^ No queries are reported to New Relic.
  TransactionTracerRecordSqlRaw |       -- ^ Query parameters are reported as-is, for SQL-like datastores
                                        --   that are supported by the C SDK.
  TransactionTracerRecordSqlObfuscated  -- ^ Query parameters are reported to NewRelic with alphanumeric
                                        --   characters set to "?", for SQL-like datastores that are
                                        --   supported by the C SDK.
  deriving (Show, Enum)

-- |
-- A NewRelic application.
newtype App = App (ForeignPtr App) deriving (Eq, Show)

-- |
-- A NewRelic [Transaction](https://docs.newrelic.com/docs/apm/transactions/intro-transactions/transactions-new-relic-apm)
newtype Transaction = Transaction (Ptr Transaction) deriving (Eq, Show)

-- |
-- A NewRelic [Segment](https://docs.newrelic.com/docs/apm/transactions/intro-transactions/transactions-new-relic-apm#segments)
data Segment = Segment Transaction (Ptr Segment) deriving (Eq, Show)

-- |
-- Log levels.
--
-- An enumeration of the possible log levels for an SDK configuration, or `AppConfig`.
-- The highest priority loglevel is `Error`. The level `Debug` offers the greatest verbosity.
data LogLevel = Error   | -- ^ The highest-priority loglevel; only errors are logged.
                Warning | -- ^ The loglevel for warnings and errors.
                Info    | -- ^ The loglevel for informational logs, warnings, and errors.
                Debug     -- ^ The highest-verbosity loglevel.
                deriving (Enum)

-- |
-- NewRelic [custom events](https://docs.newrelic.com/docs/using-new-relic/data/understand-data/new-relic-data-types#event-data)
newtype CustomEvent = CustomEvent (Ptr CustomEvent) deriving (Eq, Show)

-- |
-- The function `acceptDistributedTracePayload` accepts a transport type
-- which represents the type of connection used to transmit the trace payload.
-- These transport names are used across New Relic agents and are used by the UI to group traces.
data TransportType = Unknown | HTTP | HTTPS | Kafka | JMS | IronMQ | AMQP | Queue |
                     OtherTransportType Text

instance Show TransportType where
  show transportType =
    case transportType of
      Unknown -> (#const_str NEWRELIC_TRANSPORT_TYPE_UNKNOWN)
      HTTP -> (#const_str NEWRELIC_TRANSPORT_TYPE_HTTP)
      HTTPS -> (#const_str NEWRELIC_TRANSPORT_TYPE_HTTPS)
      Kafka -> (#const_str NEWRELIC_TRANSPORT_TYPE_KAFKA)
      JMS -> (#const_str NEWRELIC_TRANSPORT_TYPE_JMS)
      IronMQ -> (#const_str NEWRELIC_TRANSPORT_TYPE_IRONMQ)
      AMQP -> (#const_str NEWRELIC_TRANSPORT_TYPE_AMQP)
      Queue -> (#const_str NEWRELIC_TRANSPORT_TYPE_QUEUE)
      OtherTransportType text -> Text.unpack text

-- |
-- Segment configuration used to instrument external calls.
data ExternalSegment
  = ExternalSegment
      { externalSegmentUri :: Text,
          -- ^ The URI that was loaded.
          --
          -- This field is required to be text containing a valid URI.
        externalSegmentProcedure :: Maybe Text,
          -- ^
          -- The procedure used to load the external resource.
          --
          -- In HTTP contexts, this will usually be the request method
          -- (eg @GET@, @POST@, et al). For non-HTTP requests, or protocols that
          -- encode more specific semantics on top of HTTP like SOAP, you may
          -- wish to use a different value that more precisely encodes how
          -- the resource was requested.
          --
          -- If provided, this field is required to be text that does not include
          -- any slash characters.
        externalSegmentLibrary :: Maybe Text
          -- ^ The library used to load the external resource.
          --
          -- If provided, this field is required to be text that does
          -- not include any slash characters.
        }
  deriving (Show)

instance Storable ExternalSegment where

  alignment _ = (#alignment newrelic_external_segment_params_t)

  sizeOf _ = (#size newrelic_external_segment_params_t)

  peek ptr = do
    ExternalSegment
      <$> (peekTextOff ptr (#offset newrelic_external_segment_params_t, uri))
      <*> (peekMaybeTextOff ptr (#offset newrelic_external_segment_params_t, procedure))
      <*> (peekMaybeTextOff ptr (#offset newrelic_external_segment_params_t, library))

  poke ptr (ExternalSegment uri procedure library) = do
    pokeTextOff ptr (#offset newrelic_external_segment_params_t, uri) uri
    pokeMaybeTextOff ptr (#offset newrelic_external_segment_params_t, procedure) procedure
    pokeMaybeTextOff ptr (#offset newrelic_external_segment_params_t, library) library

-- |
-- The `datastoreSegmentProduct` field of the `DatastoreSegment` specifies the
-- datastore type of a datastore segment. For example, `MySQL` indicates that
-- the segment's query is against a MySQL database.
--
-- For the SQL-like datastores which are supported by the C SDK, when the
-- `TransactionTracerRecordSql` is set to `TransactionTracerRecordSqlRaw` or
-- `TransactionTracerRecordSqlObfuscated` using `setTransactionTracerRecordSql`,
-- the query param of the `DatastoreSegment` is reported to New Relic.
data DatastoreSegmentProduct
  = Firebird
  | Informix
  | MSSQL
  | MySQL
  | Oracle
  | Postgres
  | SQLite
  | Sybase
  | Memcached
  | MongoDB
  | ODBC
  | Redis
  | OtherDatastoreSegmentProduct Text

instance Show DatastoreSegmentProduct where
  show datastoreSegmentProduct =
    case datastoreSegmentProduct of
      Firebird -> (#const_str NEWRELIC_DATASTORE_FIREBIRD)
      Informix -> (#const_str NEWRELIC_DATASTORE_INFORMIX)
      MSSQL -> (#const_str NEWRELIC_DATASTORE_MSSQL)
      MySQL -> (#const_str NEWRELIC_DATASTORE_MYSQL)
      Oracle -> (#const_str NEWRELIC_DATASTORE_ORACLE)
      Postgres -> (#const_str NEWRELIC_DATASTORE_POSTGRES)
      SQLite -> (#const_str NEWRELIC_DATASTORE_SQLITE)
      Sybase -> (#const_str NEWRELIC_DATASTORE_SYBASE)
      Memcached -> (#const_str NEWRELIC_DATASTORE_MEMCACHE)
      MongoDB -> (#const_str NEWRELIC_DATASTORE_MONGODB)
      ODBC -> (#const_str NEWRELIC_DATASTORE_ODBC)
      Redis -> (#const_str NEWRELIC_DATASTORE_REDIS)
      OtherDatastoreSegmentProduct text -> Text.unpack text

-- |
-- Segment configuration used to instrument calls to databases and object stores.
data DatastoreSegment
  = DatastoreSegment
      { datastoreSegmentProduct :: DatastoreSegmentProduct,
          -- ^
          -- Specifies the datastore type, e.g., `MySQL`, to indicate that the segment
          -- represents a query against a MySQL database.
          --
          -- For the SQL-like datastores which are supported by the C SDK, when the
          -- `TransactionTracerRecordSql` is set to `TransactionTracerRecordSqlRaw` or
          -- `TransactionTracerRecordSqlObfuscated` using `setTransactionTracerRecordSql`,
          -- the query param of the `DatastoreSegment` is reported to New Relic.
          --
          -- This field is required to be a non-empty text that does not include any slash characters.
          -- Empty texts are replaced with @`OtherDatastoreSegmentProduct` \"Other\"@.
        datastoreSegmentCollection :: Maybe Text,
          -- ^
          -- Optional. Specifies the table or collection being used or queried against.
          --
          -- If provided, this field is required to be text that does not include any slash characters.
          -- It is also valid to use `Nothing`, in which case the default text of "other" will be attached
          -- to the datastore segment.
        datastoreSegmentOperation :: Maybe Text,
          -- ^
          -- Optional. Specifies the operation being performed: for example, "select" for an SQL SELECT
          -- query, or "set" for a Memcached set operation. While operations may be specified with any
          -- case, New Relic suggests using lowercase.
          --
          -- If provided, this field is required to be text that does not include any slash characters.
          -- It is also valid to use `Nothing`, in which case the default text of "other" will be attached
          -- to the datastore segment.
        datastoreSegmentHost :: Maybe Text,
          -- ^
          -- Optional. Specifies the datahost host name.
          --
          -- If provided, this field is required to be text that does not include any slash characters.
          -- It is also valid to use `Nothing`, in which case the default text of "other" will be attached
          -- to the datastore segment.
        datastoreSegmentPortPathOrId :: Maybe Text,
          -- ^
          -- Optional. Specifies the port or socket used to connect to the datastore.
        datastoreSegmentDatabaseName :: Maybe Text,
          -- ^
          -- Optional. Specifies the database name or number in use.
        datastoreSegmentQuery :: Maybe Text
          -- ^
          -- Optional. Specifies the database query that was sent to the server.
          --
          -- For security reasons, this value is only used if you set product to a supported SQL-like datastore,
          -- `Firebird`, `Informix`, `MSSQL`, etc. This allows the SDK to correctly obfuscate the query.
          -- When the product is set otherwise, no query information is reported to New Relic.
        }
  deriving (Show)

instance Storable DatastoreSegment where

  alignment _ = (#alignment newrelic_datastore_segment_params_t)

  sizeOf _ = (#size newrelic_datastore_segment_params_t)

  peek ptr = do
    DatastoreSegment
      <$> (peekDatastoreSegmentProductOff ptr (#offset newrelic_datastore_segment_params_t, product))
      <*> (peekMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, collection))
      <*> (peekMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, operation))
      <*> (peekMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, host))
      <*> (peekMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, port_path_or_id))
      <*> (peekMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, database_name))
      <*> (peekMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, query))
    where
      peekDatastoreSegmentProductOff :: Ptr a -> Int -> IO DatastoreSegmentProduct
      peekDatastoreSegmentProductOff ptr' offset = do
        maybeText <- peekMaybeTextOff ptr' offset
        pure $ case maybeText of
          Nothing -> OtherDatastoreSegmentProduct (#const_str NEWRELIC_DATASTORE_OTHER)
          Just (#const_str NEWRELIC_DATASTORE_FIREBIRD) -> Firebird
          Just (#const_str NEWRELIC_DATASTORE_INFORMIX) -> Informix
          Just (#const_str NEWRELIC_DATASTORE_MSSQL) -> MSSQL
          Just (#const_str NEWRELIC_DATASTORE_MYSQL) -> MySQL
          Just (#const_str NEWRELIC_DATASTORE_ORACLE) -> Oracle
          Just (#const_str NEWRELIC_DATASTORE_POSTGRES) -> Postgres
          Just (#const_str NEWRELIC_DATASTORE_SQLITE) -> SQLite
          Just (#const_str NEWRELIC_DATASTORE_SYBASE) -> Sybase
          Just (#const_str NEWRELIC_DATASTORE_MEMCACHE) -> Memcached
          Just (#const_str NEWRELIC_DATASTORE_MONGODB) -> MongoDB
          Just (#const_str NEWRELIC_DATASTORE_ODBC) -> ODBC
          Just (#const_str NEWRELIC_DATASTORE_REDIS) -> Redis
          Just text -> OtherDatastoreSegmentProduct text

  poke ptr (DatastoreSegment product' collection operation host portPathOrId databaseName query) = do
    pokeDatastoreSegmentProductOff ptr (#offset newrelic_datastore_segment_params_t, product) product'
    pokeMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, collection) collection
    pokeMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, operation) operation
    pokeMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, host) host
    pokeMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, port_path_or_id) portPathOrId
    pokeMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, database_name) databaseName
    pokeMaybeTextOff ptr (#offset newrelic_datastore_segment_params_t, query) query
    where
      pokeDatastoreSegmentProductOff :: Ptr a -> Int -> DatastoreSegmentProduct -> IO ()
      pokeDatastoreSegmentProductOff ptr' offset datastoreSegmentProduct = do
        pokeTextOff ptr' offset $ Text.pack (show datastoreSegmentProduct)


-- |
-- A start time in microseconds since the UNIX epoch.
newtype StartTimeUsSinceUnixEpoch = StartTimeUsSinceUnixEpoch Word64
                    deriving (Eq, Show)

-- |
-- A duration in microseconds.
newtype DurationUs = DurationUs Word64
                    deriving (Eq, Show)

-- |
-- Initialise the C SDK with non-default settings.
--
-- Generally, this function only needs to be called explicitly if the daemon
-- socket location needs to be customised. By default, @\/tmp\/.newrelic.sock@
-- is used, which matches the default socket location used by newrelic-daemon
-- if one isn't given.
--
-- The daemon socket location can be specified in four different ways:
--
-- * To use a specified file as a UNIX domain socket (UDS), provide an absolute path name as a string.
-- * To use a standard TCP port, specify a number in the range 1 to 65534.
-- * To use an abstract socket, prefix the socket name with @\@@.
-- * To connect to a daemon that is running on a different host, set this value to @\<host\>:\<port\>@, where @\<host\>@ denotes either a host name or an IP address, and @\<port\>@ denotes a valid port number. Both IPv4 and IPv6 are supported.
--
-- If an explicit call to this function is required, it must occur before the first call to `createApp`.
--
-- Subsequent calls to this function after a successful call to `init` or `createApp` will fail.
init :: DaemonSocket
        -- ^
        -- The path to the daemon socket.
        -- If this is `Nothing`, then the default will be used, which is to look for a UNIX
        -- domain socket at @\/tmp\/.newrelic.sock@.
        -> TimeLimitMs
        -- ^
        -- The amount of time, in milliseconds, that the C SDK will wait for a response
        -- from the daemon before considering initialization to have failed.
        -- If this is 0, then a default value will be used.
        -> IO Bool
        -- ^ `True` on success; `False` otherwise.
init (DaemonSocket daemonSocket) (TimeLimitMs timeLimitMs) =
  withMaybeTextCString daemonSocket $ \cDaemonSocket ->
    newrelic_init cDaemonSocket (CInt timeLimitMs)

-- |
-- Configure the C SDK's logging system.
--
-- If the logging system was previously initialized (either by a prior call to `configureLog`
-- or implicitly by a call to `init` or `createApp`), then invoking this function will close
-- the previous log file.
configureLog :: Text
                -- ^
                --  The path to the file to write logs to.
                -- If this is the literal string "stdout" or "stderr", then logs will be
                -- written to standard output or standard error, respectively.
                -> LogLevel
                -- ^
                -- The lowest level of log message that will be output.
                -> IO Bool
                -- ^ `True` on success; `False` otherwise.
configureLog filename level =
  withTextCString filename $ \cFilename ->
    newrelic_configure_log cFilename (toEnum (fromEnum level))

-- |
-- Create a populated application configuration.
--
-- Given an application name and license key, this method returns an SDK
-- configuration. Specifically, it returns an `AppConfig` with the given
-- application name and license key, along with default values for the
-- remaining fields.
--
-- The `AppConfig` can be further configured with `setDistributedTracingEnabled`,
-- `setDatastoreTracerInstanceReportingEnabled`, `setDatastoreTracerDatabaseNameReportingEnabled`,
-- `setSpanEventsEnabled`, `setTransactionTracerEnabled`, `setTransactionTracerDatastoreReportingSlowQueriesEnabled`
-- and `setTransactionTracerRecordSql`.
createAppConfig :: AppName
                   -- ^ The name of the application
                   -> LicenseKey
                   -- ^   A valid license key supplied by New Relic
                   -> IO AppConfig
                   -- ^ An application configuration populated with "application name" and "license key";
                   -- all other fields are initialized to their defaults.
createAppConfig (AppName appName) (LicenseKey licenseKey) =
  withTextCString appName $ \cAppName ->
    withTextCString licenseKey $ \cLicenseKey -> do
      configPtr <- newrelic_create_app_config cAppName cLicenseKey
      if configPtr == nullPtr
        then throwIO CantCreateNewRelicAppConfig
        else do
          foreignConfigPtr <- newForeignPtr configPtr (destroy configPtr newrelic_destroy_app_config)
          pure $ AppConfig foreignConfigPtr

-- |
-- Specifies whether or not distributed tracing is enabled.
--
-- When set to `True`, distributed tracing is enabled for the C SDK.
-- The default configuration returned by `createAppConfig` sets this value to `False`.
setDistributedTracingEnabled :: AppConfig -> Bool -> IO ()
setDistributedTracingEnabled (AppConfig foreignAppConfigPtr) value =
  withForeignPtr foreignAppConfigPtr $ \appConfigPtr ->
    pokeByteOff appConfigPtr (#offset newrelic_app_config_t, distributed_tracing.enabled)
      (boolToCInt value)

-- |
-- Configuration which controls whether datastore instance names are reported to New Relic.
--
-- If set to `True` for a transaction, instance names are reported to New Relic.
-- More specifically, the `datastoreSegmentHost` and `datastoreSegmentPortPathOrId` fields in a
-- `DatastoreSegment` passed to `startDatastoreSegment` is reported when the corresponding
-- transaction is reported.
setDatastoreTracerInstanceReportingEnabled :: AppConfig -> Bool -> IO ()
setDatastoreTracerInstanceReportingEnabled (AppConfig foreignAppConfigPtr) value =
  withForeignPtr foreignAppConfigPtr $ \appConfigPtr -> do
    pokeByteOff appConfigPtr (#offset newrelic_app_config_t, datastore_tracer.instance_reporting)
      (boolToCInt value)

-- |
-- Configuration which controls whether datastore database names are reported to New Relic.
--
-- If set to `True` for a transaction, database names are reported to New Relic.
-- More specifically, the `datastoreSegmentDatabaseName` in a `DatastoreSegment` passed to
-- `startDatastoreSegment` is reported when the corresponding transaction is reported.
setDatastoreTracerDatabaseNameReportingEnabled :: AppConfig -> Bool -> IO ()
setDatastoreTracerDatabaseNameReportingEnabled (AppConfig foreignAppConfigPtr) value =
  withForeignPtr foreignAppConfigPtr $ \appConfigPtr -> do
    pokeByteOff appConfigPtr (#offset newrelic_app_config_t, datastore_tracer.database_name_reporting)
      (boolToCInt value)

-- |
-- Specifies whether or not span events are generated.
--
-- When set to `True`, span events are generated by the C SDK.
-- The default configuration returned by `createAppConfig` sets this value to `True`.
setSpanEventsEnabled :: AppConfig -> Bool -> IO ()
setSpanEventsEnabled (AppConfig foreignAppConfigPtr) value =
  withForeignPtr foreignAppConfigPtr $ \appConfigPtr ->
    pokeByteOff appConfigPtr (#offset newrelic_app_config_t, span_events.enabled)
      (boolToCInt value)

-- |
-- Whether to enable transaction traces.
--
-- Default: `True`.
setTransactionTracerEnabled :: AppConfig -> Bool -> IO ()
setTransactionTracerEnabled (AppConfig foreignAppConfigPtr) value =
  withForeignPtr foreignAppConfigPtr $ \appConfigPtr -> do
    pokeByteOff appConfigPtr (#offset newrelic_app_config_t, transaction_tracer.enabled)
      (boolToCInt value)

-- |
-- Controls whether slow datastore queries are recorded.
--
-- If set to true for a transaction, the transaction tracer records the top-10 slowest
-- queries along with a stack trace of where the call occurred.
--
-- Default: `True`.
setTransactionTracerDatastoreReportingSlowQueriesEnabled :: AppConfig -> Bool -> IO ()
setTransactionTracerDatastoreReportingSlowQueriesEnabled (AppConfig foreignAppConfigPtr) value =
  withForeignPtr foreignAppConfigPtr $ \appConfigPtr -> do
    pokeByteOff appConfigPtr (#offset newrelic_app_config_t, transaction_tracer.datastore_reporting.enabled)
      (boolToCInt value)

-- |
-- Controls the format of the SQL put into transaction traces for supported SQL-like products.
--
-- Only relevant if `setTransactionTracerDatastoreReportingSlowQueriesEnabled` is set to `True`.
--
-- If set to `TransactionTracerRecordSqlOff`, transaction traces have no SQL in them.
-- If set to `TransactionTracerRecordSqlRaw`, the SQL is added to the transaction trace as-is.
-- If set to `TransactionTracerRecordSqlObfuscated`, alphanumeric characters are set to "?". For example @SELECT * FROM table WHERE foo = 42@ is reported as @SELECT * FROM table WHERE foo = ?@. These obfuscated queries are added to the transaction trace for supported datastore products.
--
-- Warning: New Relic highly discourages the use of the `TransactionTracerRecordSqlRaw`
-- setting in production environments.
--
-- Default: `TransactionTracerRecordSqlObfuscated`
setTransactionTracerRecordSql :: AppConfig -> TransactionTracerRecordSql -> IO ()
setTransactionTracerRecordSql (AppConfig foreignAppConfigPtr) value =
  withForeignPtr foreignAppConfigPtr $ \appConfigPtr -> do
    pokeByteOff appConfigPtr (#offset newrelic_app_config_t, transaction_tracer.datastore_reporting.record_sql)
      (toEnum (fromEnum value) :: CInt)

-- |
-- Create an application.
--
-- Given a configuration, `createApp` returns a newly allocated application,
-- or throws `CantCreateNewRelicApp` if there was an error.
createApp :: AppConfig
             -- ^ An application configuration created by `createAppConfig`.
             -> TimeoutMs
             -- ^ Specifies the maximum time to wait for a connection to be established;
             -- a value of 0 causes the method to make only one attempt at connecting to the daemon.
             -> IO App
             -- ^
             -- An allocated application, or throws on error; any errors resulting from a
             -- badly-formed configuration are logged.
createApp (AppConfig foreignAppConfigPtr) (TimeoutMs timeout) =
  withForeignPtr foreignAppConfigPtr $ \appConfigPtr -> do
    appPtr <- newrelic_create_app appConfigPtr (CShort timeout)
    if appPtr == nullPtr
      then throwIO CantCreateNewRelicApp
      else do
        foreignAppPtr <- newForeignPtr appPtr (destroy appPtr newrelic_destroy_app)
        pure $ App foreignAppPtr

-- |
-- Start a web based transaction.
--
-- Given an `App` and a transaction name, this function begins timing a new transaction.
-- It returns an active New Relic `Transaction`.
--
-- The return value of this function may be used as an input parameter to functions
-- that modify an active transaction.
startWebTransaction :: App
                      -- ^ An application.
                      -> Text
                      -- The name for the transaction.
                      -> IO (Maybe Transaction)
                      -- Just a transaction, or `Nothing` if the transaction couldn't be created.
startWebTransaction (App foreignAppPtr) name =
  withForeignPtr foreignAppPtr $ \appPtr ->
    withTextCString name $ \cName -> do
      transactionPtr <- newrelic_start_web_transaction appPtr cName
      if transactionPtr == nullPtr
        then pure Nothing
        else pure $ Just $ Transaction transactionPtr

-- |
-- Start a non-web based transaction.
--
-- Given an `App` and a transaction name, this function begins timing a new transaction.
-- It returns an active New Relic `Transaction`.
--
-- The return value of this function may be used as an input parameter to functions
-- that modify an active transaction.
startNonWebTransaction :: App
                          -- ^ An application.
                          -> Text
                          -- The name for the transaction.
                          -> IO (Maybe Transaction)
                          -- Just a transaction, or `Nothing` if the transaction couldn't be created.
startNonWebTransaction (App foreignAppPtr) name =
  withForeignPtr foreignAppPtr $ \appPtr ->
    withTextCString name $ \cName -> do
      transactionPtr <- newrelic_start_non_web_transaction appPtr cName
      if transactionPtr == nullPtr
        then pure Nothing
        else pure $ Just $ Transaction transactionPtr

-- |
-- Ignore the current transaction.
--
-- Given a transaction, this function instructs the C SDK to not send
-- data to New Relic for that transaction.
--
-- Warning: Even when `ignoreTransaction` is called, one must still call
-- `endTransaction` to free the memory used by the transaction and
-- avoid a memory leak.
ignoreTransaction :: Transaction -> IO Bool
ignoreTransaction (Transaction transactionPtr) =
  newrelic_ignore_transaction transactionPtr

-- |
-- Override the timing for the given transaction.
--
-- Transactions are normally timed automatically based on when they were started
-- and ended. Calling this function disables the automatic timing, and uses the
-- times given instead.
--
-- Note that this may cause unusual looking transaction traces. This function manually
-- alters a transaction's start time and duration, but it does not alter any timing
-- for the segments belonging to the transaction. As a result, the sum of all segment
-- durations may be substantively greater or less than the total duration of the transaction.
--
-- It is likely that users of this function will also want to
-- `setSegmentTiming` to manually time their segments.
setTransactionTiming :: Transaction
                        -- ^ The transaction to manually time.
                        -> StartTimeUsSinceUnixEpoch
                        -- ^ The start time for the segment, in microseconds since the UNIX Epoch.
                        -> DurationUs
                        -- ^ The duration of the transaction in microseconds.
                        -> IO Bool
                        -- ^ `True` if the segment timing was changed; `False` otherwise.
setTransactionTiming (Transaction transactionPtr) (StartTimeUsSinceUnixEpoch startTime) (DurationUs duration) =
  newrelic_set_transaction_timing transactionPtr (CULong startTime) (CULong duration)

-- |
-- End a transaction.
--
-- Given an active transaction, this function stops the transaction's timing,
-- sends any data to the New Relic daemon, and destroys the transaction.
--
-- Warning: This function must only be called once for a given transaction.
--
-- Returns `False` if data cannot be sent to newrelic; true otherwise.
endTransaction :: Transaction -> IO Bool
endTransaction (Transaction transactionPtr) =
  allocatePointerWith transactionPtr $ newrelic_end_transaction

-- |
-- Record the start of a custom segment in a transaction.
--
-- Given an active transaction this function creates a custom segment
-- to be recorded as part of the transaction. A subsequent call to
-- `endSegment` records the end of the segment.
startSegment :: Transaction
                -- ^ An active transaction
                -> Maybe Text
                -- ^ The segment name. If `Nothing` or an invalid name is passed,
                -- this defaults to "Unnamed segment".
                -> Maybe Text
                -- ^ The segment category. If `Nothing` or an invalid category is passed,
                -- this defaults to "Custom".
                -> IO (Maybe Segment)
                -- ^ A valid segment. `Nothing` if the segment couldn't be created.
startSegment transaction@(Transaction transactionPtr) name category =
  withMaybeTextCString name $ \cName ->
    withMaybeTextCString category $ \cCategory -> do
      segmentPtr <- newrelic_start_segment transactionPtr cName cCategory
      startSegmentHelper transaction segmentPtr

-- |
-- Start recording an external segment within a transaction.
--
-- Given an active transaction, this function creates an external segment inside of
-- the transaction and marks it as having been started. An external segment is
-- generally used to represent a HTTP or RPC request.
startExternalSegment :: Transaction
                        -- ^ An active transaction.
                        -> ExternalSegment
                        -- ^  The parameters describing the external request.
                        -> IO (Maybe Segment)
                        -- ^ An external segment, which may then be provided to
                        -- `endSegment` when the external request is complete.
                        -- If an error occurs when creating the external segment,
                        -- `Nothing` is returned, and a log message will be written
                        -- to the SDK log at LOG_ERROR level.
startExternalSegment transaction@(Transaction transactionPtr) externalSegment =
  allocatePointerWith externalSegment $ \externalSegmentPtr -> do
    segmentPtr <- newrelic_start_external_segment transactionPtr externalSegmentPtr
    startSegmentHelper transaction segmentPtr

-- |
-- Record the start of a datastore segment in a transaction.
--
-- Given an active transaction and valid parameters, this function creates
-- a datastore segment to be recorded as part of the transaction.
-- A subsequent call to `endSegment` records the end of the segment.
startDatastoreSegment :: Transaction
                         -- ^ An active transaction.
                         -> DatastoreSegment
                         -- ^ Valid parameters describing a datastore segment.
                         -> IO (Maybe Segment)
                        -- ^ A datastore segment, which may then be provided to
                        -- `endSegment` when the datastore request is complete.
                        -- If an error occurs when creating the datastore segment,
                        -- `Nothing` is returned, and a log message will be written
                        -- to the SDK log at LOG_ERROR level.
startDatastoreSegment transaction@(Transaction transactionPtr) datastoreSegment =
  allocatePointerWith datastoreSegment $ \datastoreSegmentPtr -> do
    segmentPtr <- newrelic_start_datastore_segment transactionPtr datastoreSegmentPtr
    startSegmentHelper transaction segmentPtr

startSegmentHelper :: Transaction -> Ptr Segment -> IO (Maybe Segment)
startSegmentHelper transaction segmentPtr =
  if segmentPtr == nullPtr
    then pure Nothing
    else pure $ Just $ Segment transaction segmentPtr

-- |
-- Set the transaction's root as the parent for the given segment.
--
-- Transactions are represented by a collection of segments. Segments
-- are created by calls to `startSegment`, `startDatastoreSegment` and
-- `startExternalSegment`. In addition, a transaction has an automatically-created
-- root segment that represents the entrypoint of the transaction.
-- In some cases, users may want to manually parent their segments with
-- the transaction's root segment.
setSegmentParentRoot :: Segment
                        -- ^ The segment to be parented.
                        -> IO Bool
                        -- ^ `True` if the segment was successfully reparented; `False` otherwise.
setSegmentParentRoot (Segment _ segmentPtr) =
  newrelic_set_segment_parent_root segmentPtr

-- |
-- Set the parent for the given segment.
--
-- This function changes the parent for the given segment to another segment.
-- Both segments must exist on the same transaction, and must not have ended.
setSegmentParent :: Segment
                    -- ^ The segment to reparent.
                    -> Segment
                    -- ^ The new parent segment.
                    -> IO Bool
                    -- ^ `True` if the segment was successfully reparented; `False` otherwise.
setSegmentParent (Segment _ segmentPtr) (Segment _ parentPtr) =
  newrelic_set_segment_parent segmentPtr parentPtr

-- |
-- Override the timing for the given segment.
--
-- Segments are normally timed automatically based on when they were started and ended.
-- Calling this function disables the automatic timing, and uses the times given instead.
--
-- Note that this may cause unusual looking transaction traces, as this function does not
-- change the parent segment. It is likely that users of this function will also want
-- to use `setSegmentParent` to manually parent their segments.
setSegmentTiming :: Segment
                    -- ^ The segment to manually time.
                    -> StartTimeUsSinceUnixEpoch
                    -- ^ The start time for the segment, in microseconds since the start of the transaction.
                    -> DurationUs
                    -- ^ The duration of the segment in microseconds.
                    -> IO Bool
                    -- ^ `True` if the segment timing was changed; `False` otherwise.
setSegmentTiming (Segment _ segmentPtr) (StartTimeUsSinceUnixEpoch startTime) (DurationUs duration) =
  newrelic_set_segment_timing segmentPtr (CULong startTime) (CULong duration)

-- |
-- Record the completion of a segment in a transaction.
--
-- Given an active transaction, this function records the segment's
-- metrics on the transaction.
endSegment :: Segment
              -- ^ The segment to end.
              -> IO Bool
              -- ^ `True` if the parameters represented an active segment to record as complete;
                -- `False` otherwise. If an error occurred, a log message will be written to the
                -- SDK log at LOG_ERROR level.
endSegment (Segment (Transaction transactionPtr) segmentPtr) =
  allocatePointerWith segmentPtr $ newrelic_end_segment transactionPtr

-- |
-- Add a custom integer attribute to a transaction.
--
-- Given an active transaction, this function appends an integer attribute to the transaction.
addAttributeInt :: Transaction  -- ^ An active transaction
                   -> Text      -- ^ The name of the attribute.
                   -> Int32     -- ^ The integer value of the attribute.
                   -> IO Bool   -- ^ `True` if successful; `False` otherwise.
addAttributeInt (Transaction transactionPtr) key value =
  withTextCString key $ \cKey ->
    newrelic_add_attribute_int transactionPtr cKey (CInt value)

-- |
-- Add a custom long attribute to a transaction.
--
-- Given an active transaction, this function appends an integer attribute to the transaction.
addAttributeLong :: Transaction  -- ^ An active transaction
                    -> Text      -- ^ The name of the attribute.
                    -> Int64     -- ^ The long value of the attribute.
                    -> IO Bool   -- ^ `True` if successful; `False` otherwise.
addAttributeLong (Transaction transactionPtr) key value =
  withTextCString key $ \cKey ->
    newrelic_add_attribute_long transactionPtr cKey (CLong value)

-- |
-- Add a custom double attribute to a transaction.
--
-- Given an active transaction, this function appends a double attribute to the transaction.
addAttributeDouble :: Transaction  -- ^ An active transaction
                      -> Text      -- ^ The name of the attribute.
                      -> Double    -- ^ The double value of the attribute.
                      -> IO Bool   -- ^ `True` if successful; `False` otherwise.
addAttributeDouble (Transaction transactionPtr) key value =
  withTextCString key $ \cKey ->
    newrelic_add_attribute_double transactionPtr cKey (CDouble value)

-- |
-- Add a custom text attribute to a transaction.
--
-- Given an active transaction, this function appends a text attribute to the transaction.
addAttributeText :: Transaction  -- ^ An active transaction
                    -> Text      -- ^ The name of the attribute.
                    -> Text      -- ^ The text value of the attribute.
                    -> IO Bool   -- ^ `True` if successful; `False` otherwise.
addAttributeText (Transaction transactionPtr) key value =
  withTextCString key $ \cKey ->
    withTextCString value $ \cValue ->
      newrelic_add_attribute_string transactionPtr cKey cValue

-- |
-- Generate a custom metric.
--
-- Given an active transaction and valid parameters, this function creates a custom
-- metric to be recorded as part of the transaction.
recordCustomMetric :: Transaction -- ^ An active transaction
                      -> Text     -- ^ The name/identifier for the metric.
                      -> Double   -- ^ The amount of time the metric will record, in milliseconds.
                      -> IO Bool  -- ^ `True` if successful; `False` otherwise.
recordCustomMetric (Transaction transactionPtr) metricName milliseconds =
  withTextCString (Text.append "Custom/" metricName) $ \cMetricName ->
    newrelic_record_custom_metric transactionPtr cMetricName (CDouble milliseconds)

-- |
-- Record an error in a transaction.
--
-- Given an active transaction, this function records an error inside of the transaction.
noticeError :: Transaction -- ^ An active transaction.
               -> Int32
               -- ^ The error's priority. The C SDK sends up one error per transaction.
               -- If multiple calls to this function are made during a single transaction,
               -- the error with the highest priority is reported to New Relic.
               -> Text
               -- ^ A string comprising the error message.
               -> Text
               -- ^ A string comprising the error class.
               -> IO ()
noticeError (Transaction transactionPtr) priority errorMessage errorClass =
  withTextCString errorMessage $ \cErrorMessage ->
    withTextCString errorClass $ \cErrorClass ->
      newrelic_notice_error transactionPtr (CInt priority) cErrorMessage cErrorClass

-- |
-- Creates a custom event.
--
-- Attributes can be added to the custom event using the @customEventAdd*@ family of functions.
-- When the required attributes have been added, the custom event can be recorded using
-- `recordCustomEvent`.
--
-- The created event must be eventually given to `recordCustomEvent` or `discardCustomEvent`,
-- otherwise the event will result in a memory leak.
createCustomEvent :: Text
                     -- ^ The type/name of the event
                     -> IO (Maybe CustomEvent)
                     -- ^ A custom event, or `Nothing` if the event couldn't be created.
createCustomEvent eventType =
  withTextCString eventType $ \cEventType -> do
    customEventPtr <- newrelic_create_custom_event cEventType
    if customEventPtr == nullPtr then
      pure Nothing
    else
      fmap (Just . CustomEvent) $ newrelic_create_custom_event cEventType

-- |
-- Records the custom event.
--
-- Given an active transaction, this function adds the custom event to the transaction
-- and timestamps it, ensuring the event will be sent to New Relic.
recordCustomEvent :: Transaction -> CustomEvent -> IO ()
recordCustomEvent (Transaction transactionPtr) (CustomEvent eventPtr) =
  allocatePointerWith eventPtr $ newrelic_record_custom_event transactionPtr

-- |
-- Adds an int key/value pair to the custom event's attributes.
--
-- Given a custom event, this function adds an integer attributes to the event.
customEventAddAttributeInt :: CustomEvent  -- ^ A custom event,
                              -> Text      -- ^ the string key for the key/value pair
                              -> Int32     -- ^ the integer value of the key/value pair
                              -> IO Bool   -- ^ `False` indicates the attribute could not be added
customEventAddAttributeInt (CustomEvent eventPtr) key value =
  withTextCString key $ \cKey ->
    newrelic_custom_event_add_attribute_int eventPtr cKey (CInt value)

-- |
-- Adds a long key/value pair to the custom event's attributes.
--
-- Given a custom event, this function adds a long attributes to the event.
customEventAddAttributeLong :: CustomEvent  -- ^ A custom event,
                               -> Text      -- ^ the string key for the key/value pair
                               -> Int64     -- ^ the long value of the key/value pair
                               -> IO Bool   -- ^ `False` indicates the attribute could not be added
customEventAddAttributeLong (CustomEvent eventPtr) key value =
  withTextCString key $ \cKey ->
    newrelic_custom_event_add_attribute_long eventPtr cKey (CLong value)

-- |
-- Adds a double key/value pair to the custom event's attributes.
--
-- Given a custom event, this function adds a double attributes to the event.
customEventAddAttributeDouble :: CustomEvent  -- ^ A custom event,
                                 -> Text      -- ^ the string key for the key/value pair
                                 -> Double    -- ^ the double value of the key/value pair
                                 -> IO Bool   -- ^ `False` indicates the attribute could not be added
customEventAddAttributeDouble (CustomEvent eventPtr) key value =
  withTextCString key $ \cKey ->
    newrelic_custom_event_add_attribute_double eventPtr cKey (CDouble value)

-- |
-- Adds a text key/value pair to the custom event's attributes.
--
-- Given a custom event, this function adds a text attribute to the event.
customEventAddAttributeText :: CustomEvent  -- ^ A custom event,
                               -> Text      -- ^ the string key for the key/value pair
                               -> Text      -- ^ the text value of the key/value pair
                               -> IO Bool   -- ^ `False` indicates the attribute could not be added
customEventAddAttributeText (CustomEvent eventPtr) key value =
  withTextCString key $ \cKey ->
    withTextCString value $ \cValue ->
      newrelic_custom_event_add_attribute_string eventPtr cKey cValue

-- |
-- Frees the memory for custom events created via the `createCustomEvent` function.
--
-- This function is here in case there's an allocated `CustomEvent` that ends up not
-- being recorded as a custom event, but still needs to be freed.
discardCustomEvent :: CustomEvent -> IO ()
discardCustomEvent (CustomEvent eventPtr) =
  allocatePointerWith eventPtr $ newrelic_discard_custom_event

-- |
-- Create a distributed trace payload at the root segment.
--
-- Create a newrelic header, or a payload, to add to a service's outbound requests.
-- This header contains the metadata necessary to link spans together for a complete
-- distributed trace. The metadata includes: the trace ID number, the span ID number,
-- New Relic account ID number, and sampling information. Note that a payload must be
-- created within an active transaction.
createRootDistributedTracePayload :: Transaction ->
                                     IO (Maybe Text)
                                     -- ^ If successful, a string to manually add to a
                                     -- service's outbound requests. If the instrumented
                                     -- application has not established a connection to the
                                     -- daemon or if distributed tracing is not enabled in
                                     -- the `AppConfig`, this function returns `Nothing`.
createRootDistributedTracePayload (Transaction transactionPtr) =
  createDistributedTracePayloadHelper
    $ newrelic_create_distributed_trace_payload transactionPtr nullPtr

-- |
-- Create a distributed trace payload at a given segment.
--
-- Create a newrelic header, or a payload, to add to a service's outbound requests.
-- This header contains the metadata necessary to link spans together for a complete
-- distributed trace. The metadata includes: the trace ID number, the span ID number,
-- New Relic account ID number, and sampling information. Note that a payload must be
-- created within an active transaction.
createDistributedTracePayload :: Segment ->
                                 IO (Maybe Text)
                                 -- ^ If successful, a string to manually add to a
                                 -- service's outbound requests. If the instrumented
                                 -- application has not established a connection to the
                                 -- daemon or if distributed tracing is not enabled in
                                 -- the `AppConfig`, this function returns `Nothing`.
createDistributedTracePayload (Segment (Transaction transactionPtr) segmentPtr) =
  createDistributedTracePayloadHelper
    $ newrelic_create_distributed_trace_payload transactionPtr segmentPtr

-- |
-- Create a distributed trace payload at the root segment, an HTTP-safe, base64-encoded string.
--
-- This function offers the same behaviour as `createRootDistributedTracePayload` but creates
-- a base64-encoded string for the payload.
createRootDistributedTracePayloadHttpSafe :: Transaction -> IO (Maybe Text)
createRootDistributedTracePayloadHttpSafe (Transaction transactionPtr) =
  createDistributedTracePayloadHelper
    $ newrelic_create_distributed_trace_payload_httpsafe transactionPtr nullPtr

-- |
-- Create a distributed trace payload at a given segment, an HTTP-safe, base64-encoded string.
--
-- This function offers the same behaviour as `createDistributedTracePayload` but creates
-- a base64-encoded string for the payload.
createDistributedTracePayloadHttpSafe :: Segment -> IO (Maybe Text)
createDistributedTracePayloadHttpSafe (Segment (Transaction transactionPtr) segmentPtr) =
  createDistributedTracePayloadHelper
    $ newrelic_create_distributed_trace_payload_httpsafe transactionPtr segmentPtr

createDistributedTracePayloadHelper :: IO CString -> IO (Maybe Text)
createDistributedTracePayloadHelper ioPayload = do
  cPayload <- ioPayload
  if cPayload == nullPtr
    then pure Nothing
    else do
      payload <- ByteString.packCString cPayload
      _ <- free cPayload
      pure $ Just $ Encoding.decodeUtf8 payload

-- |
-- Accept a distributed trace payload.
--
-- Accept newrelic headers, or a payload, created with `createRootDistributedTracePayload` or
-- `createDistributedTracePayload`. Such headers are manually added to a service's outbound request.
-- The receiving service gets the newrelic header from the incoming request and uses this function
-- to accept the payload and link corresponding spans together for a complete distributed trace.
-- Note that a payload must be accepted within an active transaction.
acceptDistributedTracePayload :: Transaction
                                 -- ^ An active transaction
                                 -> Text
                                 -- ^ A string created by `createRootDistributedTracePayload` or
                                 -- `createDistributedTracePayload`.
                                 -> TransportType
                                 -- ^ Transport type used for communicating the external call.
                                 -> IO Bool
                                 -- ^ `True` on success; `False` otherwise.
acceptDistributedTracePayload (Transaction transactionPtr) payload transportType =
  withTextCString payload $ \cPayload ->
    withCString (show transportType) $ \cTransportType ->
      newrelic_accept_distributed_trace_payload transactionPtr cPayload cTransportType

-- |
-- Accept a distributed trace payload, an HTTP-safe, base64-encoded string.
--
-- This function offers the same behaviour as `acceptDistributedTracePayload` but accepts a
-- base64-encoded string for the payload.
acceptDistributedTracePayloadHttpSafe :: Transaction -> Text -> TransportType -> IO Bool
acceptDistributedTracePayloadHttpSafe (Transaction transactionPtr) payload transportType =
  withTextCString payload $ \cPayload ->
    withCString (show transportType) $ \cTransportType ->
      newrelic_accept_distributed_trace_payload_httpsafe transactionPtr cPayload cTransportType

-- |
-- Get the NewRelic C SDK version.
--
-- If the version number is unavailable, the string \"NEWRELIC_VERSION\" will be returned.
version :: IO Text
version = do
  cVersion <- newrelic_version
  string <- ByteString.packCString cVersion
  pure $ Encoding.decodeUtf8 string

----------------------
-- Helper functions --
----------------------

destroy :: Storable a => a -> (Ptr a -> IO b) -> IO b
destroy ptr f =
  allocatePointerWith ptr $ f

withTextCString :: Text -> (CString -> IO a) -> IO a
withTextCString text =
  ByteString.useAsCString (Encoding.encodeUtf8 text)

withMaybeTextCString :: Maybe Text -> (CString -> IO a) -> IO a
withMaybeTextCString maybeText callback =
  case maybeText of
    Just text ->
      withTextCString text callback
    Nothing ->
      callback nullPtr

allocatePointerWith :: Storable a => a -> (Ptr a -> IO b) -> IO b
allocatePointerWith value callback =
  alloca $ \ptr -> do
    poke ptr value
    callback ptr

cStringToText :: CString -> IO Text
cStringToText cString =
  fmap Encoding.decodeUtf8 $ ByteString.packCString cString

cStringToMaybeText :: CString -> IO (Maybe Text)
cStringToMaybeText cString =
  if cString == nullPtr
    then pure Nothing
    else fmap Just $ cStringToText cString

peekTextOff :: Ptr a -> Int -> IO Text
peekTextOff ptr offset = do
  value <- peekByteOff ptr offset
  cStringToText value

peekMaybeTextOff :: Ptr a -> Int -> IO (Maybe Text)
peekMaybeTextOff ptr offset = do
  value <- peekByteOff ptr offset
  cStringToMaybeText value

pokeTextOff :: Ptr a -> Int -> Text -> IO ()
pokeTextOff ptr offset text =
  withTextCString text $ pokeByteOff ptr offset

pokeMaybeTextOff :: Ptr a -> Int -> Maybe Text -> IO ()
pokeMaybeTextOff ptr offset maybeText =
  case maybeText of
    Nothing -> pokeByteOff ptr offset nullPtr
    Just text -> pokeTextOff ptr offset text

boolToCInt :: Bool -> CInt
boolToCInt value =
  CInt (if value then 1 else 0)
