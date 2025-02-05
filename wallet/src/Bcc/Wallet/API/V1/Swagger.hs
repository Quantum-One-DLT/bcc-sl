{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bcc.Wallet.API.V1.Swagger where

import           Universum hiding (get, put)

import           Bcc.Wallet.API.Indices (ParamNames)
import           Bcc.Wallet.API.Request.Filter
import           Bcc.Wallet.API.Request.Pagination
import           Bcc.Wallet.API.Request.Sort
import           Bcc.Wallet.API.Response
import           Bcc.Wallet.API.V1.Generic (gconsName)
import           Bcc.Wallet.API.V1.Parameters
import           Bcc.Wallet.API.V1.Swagger.Example
import           Bcc.Wallet.API.V1.Types
import           Bcc.Wallet.TypeLits (KnownSymbols (..))

import           Pos.Chain.Update (SoftwareVersion (svNumber))
import           Pos.Core.NetworkMagic (NetworkMagic (..))
import           Pos.Util.CompileInfo (CompileTimeInfo, ctiGitRevision)
import           Pos.Util.Servant (LoggingApi)

import           Control.Lens (At, Index, IxValue, at, (?~))
import           Data.Aeson (encode)
import           Data.Aeson.Encode.Pretty
import           Data.Map (Map)
import           Data.Swagger hiding (Example)
import           Data.Typeable
import           Formatting (build, sformat)
import           NeatInterpolation
import           Servant (Handler, ServantErr (..), Server, StdMethod (..))
import           Servant.API.Sub
import           Servant.Swagger
import           Servant.Swagger.UI (SwaggerSchemaUI')
import           Servant.Swagger.UI.Core (swaggerSchemaUIServerImpl)
import           Servant.Swagger.UI.ReDoc (redocFiles)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Pos.Core as Core
import qualified Pos.Core.Attributes as Core
import qualified Pos.Crypto.Hashing as Crypto


--
-- Helper functions
--

-- | Surround a Text with another
surroundedBy :: Text -> Text -> Text
surroundedBy wrap context = wrap <> context <> wrap

-- | Display a multi-line code-block inline (e.g. in tables)
inlineCodeBlock :: Text -> Text
inlineCodeBlock txt = "<pre>" <> replaceNewLines (replaceWhiteSpaces txt) <> "</pre>"
  where
    replaceNewLines    = T.replace "\n" "<br/>"
    replaceWhiteSpaces = T.replace " " "&nbsp;"


-- | Drill in the 'Swagger' file in an unsafe way to modify a specific operation
-- identified by a tuple (verb, path). The function looks a bit scary to use
-- but is actually rather simple (see example below).
--
-- Note that if the identified path doesn't exist, the function will throw
-- at runtime when trying to read the underlying swagger structure!
--
-- Example:
--
--     swagger
--       & paths %~ (POST, "/api/v1/wallets") `alterOperation` (description ?~ "foo")
--       & paths %~ (GET, "/api/v1/wallets/{walletId}") `alterOperation` (description ?~ "bar")
--
alterOperation ::
    ( IxValue m ~ item
    , Index m ~ FilePath
    , At m
    , HasGet item (Maybe Operation)
    , HasPut item (Maybe Operation)
    , HasPatch item (Maybe Operation)
    , HasPost item (Maybe Operation)
    , HasDelete item (Maybe Operation)
    )
    => (StdMethod, FilePath)
    -> (Operation -> Operation)
    -> m
    -> m
alterOperation (verb, path) alter =
    at path %~ (Just . unsafeAlterItem)
  where
    errUnreachableEndpoint :: Text
    errUnreachableEndpoint =
        "Unreachable endpoint: " <> show verb <> " " <> show path

    errUnsupportedVerb :: Text
    errUnsupportedVerb =
        "Used unsupported verb to identify an endpoint: " <> show verb

    unsafeAlterItem ::
        ( HasGet item (Maybe Operation)
        , HasPut item (Maybe Operation)
        , HasPatch item (Maybe Operation)
        , HasPost item (Maybe Operation)
        , HasDelete item (Maybe Operation)
        )
        => Maybe item
        -> item
    unsafeAlterItem = maybe
        (error errUnreachableEndpoint)
        (unsafeLensFor verb %~ (Just . unsafeAlterOperation))

    unsafeAlterOperation :: Maybe Operation -> Operation
    unsafeAlterOperation = maybe
        (error errUnreachableEndpoint)
        alter

    unsafeLensFor ::
        ( Functor f
        , HasGet item (Maybe Operation)
        , HasPut item (Maybe Operation)
        , HasPatch item (Maybe Operation)
        , HasPost item (Maybe Operation)
        , HasDelete item (Maybe Operation)
        )
        => StdMethod
        -> (Maybe Operation -> f (Maybe Operation))
        -> item
        -> f item
    unsafeLensFor = \case
        GET    -> get
        PUT    -> put
        PATCH  -> patch
        POST   -> post
        DELETE -> delete
        _      -> error errUnsupportedVerb


-- | A combinator to modify the description of an operation, using
-- 'alterOperation' under the hood.
--
--
-- Example:
--
--     swagger
--       & paths %~ (POST, "/api/v1/wallets") `setDescription` "foo"
--       & paths %~ (GET, "/api/v1/wallets/{walletId}") `setDescription` "bar"
setDescription
    :: (IxValue m ~ PathItem, Index m ~ FilePath, At m)
    => (StdMethod, FilePath)
    -> Text
    -> m
    -> m
setDescription endpoint str =
    endpoint `alterOperation` (description ?~ str)


--
-- Instances
--

instance HasSwagger a => HasSwagger (LoggingApi config a) where
    toSwagger _ = toSwagger (Proxy @a)

instance
    ( Typeable res
    , KnownSymbols syms
    , HasSwagger subApi
    , syms ~ ParamNames res params
    ) => HasSwagger (FilterBy params res :> subApi) where
    toSwagger _ =
        let swgr       = toSwagger (Proxy @subApi)
            allOps     = map toText $ symbolVals (Proxy @syms)
        in swgr & over (operationsOf swgr . parameters) (addFilterOperations allOps)
          where
            addFilterOperations :: [Text] -> [Referenced Param] -> [Referenced Param]
            addFilterOperations ops xs = map (Inline . newParam) ops <> xs

            newParam :: Text -> Param
            newParam opName =
                let typeOfRes = fromString $ show $ typeRep (Proxy @ res)
                in Param {
                  _paramName = opName
                , _paramRequired = Nothing
                , _paramDescription = Just $ filterDescription typeOfRes
                , _paramSchema = ParamOther ParamOtherSchema {
                         _paramOtherSchemaIn = ParamQuery
                       , _paramOtherSchemaAllowEmptyValue = Nothing
                       , _paramOtherSchemaParamSchema = mempty
                       }
                }

filterDescription :: Text -> Text
filterDescription typeOfRes = mconcat
    [ "A **FILTER** operation on a " <> typeOfRes <> ". "
    , "Filters support a variety of queries on the resource. "
    , "These are: \n\n"
    , "- `EQ[value]`    : only allow values equal to `value`\n"
    , "- `LT[value]`    : allow resource with attribute less than the `value`\n"
    , "- `GT[value]`    : allow objects with an attribute greater than the `value`\n"
    , "- `GTE[value]`   : allow objects with an attribute at least the `value`\n"
    , "- `LTE[value]`   : allow objects with an attribute at most the `value`\n"
    , "- `RANGE[lo,hi]` : allow objects with the attribute in the range between `lo` and `hi`\n"
    , "- `IN[a,b,c,d]`  : allow objects with the attribute belonging to one provided.\n\n"
    ]

instance
    ( Typeable res
    , KnownSymbols syms
    , syms ~ ParamNames res params
    , HasSwagger subApi
    ) => HasSwagger (SortBy params res :> subApi) where
    toSwagger _ =
        let swgr       = toSwagger (Proxy @subApi)
        in swgr & over (operationsOf swgr . parameters) addSortOperation
          where
            addSortOperation :: [Referenced Param] -> [Referenced Param]
            addSortOperation xs = Inline newParam : xs

            newParam :: Param
            newParam =
                let typeOfRes = fromString $ show $ typeRep (Proxy @ res)
                    allowedKeys = T.intercalate "," (map toText $ symbolVals (Proxy @syms))
                in Param {
                  _paramName = "sort_by"
                , _paramRequired = Just False
                , _paramDescription = Just (sortDescription typeOfRes allowedKeys)
                , _paramSchema = ParamOther ParamOtherSchema {
                         _paramOtherSchemaIn = ParamQuery
                       , _paramOtherSchemaAllowEmptyValue = Just True
                       , _paramOtherSchemaParamSchema = mempty
                       }
                }

instance (HasSwagger subApi) => HasSwagger (WalletRequestParams :> subApi) where
    toSwagger _ =
        let swgr       = toSwagger (Proxy @(WithWalletRequestParams subApi))
        in swgr & over (operationsOf swgr . parameters) (map toDescription)
          where
            toDescription :: Referenced Param -> Referenced Param
            toDescription (Inline p@(_paramName -> pName)) =
                case M.lookup pName requestParameterToDescription of
                    Nothing -> Inline p
                    Just d  -> Inline (p & description .~ Just d)
            toDescription x = x

instance ToParamSchema WalletId

instance ToSchema Core.Address where
    declareNamedSchema = pure . paramSchemaToNamedSchema defaultSchemaOptions

instance ToParamSchema Core.Address where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString

instance ToParamSchema (V1 Core.Address) where
  toParamSchema _ = toParamSchema (Proxy @Core.Address)


--
-- Descriptions
--

customQueryFlagToDescription :: Map T.Text T.Text
customQueryFlagToDescription = M.fromList [
    ("force_ntp_check", forceNtpCheckDescription)
  ]

requestParameterToDescription :: Map T.Text T.Text
requestParameterToDescription = M.fromList [
    ("page", pageDescription)
  , ("per_page", perPageDescription (fromString $ show maxPerPageEntries) (fromString $ show defaultPerPageEntries))
  ]

forceNtpCheckDescription :: T.Text
forceNtpCheckDescription = [text|
In some cases, API Clients need to force a new NTP check as a previous result gets cached. A typical use-case is after asking a user to fix its system clock. If this flag is set, request will block until NTP server responds or it will timeout if NTP server is not available within a short delay.
|]

pageDescription :: T.Text
pageDescription = [text|
The page number to fetch for this request. The minimum is **1**.  If nothing is specified, **this value defaults to 1** and always shows the first entries in the requested collection.
|]

perPageDescription :: T.Text -> T.Text -> T.Text
perPageDescription maxValue defaultValue = [text|
The number of entries to display for each page. The minimum is **1**, whereas the maximum is **$maxValue**. If nothing is specified, **this value defaults to $defaultValue**.
|]

sortDescription :: Text -> Text -> Text
sortDescription resource allowedKeys = [text|
A **SORT** operation on this $resource. Allowed keys: `$allowedKeys`.
|]

errorsDescription :: Text
errorsDescription = [text|
Error Name / Description | HTTP Error code | Example
-------------------------|-----------------|---------
$errors
|] where
  errors = T.intercalate "\n" rows
  rows =
    -- 'WalletError'
    [ mkRow fmtErr $ NotEnoughMoney (ErrAvailableBalanceIsInsufficient 1400)
    , mkRow fmtErr $ OutputIsRedeem sampleAddress
    , mkRow fmtErr $ UnknownError "Unexpected internal error."
    , mkRow fmtErr $ InvalidAddressFormat "Provided address format is not valid."
    , mkRow fmtErr WalletNotFound
    , mkRow fmtErr $ WalletAlreadyExists exampleWalletId
    , mkRow fmtErr AddressNotFound
    , mkRow fmtErr $ InvalidPublicKey "Extended public key (for external wallet) is invalid."
    , mkRow fmtErr UnsignedTxCreationError
    , mkRow fmtErr $ SignedTxSubmitError "Unable to submit externally-signed transaction."
    , mkRow fmtErr TooBigTransaction
    , mkRow fmtErr TxFailedToStabilize
    , mkRow fmtErr TxRedemptionDepleted
    , mkRow fmtErr $ TxSafeSignerNotFound sampleAddress
    , mkRow fmtErr $ MissingRequiredParams (("wallet_id", "walletId") :| [])
    , mkRow fmtErr $ WalletIsNotReadyToProcessPayments genExample
    , mkRow fmtErr $ NodeIsStillSyncing genExample
    , mkRow fmtErr $ CannotCreateAddress "Cannot create derivation path for new address in external wallet."
    , mkRow fmtErr $ RequestThrottled 42

    -- 'JSONValidationError'
    , mkRow fmtErr $ JSONValidationFailed "Expected String, found Null."

    -- 'UnsupportedMimeTypeError'
    , mkRow fmtErr $ UnsupportedMimeTypePresent "Expected Content-Type's main MIME-type to be 'application/json'."
    , mkRow fmtErr $ UtxoNotEnoughFragmented (ErrUtxoNotEnoughFragmented 1 msgUtxoNotEnoughFragmented)
    -- TODO 'MnemonicError' ?
    ]
  mkRow fmt err = T.intercalate "|" (fmt err)
  fmtErr err =
    [ surroundedBy "`" (gconsName err) <> "<br/>" <> toText (sformat build err)
    , show $ errHTTPCode $ toServantError err
    , inlineCodeBlock (T.decodeUtf8 $ BL.toStrict $ encodePretty err)
    ]

  sampleAddress = V1 Core.Address
      { Core.addrRoot =
          Crypto.unsafeAbstractHash ("asdfasdf" :: String)
      , Core.addrAttributes =
          Core.mkAttributes $ Core.AddrAttributes Nothing Core.BootstrapEraDistr NetworkMainOrStage
      , Core.addrType =
          Core.ATPubKey
      }


-- | Shorter version of the doc below, only for Dev & V0 documentations
highLevelShortDescription :: DescriptionEnvironment -> T.Text
highLevelShortDescription DescriptionEnvironment{..} = [text|
This is the specification for the Bcc Wallet API, automatically generated as a [Swagger](https://swagger.io/) spec from the [Servant](http://haskell-servant.readthedocs.io/en/stable/) API of [Bcc](https://github.com/The-Blockchain-Company/bcc-sl).

Protocol Version   | Git Revision
-------------------|-------------------
$deSoftwareVersion | $deGitRevision
|]


-- | Provide additional insights on V1 documentation
highLevelDescription :: DescriptionEnvironment -> T.Text
highLevelDescription DescriptionEnvironment{..} = [text|
This is the specification for the Bcc Wallet API, automatically generated as a [Swagger](https://swagger.io/) spec from the [Servant](http://haskell-servant.readthedocs.io/en/stable/) API of [Bcc](https://github.com/The-Blockchain-Company/bcc-sl).

Protocol Version   | Git Revision
-------------------|-------------------
$deSoftwareVersion | $deGitRevision


Getting Started
===============

In the following examples, we will use *curl* to illustrate request to an API running on the default port **8090**.

Please note that wallet web API uses TLS for secure communication. Requests to the API need to
send a client CA certificate that was used when launching the node and identifies the client as
being permitted to invoke the server API.

Creating a New Wallet
---------------------

You can create your first wallet using the [`POST /api/v1/wallets`](#tag/Wallets%2Fpaths%2F~1api~1v1~1wallets%2Fpost) endpoint as follow:

```
curl -X POST https://localhost:8090/api/v1/wallets \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cert ./scripts/tls-files/client.pem \
  --cacert ./scripts/tls-files/ca.crt \
  -d '{
  "operation": "create",
  "backupPhrase": $deMnemonicExample,
  "assuranceLevel": "normal",
  "name": "MyFirstWallet",
  "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
}'
```

> **Warning**: Those 12 mnemonic words given for the backup phrase act as an example. **Do
> not** use them on a production system. See the section below about mnemonic codes for more
> information.

The `spendingPassword` is optional but highly recommended. It a string of 32
characters, encoded in base 16, yielding to an hexadecimal sequence of 64 bytes.
This passphrase is required for sensitive operations on the wallet and adds
an extra security layer to it.

To generate a valid `spendingPassword`, please follow the following steps:

- Pick a long sentence using a wide variety of characters (uppercase, lowercase,
  whitespace, punctuation, etc). Using a computer to randomly generate
  a passphrase is best, as humans aren't a good source of randomness.

- Compute an appropriate hash of this passphrase. You'll need to use an
  algorithm that yields a 32-byte long string (e.g. *SHA256* or *BLAKE2b*).

- Hex-encode the 32-byte hash into a 64-byte sequence of bytes.

As a response, the API provides you with a unique wallet `id` to be used in subsequent
requests. Make sure to store it / write it down. Note that every API response is
[jsend-compliant](https://labs.omniti.com/labs/jsend); Bcc also augments responses with
meta-data specific to pagination. More details in the section below about [Pagination](#section/Pagination)

```json
$createWallet
```

You have just created your first wallet. Information about this wallet can be retrieved using the [`GET /api/v1/wallets/{walletId}`](#tag/Wallets%2Fpaths%2F~1api~1v1~1wallets~1{walletId}%2Fget)
endpoint as follows:

```
curl -X GET https://localhost:8090/api/v1/wallets/{{walletId}} \
     -H "Accept: application/json; charset=utf-8" \
     --cacert ./scripts/tls-files/ca.crt \
     --cert ./scripts/tls-files/client.pem
```

Receiving BCC
-------------

To receive _BCC_ from other users you should provide your address. This address can be obtained
from an account. Each wallet contains at least one account. An account is like a pocket inside
of your wallet. Vew all existing accounts of a wallet by using the [`GET /api/v1/wallets/{{walletId}}/accounts`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1accounts%2Fget)
endpoint:

```
curl -X GET https://localhost:8090/api/v1/wallets/{{walletId}}/accounts?page=1&per_page=10 \
  -H "Accept: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```

Since you have, for now, only a single wallet, you'll see something like this:

```json
$readAccounts
```

All the wallet's accounts are listed under the `addresses` field. You can communicate one of
these addresses to receive _BCC_ on the associated account.


Sending BCC
-----------

In order to send _BCC_ from one of your accounts to another address, you must create a new
payment transaction using the [`POST /api/v1/transactions`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions%2Fpost)
endpoint as follows:

```
curl -X POST https://localhost:8090/api/v1/transactions \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem \
  -d '{
  "destinations": [{
    "amount": 14,
    "address": "A7k5bz1QR2...Tx561NNmfF"
  }],
  "source": {
    "accountIndex": 0,
    "walletId": "Ae2tdPwUPE...8V3AVTnqGZ"
  },
  "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
}'
```

Note that, in order to perform a transaction, you need to have enough existing _BCC_ on the
source account! The Bcc API is designed to accomodate multiple recipients payments
out-of-the-box; notice how `destinations` is a list of addresses (and corresponding amounts).

When the transaction succeeds, funds are no longer available in the sources addresses, and are
soon made available to the destinations within a short delay. Note that, you can at any time see
the status of your wallets by using the [`GET /api/v1/transactions`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions%2Fget)
endpoint as follows:

```
curl -X GET https://localhost:8090/api/v1/transactions?wallet_id=Ae2tdPwUPE...8V3AVTnqGZ\
     -H "Accept: application/json; charset=utf-8" \
     --cacert ./scripts/tls-files/ca.crt \
     --cert ./scripts/tls-files/client.pem
```

Here we constrained the request to a specific account. After our previous transaction the output
should look roughly similar to this:

```json
$readTransactions
```

In addition, and because it is not possible to _preview_ a transaction, one can lookup a
transaction's fees using the [`POST /api/v1/transactions/fees`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions~1fees%2Fpost)
endpoint to get an estimation of those fees.
See [Estimating Transaction Fees](#section/Common-Use-Cases/Estimating-Transaction-Fees) for more details.


Pagination
==========

**All GET requests of the API are paginated by default**. Whilst this can be a source of
surprise, is the best way of ensuring the performance of GET requests is not affected by the
size of the data storage.

Version `V1` introduced a different way of requesting information to the API. In particular,
GET requests which returns a _collection_ (i.e. typically a JSON array of resources) lists
extra parameters which can be used to modify the shape of the response. In particular, those
are:

* `page`: (Default value: **1**).
* `per_page`: (Default value: **$deDefaultPerPage**)

For a more accurate description, see the section `Parameters` of each GET request, but as a
brief overview the first two control how many results and which results to access in a
paginated request.


Filtering and Sorting
=====================

`GET` endpoints which list collection of resources supports filters & sort operations, which
are clearly marked in the swagger docs with the `FILTER` or `SORT` labels. The query format is
quite simple, and it goes this way:


Filter Operators
----------------

| Operator | Description                                                               | Example                |
|----------|---------------------------------------------------------------------------|------------------------|
| -        | If **no operator** is passed, this is equivalent to `EQ` (see below).     | `balance=10`           |
| `EQ`     | Retrieves the resources with index _equal_ to the one provided.           | `balance=EQ[10]`       |
| `LT`     | Retrieves the resources with index _less than_ the one provided.          | `balance=LT[10]`       |
| `LTE`    | Retrieves the resources with index _less than equal_ the one provided.    | `balance=LTE[10]`      |
| `GT`     | Retrieves the resources with index _greater than_ the one provided.       | `balance=GT[10]`       |
| `GTE`    | Retrieves the resources with index _greater than equal_ the one provided. | `balance=GTE[10]`      |
| `RANGE`  | Retrieves the resources with index _within the inclusive range_ [k,k].    | `balance=RANGE[10,20]` |

Sort Operators
--------------

| Operator | Description                                                               | Example                |
|----------|---------------------------------------------------------------------------|------------------------|
| `ASC`    | Sorts the resources with the given index in _ascending_ order.            | `sort_by=ASC[balance]` |
| `DES`    | Sorts the resources with the given index in _descending_ order.           | `sort_by=DES[balance]` |
| -        | If **no operator** is passed, this is equivalent to `DES` (see above).    | `sort_by=balance`      |


Errors
======

In case a request cannot be served by the API, a non-2xx HTTP response will be issued, together
with a [JSend-compliant](https://labs.omniti.com/labs/jsend) JSON Object describing the error
in detail together with a numeric error code which can be used by API consumers to implement
proper error handling in their application. For example, here's a typical error which might be
issued:

``` json
$deErrorExample
```

Existing Wallet Errors
----------------------

$deWalletErrorTable


Monetary Denomination & Units
=============================

Bcc's currency is called _BCC_ ( ₳ ). _BCC_ has up to **6** decimal places; hence the
smallest monetary unit that can be represented in the Bcc's blockhain is: 0.000001₳. This
is also called a _Entropic_ (Bcc's currency is named after the mathematician and computer
scientist [Bcc Entropic](https://en.wikipedia.org/wiki/Bcc_Entropic)). Put in another way, one
_BCC_ is equal to one million _Entropic_.

BCC        | Entropic
-----------|----------
`1`        | `1 000 000`
`.000 001` | `1`

> **Warning**: All amounts manipulated in the API are given and expected in Entropic.


Mnemonic Codes
==============

The full list of accepted mnemonic codes to secure a wallet is defined by the [BIP-39
specifications](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki). Note that
picking up 12 random words from the list **is not enough** and leads to poor security. Make
sure to carefully follow the steps described in the protocol when you generate words for a new
wallet.


Versioning & Legacy
===================

The API is **versioned**, meaning that is possible to access different versions of the API by adding the _version number_ in the URL.

**For the sake of backward compatibility, we expose the legacy version of the API, available simply as unversioned endpoints.**

This means that _omitting_ the version number would call the old version of the API. Deprecated
endpoints are currently grouped under an appropriate section; they would be removed in upcoming
released, if you're starting a new integration with Bcc-SL, please ignore these.

Note that Compatibility between major versions is not _guaranteed_, i.e. the request & response formats might differ.


Disable TLS (Not Recommended)
-----------------------------

If needed, you can disable TLS by providing the `--no-tls` flag to the wallet or by running a wallet in debug mode with `--wallet-debug` turned on.


Common Use-Cases
================

Sending Money to Multiple Recipients
------------------------------------

As seen in [Sending BCC](#section/Getting-Started/Sending-BCC), you can send _BCC_ to
another party using the [`POST /api/v1/transactions`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions%2Fpost) endpoint.
Important to notice is the type of the field `destinations`: it's a list, enabling you to provide more
than one destination. Each destination is composed of:

- An address
- A corresponding amount

The overall transaction corresponds to the sum of each outputs. For instance, to send money to
two parties simultaneously:

```
curl -X POST https://localhost:8090/api/v1/transactions \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem \
  -d '{
  "destinations": [
    {
      "amount": 14,
      "address": "A7k5bz1QR2...Tx561NNmfF"
    },
    {
      "amount": 42,
      "address": "B56n78WKE8...jXAa34NUFz"
    }
  ],
  "source": {
    "accountIndex": 0,
    "walletId": "Ae2tdPwUPE...8V3AVTnqGZ"
  },
  "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
}'
```


About UTXO Fragmentation
------------------------

As described in [Sending Money to Multiple Recipients](#section/Common-Use-Cases/Sending-Money-to-Multiple-Recipients), it is possible to send bcc to more than one destination. Bcc only allows a given UTXO to cover at most one single transaction output. As a result,
when the number of transaction outputs is greater than the number the API returns a `UtxoNotEnoughFragmented` error which
looks like the following
```
{
    "status": "error",
    "diagnostic": {
        "details": {
            "help": "Utxo is not enough fragmented to handle the number of outputs of this transaction. Query /api/v1/wallets/{walletId}/statistics/utxos endpoint for more information",
            "missingUtxos": 1
        }
    },
    "message": "UtxoNotEnoughFragmented"
}
```

To make sure the source account has a sufficient level of UTXO fragmentation (i.e. number of UTXOs),
please monitor the state of the UTXOs as described in [Getting UTXO Statistics](#section/Common-Use-Cases/Getting-Utxo-Statistics). The
number of wallet UTXOs should be no less than the transaction outputs, and the sum of all UTXOs should be enough to cover the total
transaction amount, including fees.

Contrary to a classic accounting model, there's no such thing as spending _part of a UTXO_, and one has to wait for a transaction to be included in a
block before spending the remaining change. This is very similar to using bank notes: one can't spend a USD 20 bill at two different shops at the same time,
even if it is enough to cover both purchases — one has to wait for change from the first transaction before making the second one.
There's no "ideal" level of fragmentation; it depends on one's needs. However, the more UTXOs that are available, the higher the concurrency capacity
of one's wallet, allowing multiple transactions to be made at the same time.

Similarly, there's no practical maximum number of UTXOs, but there is nevertheless a maximum transaction size. By having many small UTXOs,
one is taking the risk of hitting that restriction, should too many inputs be selected to fill a transaction. The only way to
work around this is to make multiple smaller transactions.

Estimating Transaction Fees
---------------------------

When you submit a transaction to the network, some fees apply depending on, but not only, the
selected grouping policy and the available inputs on the source wallet. There's actually a
trade-off between fees, cryptographic security, throughput and privacy. The more inputs are
selected, the bigger is the payload, the bigger are the fees.

The API lets you estimate fees for a given transaction via the [`POST /api/v1/transaction/fees`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions~1fees%2Fpost)
endpoint. The request payload is identical to the one you would make to create a transaction:

```
curl -X POST https://localhost:8090/api/v1/transactions/fees \
  -H "Accept: application/json; charset=utf-8" \
  -H "Content-Type: application/json; charset=utf-8" \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem \
  -d '{
  "destinations": [{
      "amount": 14,
      "address": "A7k5bz1QR2...Tx561NNmfF"
  }],
  "source": {
    "accountIndex": 0,
    "walletId": "Ae2tdPwUPE...8V3AVTnqGZ"
  }
}'
```

The API resolves with an estimated amount in _BCC_. This estimation highly depends on the
current state of the ledger and diverges with time.

```json
$readFees
```


Managing Accounts
-----------------

A wallet isn't limited to one account. It can actually be useful to have more than one account
in order to separate business activities. With the API, you can retrieve a specific account,
create new ones, list all existing accounts of a wallet or edit a few things on an existing
account. By default, your wallet comes with a provided account. Let's see how to create a fresh
new account on a wallet using [`POST /api/v1/wallets/{{walletId}}/accounts`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1accounts%2Fpost):

```
curl -X POST \
  https://localhost:8090/api/v1/Ae2tdPwUPE...8V3AVTnqGZ/accounts \
  -H 'Content-Type: application/json;charset=utf-8' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem \
  -d '{
  "name": "MyOtherAccount",
  "spendingPassword": "5416b2988745725998907addf4613c9b0764f04959030e1b81c603b920a115d0"
}'
```

Note that the `spendingPassword` here should match the one provided earlier in [Creating a
New Wallet](#section/Getting-Started/Creating-a-New-Wallet).


```json
$createAccount
```

You can always retrieve this account description later if needed via [`GET /api/v1/wallets/{{walletId}}/accounts/{{accountId}}`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1accounts~1{accountId}%2Fget).

For example:

```
curl -X GET \
  https://127.0.0.1:8090/api/v1/wallets/Ae2tdPwUPE...8V3AVTnqGZ/accounts/2902829384 \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```

For a broader view, the full list of accounts of a given wallet can be retrieved using [`GET /api/v1/wallets/{{walletId}}/accounts`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1accounts%2Fget)
```
curl -X GET \
  https://127.0.0.1:8090/api/v1/wallets/Ae2tdPwUPE...8V3AVTnqGZ/accounts \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```

```json
$readAccounts
```

Partial Representations
-----------------------

The previous endpoint gives you a list of full representations. However, in some cases, it might be interesting to retrieve only a partial representation of an account (e.g. only the balance). There are two extra endpoints one could use to either fetch a given account's balance, and another to retrieve the list of addresses associated to a specific account.

[`GET /api/v1/wallets/{{walletId}}/accounts/{{accountId}}/addresses`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1%7BwalletId%7D~1accounts~1%7BaccountId%7D~1addresses%2Fget)

```json
$readAccountAddresses
```

Note that this endpoint is paginated and allow basic filtering and sorting on
addresses. Similarly, you can retrieve only the account balance with:

[`GET /api/v1/wallets/{{walletId}}/accounts/{{accountId}}/amount`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1%7BwalletId%7D~1accounts~1%7BaccountId%7D~1amount%2Fget)


```json
$readAccountBalance
```


Managing Addresses
------------------

By default, wallets you create are provided with an account which has one default address. It
is possible (and recommended) for an account to manage multiple addresses. Address reuse
actually reduces privacy for it tights more transactions to a small set of addresses.

When paying, the wallet makes many of these choices for you. Addresses are
selected from a wallet's account based on several different strategies and
policies.

To create a new address, use the [`POST /api/v1/addresses`](#tag/Addresses%2Fpaths%2F~1api~1v1~1addresses%2Fpost)
endpoint:

```
curl -X POST \
  https://localhost:8090/api/v1/addresses \
  -H 'Content-Type: application/json;charset=utf-8' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem \
  -d '{
        "walletId": "Ae2tdPwUPE...V3AVTnqGZ4",
        "accountIndex": 2147483648
}'
```

```json
$createAddress
```

If your wallet is protected with a password, this password is also required in order to create
new addresses for that wallet. In such case, the field `spendingPassword` should match the one
defined earlier to protect your wallet.

Addresses generated as just described are always valid. When the API encounters
an invalid address however (e.g. when provided by another party), it will fail with a
client error.

You can always view all your available addresses across all your wallets by using
[`GET /api/v1/addresses`](#tag/Addresses%2Fpaths%2F~1api~1v1~1addresses%2Fget):

```
curl -X GET https://localhost:8090/api/v1/addresses \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```

```json
$readAddresses
```

Checking Synchronization Progress
---------------------------------

You can control the synchronization progress of the underlying node hosting the wallet's server
via [`GET /api/v1/node-info`](#tag/Info%2Fpaths%2F~1api~1v1~1node-info%2Fget). The output is
rather verbose and gives real-time progress updates about the current node.

```
curl -X GET https://localhost:8090/api/v1/node-info \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```

```json
$readNodeInfo
```


Retrieving Transaction History
------------------------------

If needed, applications may regularly poll the wallet's backend to retrieve the history of
transactions of a given wallet. Using the [`GET /api/v1/transactions`](#tag/Transactions%2Fpaths%2F~1api~1v1~1transactions%2Fget)
endpoint, you can view the status of all transactions that ever sent or took money from the
wallet.

The following table sums up the available filters (also detailed in the endpoint documentation details):

Filter On                   | Corresponding Query Parameter(s)
----------------------------| ------------------------------
Wallet                      | `wallet_id`
Wallet's account            | `account_index` + `wallet_id`
Address                     | `address`
Transaction's creation time | `created_at`
Transaction's id            | `id`

For example, in order to retrieve the last 50 transactions of a particular account,
ordered by descending date:

```
curl -X GET https://127.0.0.1:8090/api/v1/transactions?wallet_id=Ae2tdPwU...3AVTnqGZ&account_index=2902829384&sort_by=DES\[created_at\]&per_page=50' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```
For example, in order to retrieve the last 50 transactions, ordered by descending date:

```
curl -X GET 'https://127.0.0.1:8090/api/v1/transactions?wallet_id=Ae2tdPwU...3AVTnqGZ &sort_by=DES\[created_at\]&per_page=50' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```


Another example, if you were to look for all transactions made since the 1st of January 2018:

```
curl -X GET 'https://127.0.0.1:8090/api/v1/transactions?wallet_id=Ae2tdPwU...3AVTnqGZ&created_at=GT\[2018-01-01T00:00:00.00000\]' \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```


Getting Utxo statistics
---------------------------------

You can get Utxo statistics of a given wallet using
 [`GET /api/v1/wallets/{{walletId}}/statistics/utxos`](#tag/Accounts%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1statistics~1utxos%2Fget)

```
curl -X GET \
  https://127.0.0.1:8090/api/v1/wallets/Ae2tdPwUPE...8V3AVTnqGZ/statistics/utxos \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem
```

```json
$readUtxoStatistics
```
Make sure to carefully read the section about [Pagination](#section/Pagination) to fully
leverage the API capabilities.


Importing (Unused) Addresses From a Previous Node (or Version)
--------------------------------------------------------------

When restoring a wallet, only the information available on the blockchain can
be retrieved. Some pieces of information aren't stored on
the blockchain and are only defined as _Metadata_ of the wallet backend. This
includes:

- The wallet's name
- The wallet's assurance level
- The wallet's spending password
- The wallet's unused addresses

Unused addresses are not recorded on the blockchain and, in the case of random
derivation, it is unlikely that the same addresses will be generated on two
different node instances. However, some API users may wish to preserve unused
addresses between different instances of the wallet backend.

To enable this, the wallet backend provides an endpoint ([`POST /api/v1/wallets/{{walletId}}/addresses`](#tag/Addresses%2Fpaths%2F~1api~1v1~1wallets~1{walletId}~1addresses%2Fpost))
to import a list of addresses into a given account. Note that this endpoint is
quite lenient when it comes to errors: it tries to import all provided addresses
one by one, and ignores any that can't be imported for whatever reason. The
server will respond with the total number of successes and, if any, a list of
addresses that failed to be imported. Trying to import an address that is already
present will behave as a no-op.

For example:

```
curl -X POST \
  https://127.0.0.1:8090/api/v1/wallets/Ae2tdPwUPE...8V3AVTnqGZ/addresses \
  -H 'Accept: application/json;charset=utf-8' \
  --cacert ./scripts/tls-files/ca.crt \
  --cert ./scripts/tls-files/client.pem \
  -d '[
    "Ae2tdPwUPE...8V3AVTnqGZ",
    "Ae2odDwvbA...b6V104CTV8"
  ]'
```

> **IMPORTANT**: This feature is experimental and performance is
> not guaranteed. Users are advised to import small batches only.

|]
  where
    createAccount        = decodeUtf8 $ encodePretty $ genExample @(APIResponse Account)
    createAddress        = decodeUtf8 $ encodePretty $ genExample @(APIResponse WalletAddress)
    createWallet         = decodeUtf8 $ encodePretty $ genExample @(APIResponse Wallet)
    readAccounts         = decodeUtf8 $ encodePretty $ genExample @(APIResponse [Account])
    readAccountBalance   = decodeUtf8 $ encodePretty $ genExample @(APIResponse AccountBalance)
    readAccountAddresses = decodeUtf8 $ encodePretty $ genExample @(APIResponse AccountAddresses)
    readAddresses        = decodeUtf8 $ encodePretty $ genExample @(APIResponse [Address])
    readFees             = decodeUtf8 $ encodePretty $ genExample @(APIResponse EstimatedFees)
    readNodeInfo         = decodeUtf8 $ encodePretty $ genExample @(APIResponse NodeInfo)
    readTransactions     = decodeUtf8 $ encodePretty $ genExample @(APIResponse [Transaction])
    readUtxoStatistics   = decodeUtf8 $ encodePretty $ genExample @(APIResponse UtxoStatistics)

-- | Provide an alternative UI (ReDoc) for rendering Swagger documentation.
swaggerSchemaUIServer
    :: (Server api ~ Handler Swagger)
    => Swagger -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServer =
    swaggerSchemaUIServerImpl redocIndexTemplate redocFiles
  where
    redocIndexTemplate :: Text
    redocIndexTemplate = [text|
<!doctype html>
<html lang="en">
  <head>
    <title>ReDoc</title>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
      body { margin: 0; padding: 0; }
    </style>
    <script>
        // Force Strict-URL Routing for assets relative paths
        (function onload() {
            if (!window.location.pathname.endsWith("/")) {
                window.location.pathname += "/";
            }
        }());
    </script>
  </head>
  <body>
    <redoc spec-url="../SERVANT_SWAGGER_UI_SCHEMA"></redoc>
    <script src="redoc.min.js"> </script>
  </body>
</html>|]

applyUpdateDescription :: Text
applyUpdateDescription = [text|
Apply the next available update proposal from the blockchain. Note that this
will immediately shutdown the node and makes it unavailable for a short while.
|]

postponeUpdateDescription :: Text
postponeUpdateDescription = [text|
Discard the next available update from the node's local state. Yet, this doesn't
reject the update which will still be applied as soon as the node is restarted.
|]

resetWalletStateDescription :: Text
resetWalletStateDescription = [text|
Wipe-out the node's local state entirely. The only intended use-case for this
endpoint is during API integration testing. Note also that this will fail by
default unless the node is running in debug mode.
|]

estimateFeesDescription :: Text
estimateFeesDescription = [text|
Estimate the fees which would incur from the input payment. This endpoint
**does not** require a _spending password_ to be supplied as it generates
under the hood an unsigned transaction.
|]

getAddressDescription :: Text
getAddressDescription = [text|
The previous version of this endpoint failed with an HTTP error when the given
address was unknown to the wallet.

This was misleading since an address that is unknown to the wallet may still
belong to the wallet (since it could be part of a pending transaction in
another instance of the same wallet).

To reflect this, the V1 endpoint does not fail when an address is not recognised
and returns a new field which indicates the address' ownership status, from the
node point of view.
|]

--
-- The API
--

data DescriptionEnvironment = DescriptionEnvironment
  { deErrorExample     :: !T.Text
  , deDefaultPerPage   :: !T.Text
  , deWalletErrorTable :: !T.Text
  , deGitRevision      :: !T.Text
  , deSoftwareVersion  :: !T.Text
  , deMnemonicExample  :: !T.Text
  }

api :: HasSwagger a
    => (CompileTimeInfo, SoftwareVersion)
    -> Proxy a
    -> (DescriptionEnvironment -> T.Text)
    -> Swagger
api (compileInfo, curSoftwareVersion) walletAPI mkDescription = toSwagger walletAPI
  & info.title   .~ "Bcc Wallet API"
  & info.version .~ fromString (show curSoftwareVersion)
  & host ?~ "127.0.0.1:8090"
  & info.description ?~ mkDescription DescriptionEnvironment
    { deErrorExample          = decodeUtf8 $ encodePretty WalletNotFound
    , deMnemonicExample       = decodeUtf8 $ encode (genExample @BackupPhrase)
    , deDefaultPerPage        = fromString (show defaultPerPageEntries)
    , deWalletErrorTable      = errorsDescription
    , deGitRevision           = ctiGitRevision compileInfo
    , deSoftwareVersion       = fromString $ show (svNumber curSoftwareVersion)
    }
  & info.license ?~ ("MIT" & url ?~ URL "https://raw.githubusercontent.com/The-Blockchain-Company/bcc-sl/develop/lib/LICENSE")
  & paths %~ (POST,   "/api/internal/apply-update")       `setDescription` applyUpdateDescription
  & paths %~ (POST,   "/api/internal/postpone-update")    `setDescription` postponeUpdateDescription
  & paths %~ (DELETE, "/api/internal/reset-wallet-state") `setDescription` resetWalletStateDescription
  & paths %~ (POST,   "/api/v1/transactions/fees")        `setDescription` estimateFeesDescription
  & paths %~ (GET,    "/api/v1/addresses/{address}")      `setDescription` getAddressDescription
