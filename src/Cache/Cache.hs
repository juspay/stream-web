{-# LANGUAGE OverloadedStrings #-}

module Cache.Cache where

import           Data.Hashable       (hash)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Data.Time.Clock     (NominalDiffTime, UTCTime, addUTCTime,
                                      getCurrentTime)
import           Prelude             hiding (lookup)
import qualified Streamly.Prelude    as SP

type Key = T.Text

type Hash = Int

type Value = T.Text

type Info = (Value, Hash, UTCTime)

data SearchValue
  = NotFound
  | Same
  | Found Value
  deriving (Eq, Show)

type Cache = HM.HashMap Key Info

defaultTtl :: NominalDiffTime
defaultTtl = 3600

empty :: Cache
empty = HM.empty

insert :: Key -> Value -> UTCTime -> Cache -> Cache
insert key value expiryTime = HM.insert key (value, hash value, expiryTime)

-- SearchValue will be:
-- NotFound - if its not found or the value has expired (its removed from the cache also)
-- Same - if its found and live and hash is same as the hash argument
-- Found Value - if its found and live and hash is not equal to the hash argument
lookup :: UTCTime -> Maybe Hash -> Key -> Cache -> (SearchValue, Cache)
lookup currentTime maybeHash key cache =
  let maybeInfo = HM.lookup key cache
   in case maybeInfo
    -- key not present
            of
        Nothing -> (NotFound, cache)
        Just (value, hsh, expiryTime)
          | currentTime > expiryTime
           -- key is present but it expired
           -> (NotFound, HM.delete key cache)
           -- key is present and live
             -- hashes are equal
          | Just hsh == maybeHash -> (Same, cache)
          | otherwise -> (Found value, cache)

data Op
  = Insert Key Value (Maybe NominalDiffTime)
  | Lookup Key (Maybe Hash)
  | Delete Key
  deriving (Eq, Show)

data Result
  = Inserted
  | Deleted
  | LookedUp SearchValue
  deriving (Eq, Show)

apply :: Op -> Cache -> IO (Result, Cache)
apply (Insert k v maybeTtl) cache = do
  currentTime <- getCurrentTime
  let ttlSeconds = fromMaybe defaultTtl maybeTtl
  let expiryTime = addUTCTime ttlSeconds currentTime
  return (Inserted, insert k v expiryTime cache)
apply (Lookup k maybeHash) cache = do
  currentTime <- getCurrentTime
  let (searchVal, cache') = lookup currentTime maybeHash k cache
  return (LookedUp searchVal, cache')
apply (Delete k) cache = return (Deleted, HM.delete k cache)

run :: IO [(Result, Cache)]
run =
  SP.toList $
  SP.scanlM' (\(_, cache) op -> apply op cache) (LookedUp NotFound, empty) $
  SP.fromList
    [ Insert "a" "aa" (Just 0)
    , Insert "b" "bb" Nothing
    , Insert "c" "cc" (Just 8500)
    , Lookup "a" Nothing
    , Lookup "b" (Just 123)
    , Lookup "c" (Just . hash $ ("cc" :: Value))
    , Delete "a"
    , Delete "c"
    ]

test :: IO ()
test = run >>= putStrLn . printList

printList :: (Show a) => [a] -> String
printList = unlines . map ((++) "# " . show)
