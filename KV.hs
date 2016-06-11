module KV where

import Data.List
data KV = Table String [KV] | Entry String String

instance Show KV where
  show (Entry k v  ) = "\"" ++ k ++ "\" \"" ++ v ++ "\"\n"
  show (Table k kvs) = "\"" ++ k ++ "\"\n{\n" ++ (unlines . map ("    " ++) . lines . concatMap show $ kvs) ++ "}\n"
  

isDir :: KV -> Bool
isDir (Table _ _) = True
isDir _ = False

hasKey :: String -> KV -> Bool
hasKey k x = (getKey x) == k

getKey ::  KV -> String
getKey (Table k _) = k
getKey (Entry k _) = k

findKV :: [String] -> KV -> Maybe KV
findKV [] kv = Just kv
findKV (p:ps) (Table _ kvs) =
  find (hasKey p) kvs >>= findKV ps
findKV _ _ = Nothing

writeKV :: [String] -> String -> KV -> KV
writeKV []     x (Entry k _  ) = Entry k x
writeKV (p:ps) x (Table k kvs)
  | (b, kv':a) <- break (hasKey p) kvs = Table k $ b ++ writeKV ps x kv':a
writeKV _ _ kv = kv

readKV :: [String] -> KV -> Maybe String
readKV ps kv
  | Just (Entry _ x) <- findKV ps kv = Just x
readKV _ _ = Nothing

mkDir' :: [String] -> KV -> KV
mkDir' [p] (Table k kvs) = Table k $ (Table p []):kvs
mkDir' (p:ps) (Table _ kvs)
  | (Just kv) <- find (hasKey p) kvs = mkDir' ps kv
mkDir' _ kv = kv

mkFile' :: [String] -> KV -> KV
mkFile' [p] (Table k kvs) = Table k $ (Entry p []):kvs
mkFile' (p:ps) (Table _ kvs)
  | (Just kv) <- find (hasKey p) kvs = mkFile' ps kv
mkFile' _ kv = kv
