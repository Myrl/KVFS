module Main where

import qualified Data.ByteString as B
import Foreign.C.Error
import Foreign.C.Types
import System.Fuse
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import Codec.Binary.Base64
import Data.List
import Text.Parsec
import Data.Maybe
import Data.IORef
import Control.Debounce
import Control.Arrow
import Debug.Trace
import Control.Monad

import KV
import Parser.KV

kVFSOps :: IORef KV -> FuseOperations ()
kVFSOps ref = defaultFuseOps { fuseGetFileStat        = kVGetFileStat ref
                             , fuseOpen               = kVOpen ref
                             , fuseRead               = kVRead ref
                             , fuseWrite              = kVWrite ref
                             , fuseOpenDirectory      = kVOpenDirectory ref
                             , fuseReadDirectory      = kVReadDirectory ref
                             , fuseGetFileSystemStats = kVGetFileSystemStats
                             , fuseSetFileSize        = kVSetFileSize ref
                             , fuseCreateDirectory    = kVCreateDirectory ref
                             , fuseCreateDevice       = kVCreateDevice ref
                             }

splitOn' :: (Eq a) => a -> [a] -> [[a]]
splitOn' x xs
  | (h, _:t) <- break (== x) xs = h:splitOn' x t
splitOn' _ xs = [xs]

splitOn x = filter (/= "") . splitOn' x

kVGetFileStat :: IORef KV -> FilePath -> IO (Either Errno FileStat)
kVGetFileStat ref path = do
  kv <- readIORef ref
  ctx <- getFuseContext
  case (findKV (splitOn '/' path) kv) of
    Nothing -> return . Left $ eNOENT
    Just x -> return . Right $ makeStat ctx x

makeStat ctx (Table _ kvs) = FileStat { statEntryType = Directory
                                      , statFileMode = foldr1 unionFileModes
                                                         [ ownerReadMode
                                                         , ownerExecuteMode
                                                         , groupReadMode
                                                         , groupExecuteMode
                                                         , otherReadMode
                                                         , otherExecuteMode
                                                         ]
                                      , statLinkCount = CNlink . (2+) . genericLength . filter isDir $ kvs
                                      , statFileOwner = fuseCtxUserID ctx
                                      , statFileGroup = fuseCtxGroupID ctx
                                      , statSpecialDeviceID = 0
                                      , statFileSize = 4096
                                      , statBlocks = 1
                                      , statAccessTime = 0
                                      , statModificationTime = 0
                                      , statStatusChangeTime = 0
                                      }
makeStat ctx (Entry _ v) = FileStat { statEntryType = RegularFile
                                    , statFileMode = foldr1 unionFileModes
                                                       [ ownerReadMode
                                                       , groupReadMode
                                                       , otherReadMode
                                                       ]
                                    , statLinkCount = 1
                                    , statFileOwner = fuseCtxUserID ctx
                                    , statFileGroup = fuseCtxGroupID ctx
                                    , statSpecialDeviceID = 0
                                    , statFileSize = COff . genericLength . fromMaybe [] . decode $ v
                                    , statBlocks = 1
                                    , statAccessTime = 0
                                    , statModificationTime = 0
                                    , statStatusChangeTime = 0
                                    }


kVOpen :: IORef KV -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno ())
kVOpen ref path _ _ = do
  kv <- readIORef ref
  case findKV (splitOn '/' path) kv of
    Nothing -> return . Left $ eNOENT
    Just _ -> return . Right $ ()

kVWrite :: IORef KV -> FilePath -> () -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
kVWrite ref path _ x (COff i) = do
  kv <- readIORef ref
  case readKV (splitOn '/' path) kv of
    Just v -> do let (h,t) = genericSplitAt i . fromMaybe [] . decode $ v
                 let v' = encode . concat $ [ h, B.unpack x, drop (B.length x) t ]
                 atomicWriteIORef ref $ writeKV (splitOn '/' path) v' kv
                 readIORef ref >>= writeFile "/home/myrl/KVFS/fs.kv" . show
                 return . Right . CSize . fromIntegral . B.length $ x
    _ -> return . Left $ eNOENT
    
kVRead :: IORef KV -> FilePath -> () -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
kVRead ref path _ (CSize n) (COff i) = do
  kv <- readIORef ref
  case readKV (splitOn '/' path) kv of
    Just v -> return . Right . B.pack . genericTake n . genericDrop i . (++ repeat 0) . fromMaybe [] . decode $ v
    _ -> return . Left $ eNOENT

kVSetFileSize :: IORef KV -> FilePath -> FileOffset -> IO Errno
kVSetFileSize ref path (COff n) = do
  kv <- readIORef ref
  case readKV (splitOn '/' path) kv of
    Just v -> do let v' = encode . genericTake n . fromMaybe [] . decode $ v
                 atomicWriteIORef ref $ writeKV (splitOn '/' path) v' kv
                 return eOK
    _ -> return eNOENT

kVCreateDirectory :: IORef KV -> FilePath -> FileMode -> IO Errno
kVCreateDirectory _ "/" _ = return eEXIST
kVCreateDirectory ref path _ = do
  kv <- readIORef ref
  case findKV (splitOn '/' path) kv of
    Nothing -> do
      atomicModifyIORef ref $ \x -> (mkDir' (splitOn '/' path) x, ())
      readIORef ref >>= writeFile "/home/myrl/KVFS/fs.kv" . show
      return eOK
    _ -> return eEXIST
 
kVCreateDevice :: IORef KV -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
kVCreateDevice _ "/" _ _ _ = return eEXIST
kVCreateDevice ref path _ _ _= do
  kv <- readIORef ref
  case findKV (splitOn '/' path) kv of
    Nothing -> do
      atomicModifyIORef ref $ \x -> (mkFile' (splitOn '/' path) x, ())
      readIORef ref >>= writeFile "/home/myrl/KVFS/fs.kv" . show
      return eOK
    _ -> return eEXIST
 
kVOpenDirectory :: IORef KV -> FilePath -> IO Errno
kVOpenDirectory ref path = do
  kv <- readIORef ref
  return $ case findKV (splitOn '/' path) kv of
             Just (Table _ _) -> eOK
             _ -> eNOTDIR

kVReadDirectory :: IORef KV -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
kVReadDirectory ref path = do
  kv <- readIORef ref
  ctx <- getFuseContext
  case findKV (splitOn '/' path) kv of
    Just kv@(Table _ kvs) -> do
      return . Right . ([("..", dirStat ctx), (".", makeStat ctx kv)] ++) . map ((getKey) &&& makeStat ctx) $ kvs
    _ -> return . Left $ eNOTDIR

dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }
              
kVGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
kVGetFileSystemStats _ =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }    

main :: IO ()
main = do
  x <- readFile "fs.kv"
  case parse kvParser "" x of
    Left _ -> putStrLn "Corrupted kv file."
    Right kv -> do
      ref <- newIORef kv
      fuseMain (kVFSOps ref) defaultExceptionHandler

