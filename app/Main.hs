module Main where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.List.NonEmpty as NE
import qualified Data.ByteString.Char8 as C
import GHC.IO.Handle
import GHC.IO.StdHandles
import Control.Monad (unless)
import Data.Functor (void)
import Control.Exception (evaluate)
import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearLine, setCursorColumn, hideCursor, showCursor)
import Control.Exception.Base (try)
import Control.Exception (SomeException)
import Data.Either (isLeft, fromLeft)
import System.Exit (exitSuccess)
import GHC.Base (when)
import Text.Wrap as W

prompt :: IO C.ByteString
prompt = do
  putStr "[*] perplex-cli> "
  query <- getLine
  putStr "\n"
  return $ C.pack $ query ++ "\r\n\r\n"

clearInputLine :: IO ()
clearInputLine = threadDelay 500000 >> putStr "\r\x1b[K" >> hFlush stdout 

dropIfExit :: C.ByteString -> IO ()
dropIfExit input = when (C.pack ":exit" `C.isInfixOf` input) exitSuccess >> pure ()


recvAndPrintToEnd :: Socket -> IO ()
recvAndPrintToEnd sock = do
  bs <- recv sock 1024
  unless  ((C.pack "\r\n\r\n") `C.isInfixOf` bs) $ C.putStr bs >> recvAndPrintToEnd sock
  C.putStr bs   

waitForReady :: Socket -> IO C.ByteString
waitForReady sock = recv sock 64    
    
clientLoop :: Socket -> IO ()
clientLoop sock = do
  input <- prompt
  _     <- sendAll sock input
  dropIfExit input 
  recvAndPrintToEnd sock
  clientLoop sock

waitThenClear :: IO ()
waitThenClear = threadDelay 500000 >> clearLine >> setCursorColumn 0

waitForConnect :: Socket -> SockAddr -> IO ()
waitForConnect sock sockAddr = _waitForConnect sock sockAddr 1 0
  
_waitForConnect :: Socket -> SockAddr -> Int -> Int -> IO ()
_waitForConnect sock sockAddr count attempts = do
  hideCursor
  newCount <- if count > 3 then pure 1 else pure count
  result <- try (connect sock sockAddr) :: IO (Either SomeException ())  
  if isLeft result && attempts > 10 then
    error $ "[!] Exiting due to error: " ++ (show result)
  else
    case result of
      Left  _                      -> (putStr $ "[*] Connecting" ++ (concat $ replicate newCount ".")) >> clearInputLine >> _waitForConnect sock sockAddr (succ count) (succ attempts)
      Right _                      -> putStrLn "[*] Connected" >> showCursor >> pure ()
    
   
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let hints = defaultHints { addrSocketType = Stream }
  addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "4321")
  sock <- openSocket addr

  waitForConnect sock $ addrAddress addr
  signal <- waitForReady sock
  C.putStrLn $ C.pack "[*] Signal received: " <> signal  
  clientLoop sock
  
