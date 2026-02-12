module Main where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.List.NonEmpty as NE
import qualified Data.ByteString.Char8 as C
import GHC.IO.Handle
import GHC.IO.StdHandles
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearLine, setCursorColumn, hideCursor, showCursor)
import Control.Exception.Base (try)
import Control.Exception (SomeException)
import Data.Either (isLeft)
import System.Exit (exitSuccess)
import GHC.Base (when)
import qualified Text.Wrap as W
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import Control.Monad.Extra (whenM)


prompt :: IO C.ByteString
prompt = do
  putStr "[*] perplex-cli> "
  query <- getLine
  putStr "\n"
  return $ C.pack $ query ++ "\r\n\r\n"

clearInputLine :: IO ()
clearInputLine = threadDelay 500000 >> putStr "\r\x1b[K"

dropIfExit :: C.ByteString -> IO ()
dropIfExit input = when (C.pack ":exit" `C.isInfixOf` input) exitSuccess >> pure ()

checkForHelpCommand :: C.ByteString -> IO Bool
checkForHelpCommand input = if C.pack ":help" `C.isInfixOf` input then do
                              mapM_ putStrLn [ ":length <short|mid|long|uncapped> - Sets the response length\n"
                                             , ":ctx <off|on> - Whether to keep prior queries in context\n"
                                             , ":exit - Exit the program\n"
                                             ]
                              pure True
                            else
                              pure False 

recvAndPrintToEnd :: Socket -> IO ()
recvAndPrintToEnd sock = do
  bs <- recv sock 1024
  unless  (C.pack "\r\n\r\n" `C.isInfixOf` bs) $ wrapPrint bs >> recvAndPrintToEnd sock
  putStr "\n"
  
waitForReady :: Socket -> IO C.ByteString
waitForReady sock = recv sock 64    
    
clientLoop :: Socket -> IO ()
clientLoop sock = do
  input <- prompt
  whenM (checkForHelpCommand input) $ clientLoop sock 
  _     <- sendAll sock input
  dropIfExit input
  recvAndPrintToEnd sock
  clientLoop sock

waitThenClear :: IO ()
waitThenClear = threadDelay 500000 >> clearLine >> setCursorColumn 0

waitForConnect :: Socket -> SockAddr -> IO ()
waitForConnect sock sockAddr = _waitForConnect sock sockAddr 1 0

wrapPrint :: C.ByteString -> IO ()
wrapPrint str = TIO.putStrLn $ W.wrapText W.defaultWrapSettings 80 $ decodeUtf8Lenient str
  
_waitForConnect :: Socket -> SockAddr -> Int -> Int -> IO ()
_waitForConnect sock sockAddr count attempts = do
  hideCursor
  newCount <- if count > 3 then pure 0 else pure count
  result <- try (connect sock sockAddr) :: IO (Either SomeException ())  
  if isLeft result && attempts > 10 then
    error $ "[!] Exiting due to error: " ++ (show result)
  else
    case result of
      Left  _                      -> (putStr $ "[*] Connecting" ++ (concat $ replicate newCount ".")) >> clearInputLine >> _waitForConnect sock sockAddr (succ newCount) (succ attempts)
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
  


