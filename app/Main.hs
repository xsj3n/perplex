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


prompt :: IO C.ByteString
prompt = do
  putStr "[*] perplex-cli> "
  query <- getLine
  putStr "\n"
  return $ C.pack $ query ++ "\r\n\r\n"


recvAndPrintToEnd :: Socket -> IO ()
recvAndPrintToEnd sock = do
  bs <- recv sock 1024
  unless  ((C.pack "\r\n\r\n") `C.isInfixOf` bs) $ C.putStr bs >> recvAndPrintToEnd sock
  C.putStr bs   

waitForReady :: Socket -> IO C.ByteString
waitForReady sock = recv sock 64    
    
clientLoop :: Socket -> IO ()
clientLoop sock = do
  prompt >>= (sendAll sock)
  recvAndPrintToEnd sock
  clientLoop sock 
   
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let hints = defaultHints { addrSocketType = Stream }
  addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "4321")
  sock <- openSocket addr
  
  connect sock $ addrAddress addr
  putStrLn "[*] Connected"
  signal <- waitForReady sock
  C.putStrLn $ C.pack "[*] Signal received: " <> signal  
  clientLoop sock
  
