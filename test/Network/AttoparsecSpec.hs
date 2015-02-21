{-# LANGUAGE OverloadedStrings #-}

module Network.AttoparsecSpec where

import           Control.Concurrent               (ThreadId, forkIO, killThread,
                                                   threadDelay, myThreadId)

import           Control.Monad.IO.Class
import           Control.Monad.Catch

import qualified Data.Attoparsec.ByteString       as Atto

import qualified Network.Simple.TCP               as NS (Socket, accept,
                                                         connect, listen)
import qualified Network.Socket.ByteString        as NS (send)

import           Data.Attoparsec.ByteString.Char8 (decimal, endOfLine)
import qualified Network.Attoparsec               as Atto

import           Test.Hspec

pairSockets :: ( MonadIO m
               , MonadMask m)
            => (NS.Socket -> IO r0)
            -> (NS.Socket -> m r1)
            -> m r1
pairSockets writeCallback readCallback =
  let
    handleWrite :: NS.Socket -> IO ()
    handleWrite s = do
      putStrLn "handling writing"
      _ <- writeCallback s
      myTid <- myThreadId
      putStrLn ("handled write, my thread id = " ++ show myTid)
      return ()

    handleRead s = do
      result <- readCallback s
      return result

    handleServer :: IO ThreadId
    handleServer =
      return =<< forkIO $ NS.listen "*" "1234" $ \(lsock, _) ->
                                                  NS.accept lsock (handleWrite . fst)

  in do
    liftIO $ putStrLn "starting server"
    serverThread <- liftIO $ handleServer

    liftIO $ putStrLn ("started server, thread id = " ++ show serverThread)
    liftIO $ threadDelay 100000

    result <- NS.connect "127.0.0.1" "1234" (handleRead . fst)

    liftIO $ putStrLn "killing server"
    liftIO $ killThread serverThread
    liftIO $ putStrLn "killed server"

    return result

spec :: Spec
spec = do
  describe "when parsing a single object" $ do
    it "it should work with correct data" $
      let writeSocket s = NS.send s "1234"
          readSocket s  = Atto.parseOne s (Atto.parse numberParser)
          numberParser  = decimal

      in (pairSockets writeSocket readSocket) `shouldReturn` 1234

    it "it should work with partial data" $
      let writeSocket s = do
            _ <- NS.send s "12"
            threadDelay 100000
            NS.send s "34"

          readSocket s  = Atto.parseOne s (Atto.parse numberParser)
          numberParser  = decimal

      in (pairSockets writeSocket readSocket) `shouldReturn` 1234

    it "it should expect no unconsumed data" $
      let writeSocket s = NS.send s "1234ab"

          readSocket s  = Atto.parseOne s (Atto.parse numberParser)
          numberParser  = decimal

      in (pairSockets writeSocket readSocket) `shouldThrow` anyIOException

    it "it should throw an error when the provided data is incorrect" $
      let writeSocket s = NS.send s "ab"
          readSocket s  = Atto.parseOne s (Atto.parse numberParser)
          numberParser  = decimal

      in (pairSockets writeSocket readSocket) `shouldThrow` anyIOException

    it "it should throw an error when the socket is closed before parsing could be completed" $
      let writeSocket s = NS.send s "12"
          readSocket s  = Atto.parseOne s (Atto.parse numberParser)
          numberParser  = do
            _ <- "1234"
            _ <- endOfLine
            return ()

      in (pairSockets writeSocket readSocket) `shouldThrow` anyIOException

  describe "when parsing multiple matching objects" $ do
    let numberParser :: Atto.Parser Integer
        numberParser = do
          num <- decimal
          endOfLine
          return num

    it "should return error when using single object parser" $
      let writeSocket s = NS.send s "1234\n5678\n"
          readSocket s  = Atto.parseOne s (Atto.parse numberParser)

      in (pairSockets writeSocket readSocket) `shouldThrow` anyIOException

    it "should return multiple objects when using multi object parser" $
      let writeSocket s = NS.send s "1234\n5678\n"
          readSocket s  = do
            (_, xs) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            return xs
      in (pairSockets writeSocket readSocket) `shouldReturn` [1234, 5678]

    it "should return single object when using multi object parser and providing partial data" $
      let writeSocket s = NS.send s "1234\n56"
          readSocket s  = do
            (_, xs) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            return xs

      in (pairSockets writeSocket readSocket) `shouldReturn` [1234]

    it "should return multiple objects when using multi object parser and providing incremental data" $
      let writeSocket s = do
            _ <- NS.send s "1234\n56"
            threadDelay 100000
            NS.send s "78\n9012\n"
          readSocket s  = do
            (p, xs1) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            (_, xs2) <- Atto.parseMany s (Atto.parse numberParser) p
            return (xs1 ++ xs2)

      in (pairSockets writeSocket readSocket) `shouldReturn` [1234, 5678, 9012]

    it "should return nothing objects when using multi object parser and providing not enough data" $
      let writeSocket s = NS.send s "12"
          readSocket s  = do
            (_, xs) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            return xs

      in (pairSockets writeSocket readSocket) `shouldReturn` []

    it "should return one object when using multi object parser and providing incremental data" $
      let writeSocket s = do
            _ <- NS.send s "12"
            threadDelay 100000
            NS.send s "34\n"
          readSocket s  = do
            (p, xs1) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            (_, xs2) <- Atto.parseMany s (Atto.parse numberParser) p
            return (xs1 ++ xs2)

      in (pairSockets writeSocket readSocket) `shouldReturn` [1234]
