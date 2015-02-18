{-# LANGUAGE OverloadedStrings #-}

module Network.AttoparsecSpec where

import           Control.Concurrent               (forkIO, killThread,
                                                   threadDelay)
import           Control.Monad.Catch
import           Control.Monad.Error

import qualified Data.Attoparsec.ByteString       as Atto

import qualified Network.Simple.TCP               as NS (Socket, accept,
                                                         connect, listen)
import qualified Network.Socket.ByteString        as NS (send)

import           Data.Attoparsec.ByteString.Char8 (decimal, endOfLine)
import qualified Network.Attoparsec               as Atto

import           Test.Hspec

pairSockets :: ( MonadIO m
               , MonadMask m)
            => (NS.Socket -> IO a)
            -> (NS.Socket -> IO b)
            -> m b
pairSockets writeCallback readCallback = do
  serverThread <- liftIO $ forkIO $ NS.listen "*" "1234" (\(lsock, _) ->
                                                           NS.accept lsock (\pair -> do
                                                                                     _ <- writeCallback (fst pair)
                                                                                     return ()))

  liftIO $ threadDelay 100000

  result <- NS.connect "127.0.0.1" "1234" (liftIO . readCallback . fst)

  liftIO $ killThread serverThread

  return result

spec :: Spec
spec = do
  describe "when parsing a single object" $ do
    it "it should work with correct data" $
      let writeSocket s = NS.send s "1234"
          readSocket s  = runErrorT $ Atto.parseOne s (Atto.parse numberParser)
          numberParser  = decimal

      in (pairSockets writeSocket readSocket) `shouldReturn` Right 1234

    it "it should work with partial data" $
      let writeSocket s = do
            _ <- NS.send s "12"
            threadDelay 100000
            NS.send s "34"

          readSocket s  = runErrorT $ Atto.parseOne s (Atto.parse numberParser)
          numberParser  = decimal

      in (pairSockets writeSocket readSocket) `shouldReturn` Right 1234

    it "it should leave unconsumed data in tact" $
      let writeSocket s = do
            NS.send s "1234ab"

          readSocket s  = runErrorT $ Atto.parseOne s (Atto.parse numberParser)
          numberParser  = decimal

      in (pairSockets writeSocket readSocket) `shouldReturn` Left "Unconsumed data left on socket: \"ab\""

    it "it should throw an error when the provided data is incorrect" $
      let writeSocket s = do
            NS.send s "ab"

          readSocket s  = runErrorT $ Atto.parseOne s (Atto.parse numberParser)
          numberParser  = decimal

      in (pairSockets writeSocket readSocket) `shouldReturn` Left "An error occured while parsing input: \"ab\""

  describe "when parsing multiple matching objects" $ do
    it "should return error when using single object parser" $
      let writeSocket s = NS.send s "1234\n5678\n"
          readSocket s  = runErrorT $ Atto.parseOne s (Atto.parse numberParser)
          numberParser  = do
            num <- decimal
            endOfLine
            return num

      in (pairSockets writeSocket readSocket) `shouldReturn` Left "Unconsumed data left on socket: \"5678\\n\""

    it "should return multiple objects when using multi object parser" $
      let writeSocket s = NS.send s "1234\n5678\n"
          readSocket s  = runErrorT $ do
            (_, xs) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            return xs
          numberParser  = do
            num <- decimal
            endOfLine
            return num

      in (pairSockets writeSocket readSocket) `shouldReturn` Right [1234, 5678]

    it "should return single object when using multi object parser and providing partial data" $
      let writeSocket s = NS.send s "1234\n56"
          readSocket s  = runErrorT $ do
            (_, xs) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            return xs
          numberParser  = do
            num <- decimal
            endOfLine
            return num

      in (pairSockets writeSocket readSocket) `shouldReturn` Right [1234]

    it "should return multiple objects when using multi object parser and providing incremental data" $
      let writeSocket s = do
            _ <- NS.send s "1234\n56"
            threadDelay 100000
            NS.send s "78\n9012\n"
          readSocket s  = runErrorT $ do
            (p, xs1) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            (_, xs2) <- Atto.parseMany s (Atto.parse numberParser) p
            return (xs1 ++ xs2)
          numberParser  = do
            num <- decimal
            endOfLine
            return num

      in (pairSockets writeSocket readSocket) `shouldReturn` Right [1234, 5678, 9012]

    it "should return nothing objects when using multi object parser and providing not enough data" $
      let writeSocket s = NS.send s "12"
          readSocket s  = runErrorT $ do
            (_, xs) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            return xs
          numberParser  = do
            num <- decimal
            endOfLine
            return num

      in (pairSockets writeSocket readSocket) `shouldReturn` Right []

    it "should return one object when using multi object parser and providing incremental data" $
      let writeSocket s = do
            _ <- NS.send s "12"
            threadDelay 100000
            NS.send s "34\n"
          readSocket s  = runErrorT $ do
            (p, xs1) <- Atto.parseMany s (Atto.parse numberParser) (Atto.parse numberParser)
            (_, xs2) <- Atto.parseMany s (Atto.parse numberParser) p
            return (xs1 ++ xs2)
          numberParser  = do
            num <- decimal
            endOfLine
            return num

      in (pairSockets writeSocket readSocket) `shouldReturn` Right [1234]
