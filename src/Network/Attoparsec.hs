{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for running a parser against a socket, without the need
--   of a bigger framework such as Pipes or Conduit.

module Network.Attoparsec (parseMany, parseOne) where

import           Control.Monad.Error

import qualified Data.ByteString            as BS
import qualified Network.Socket             as NS
import qualified Network.Socket.ByteString  as NSB
import qualified Data.Attoparsec.ByteString as Atto

-- | The parsing continuation form of a "Data.Attoparsec" parser. This is
--   typically created by running the attoparsec "parse" function:
--
--   @
--     createParser = AttoParsec.parse myParser
--   @
type ParseC a = BS.ByteString -> Atto.Result a

-- | The type of parsing to perform, greedy or non-greedy
data ParseMode = Single | Many

-- | Consumes input from socket and attempts to parse as many objects from the
--   socket as possible. Use this function only when you expect more than one
--   parse operation to succeed.
--
--   The function is continuation based, so you must provide the next parser
--   state in successing calls as follows:
--
--   > doParse sock = do
--   >   (p1, xs1) <- parseMany sock (AttoParsec.parse myParser) (AttoParsec.parse myParser)
--   >   (_,  xs2) <- parseMany sock (AttoParsec.parse myPArser) p1
--   >   return (xs1 ++ xs2)
--
--   For more usage examples, see the test directory.
parseMany :: ( MonadIO m
             , MonadError String m)
          => NS.Socket         -- ^ Socket to read data from
          -> ParseC a          -- ^ Initial parser state
          -> ParseC a          -- ^ Continuation parser state
          -> m (ParseC a, [a]) -- ^ Next parser state with parsed values
parseMany s p0 pCur = do
  buf <- liftIO $ NSB.recv s 4096
  (p1, xs) <- parseBuffer p0 Many buf pCur
  return (p1, xs)

-- | Similar to parseMany, but assumes that there will only be enough data for a
--   single succesful parse on the socket, and guarantees that exactly one item
--   will be parsed.
--
--   __Warning:__ this function will /not/ work correctly when input data is
--   pipelined. The parser might consume more data than required from the socket,
--   or a partial second object is parsed, and the parser state and buffer will
--   be discarded.
--
--  The is typically used as follows:
--
--  > doParse sock = parseOne sock (AttoParsec.parse myParser)
parseOne :: ( MonadIO m
            , MonadError String m)
         => NS.Socket -- ^ Socket to read data from
         -> ParseC a  -- ^ Initial parser state
         -> m a       -- ^ Parsed value
parseOne s p0 = do
  buf <- liftIO $ NSB.recv s 4096
  (p1, value) <- parseBuffer p0 Single buf p0

  case value of
   -- We do not yet have enough data for a single item, let's request more
   []  -> parseOne s p1
   [x] -> return x

   -- This is an internal error, since it means our single-object parser
   -- returned multiple objects.
   _   -> error "More than one element parsed"

parseBuffer :: ( MonadIO m
               , MonadError String m)
            => ParseC a          -- ^ Initial parser state
            -> ParseMode         -- ^ Whether to perform greedy or non-greedy parsing
            -> BS.ByteString     -- ^ Unconsumed buffer from previous run
            -> ParseC a          -- ^ Current parser state
            -> m (ParseC a, [a]) -- ^ Next parser state with parsed values
parseBuffer p0 mode =

  let next bCur pCur =
        case pCur bCur of
         -- On error, throw error through MonadError
         Atto.Fail    err _ _  -> throwError ("An error occured while parsing input: " ++ show err)
         Atto.Partial p1       -> return (p1, [])
         Atto.Done    b1 v     ->
           if BS.null b1
             -- This means a "perfect parse" occured: exactly enough data was on
             -- the socket to complete one parse round.
             then return (p0, [v])
             else case mode of
                   -- We are in single-object parsing mode, have parsed one object,
                   -- but still have data left on the buffer: at this point, we can
                   -- either discard the data on the buffer, or throw an error.
                   --
                   -- We throw an error, since within "single-object parsing mode"
                   -- we assume only perfect parses happen.
                   Single -> throwError ("Unconsumed data left on socket: " ++ show b1)

                   -- Multi-object parsing mode, in which case we will enter
                   -- recursion.
                   Many   -> do
                     (p1, xs) <- next b1 p0
                     return (p1, v : xs)

  in next
