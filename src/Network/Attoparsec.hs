{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : network-attoparsec
Description : Utility functions for running a parser against a socket
Copyright   : (c) Leon Mergen, 2015
License     : MIT
Maintainer  : leon@solatis.com
Stability   : experimental

Utility functions for running a parser against a socket, without the need of a
bigger framework such as Pipes or Conduit.

__WARNING__: In certain situations while using the attoparsec string parser, it
             is possible that a network parser ends in a forever-blocking state,
             expecting more input. This is a side effect of the way attoparsec
             is written. I have written a thorough explanation on this issue, and
             <http://www.leonmergen.com/haskell/attoparsec/2015/03/15/on-writing-non-blocking-parsers-for-attoparsec.html how to avoid attoparsec returning a partial result> when a different branch should be evaluated.

-}

module Network.Attoparsec (ParseC, parseMany, parseOne) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Exception.Enclosed (tryAny)

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as BS
import qualified Network.Socket             as NS
import qualified Network.Socket.ByteString  as NSB

-- | The parsing continuation form of a "Data.Attoparsec" parser. This is
--   typically created by running the attoparsec "parse" function:
--
--   > createParser = AttoParsec.parse myParser
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
--   >   (_,  xs2) <- parseMany sock (AttoParsec.parse myParser) p1
--   >   return (xs1 ++ xs2)
--
--   For more usage examples, see the test directory.
parseMany :: ( MonadIO m
             , MonadMask m
             , Show a)
          => NS.Socket         -- ^ Socket to read data from
          -> ParseC a          -- ^ Initial parser state
          -> ParseC a          -- ^ Continuation parser state
          -> m (ParseC a, [a]) -- ^ Next parser state with parsed values
parseMany s p0 pCur = do
  buf <- readAvailable s Nothing
  (p1, xs) <- parseBuffer p0 Many buf pCur
  return (p1, xs)

-- | Similar to parseMany, but assumes that there will only be enough data for a
--   single succesful parse on the socket, and guarantees that exactly one item
--   will be parsed.
--
--   __Warning:__ In order to make this function work stable with pipelined data,
--                we read in data one byte at a time, which causes many context
--                switches and kernel syscalls, and furthermore causes a lot of
--                separate calls to attoparsec. So only use if performance is not
--                a consideration.
--
--  The is typically used as follows:
--
--  > doParse sock = parseOne sock (AttoParsec.parse myParser)
parseOne :: ( MonadIO m
            , MonadMask m
            , Show a)
         => NS.Socket -- ^ Socket to read data from
         -> ParseC a  -- ^ Initial parser state
         -> m a       -- ^ Parsed value
parseOne s p0 = do
  buf <- readAvailable s (Just 1)
  (p1, value) <- parseBuffer p0 Single buf p0

  case value of
   -- We do not yet have enough data for a single item, let's request more
   []  -> parseOne s p1
   [x] -> return x

   -- This is an internal error, since it means our single-object parser
   -- returned multiple objects.
   _   -> error "More than one element parsed"

parseBuffer :: ( MonadIO m
               , MonadMask m
               , Show a)
            => ParseC a          -- ^ Initial parser state
            -> ParseMode         -- ^ Whether to perform greedy or non-greedy parsing
            -> BS.ByteString     -- ^ Unconsumed buffer from previous run
            -> ParseC a          -- ^ Current parser state
            -> m (ParseC a, [a]) -- ^ Next parser state with parsed values
parseBuffer p0 mode =


  let next bCur pCur =
        case pCur bCur of
         -- On error, throw error through MonadError
         Atto.Fail    err _ _  -> fail ("An error occurred while parsing: " ++ show err)
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
                   Single -> fail ("Unconsumed data left on socket: " ++ show b1)

                   -- Multi-object parsing mode, in which case we will enter
                   -- recursion.
                   Many   -> do
                     (p1, xs) <- next b1 p0
                     return (p1, v : xs)

  in next

readAvailable :: ( MonadIO m
                 , MonadMask m)
              => NS.Socket
              -> Maybe Int
              -> m BS.ByteString
readAvailable s Nothing = readAvailable s (Just 2048)
readAvailable s (Just bytes) =
  let  buf :: IO (Maybe BS.ByteString)
       buf    = do
        -- For some reason, Windows seems to be generating an exception sometimes
        -- when the remote has closed the connection
        result <- tryAny $ NSB.recv s bytes

        case result of
         Left _  -> return Nothing
         Right v -> return (Just v)

  in maybe (return BS.empty) return =<< liftIO buf
