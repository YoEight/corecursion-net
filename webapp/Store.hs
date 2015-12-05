module Store (streamFold) where

------------------------------------------------------------------------------
import Prelude
import Control.Monad

-------------------------------------------------------------------------------
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time hiding (parseTime)
import Database.EventStore

------------------------------------------------------------------------------
parseTime :: ByteString -> Maybe UTCTime
parseTime bs =
    case parseOnly action bs of
      Left _  -> Nothing
      Right a -> Just a
  where
    action = do
        v <- value
        case fromJSON v of
          Error e   -> fail e
          Success a -> return a

-----------------------------------------------------------------------------
-- | Left fold a stream of events from the begining given a state seed.
streamFold :: FromJSON a
           => Connection
           -> Text
           -> (s -> a -> Maybe UTCTime -> IO s)
           -> s
           -> IO s
streamFold conn stream k seed = go 0 seed
  where
    go start s = do
        act <- readStreamEventsForward conn stream start 500 False
        res <- wait act
        case res of
            ReadSuccess sl -> do
                let foldF ss revt =
                        let action = do
                                 let evt = resolvedEventOriginal revt
                                 meta <- recordedEventMetadata evt
                                 e    <- recordedEventDataAsJson evt
                                 return (parseTime meta, e) in
                        case action of
                            Nothing        -> return ss
                            Just (date, e) -> k ss e date
                    next = sliceNext sl
                newS <- foldM foldF s $ sliceEvents sl
                if sliceEOS sl
                    then return newS
                    else go next newS
            ReadNoStream -> return s
            e            -> fail $ "wrong slice read result" ++ show e
