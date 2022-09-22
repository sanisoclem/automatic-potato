module AP.Data.Log
  ( LogLevel(..)
  , message
  , level
  , Log
  , mkLog
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime)

data LogLevel = Debug | Info | Warn | Error

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

newtype Log = Log
  { level :: LogLevel
  , timestamp :: DateTime
  , message :: String
  }

derive instance eqLog :: Eq Log

message :: Log -> String
message (Log { message: m }) = m

level :: Log -> LogLevel
level (Log { level: r }) = r

mkLog :: DateTime -> LogLevel -> String -> Log
mkLog now logLevel inputMessage =
  Log { level: logLevel, timestamp: now, message: formattedLog }
  where
    -- Will format "2018-10-25 11:25:29 AM"
    formatTimestamp =
      either (const "(Failed to assign time)") identity
        <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"

    -- Will produce a header like "{DEBUG: 2018-10-25 11:25:29 AM]\nMessage contents..."
    headerWith start =
      fold [ "[", start, ": ", formatTimestamp now, "]\n", inputMessage ]

    -- Writes the header with the correct log reason
    formattedLog = headerWith case logLevel of
      Debug -> "DEBUG"
      Info -> "INFO"
      Warn -> "WARNING"
      Error -> "ERROR"
