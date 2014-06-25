-- A script to parse binary tcpdump files -> text

import System.Environment
import System.Exit
import System.IO.Unsafe
import Text.Regex.Posix
import Data.List
import Control.Arrow
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as S
import qualified Crypto.Hash.SHA256 as H
import qualified Data.Text as T

-- Regular expressions
regexTime   = "[0-9]{2}:[0-9]{2}:[0-9]{2}"
regexSignal = "(\\-[0-9]{1,3}dB)"
-- SA (source address) and TA (transmitter address)
regexMac    = "SA:([0-9a-f:]{17})"

parseMac    line = line =~ regexMac :: String
parseSignal line = line =~ regexSignal :: String
parseTime   line = line =~ regexTime :: String

-- Anonymises a MAC address by hashing it
anonymise :: String -> String -> String
anonymise key mac =
  take 8 $ S.unpack $ B16.encode $ H.hash $ S.pack $ key ++ mac

-- Parses a line from a tcpdump file to a list of relevant fields
parseLine :: String -> String -> String
parseLine key line =
  case (mac) of
    ""  -> ""
    mac -> let hash = anonymise key mac in
           unwords [parseTime line, parseSignal line, hash]
  where
    mac = parseMac line

parse :: [String] -> IO String
parse files = fmap concat $ mapM readFile files

sanitise :: String -> String -> String
sanitise key input = unlines $ map (parseLine key) $ lines input

main :: IO()
main = getArgs >>= parse >>= putStr . sanitise "Key"
--  args <- getArgs
--  content <- readFile $ args !! 0
--  let iterator = lines content
--  let router = args !! 1
--  putStr $ unlines $ filter (not . null) $ map unwords $ map parseLine iterator