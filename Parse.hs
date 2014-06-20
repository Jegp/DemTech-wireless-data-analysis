-- A script to parse binary tcpdump files -> text

import System.Environment
import System.Exit
import System.IO.Unsafe
import Text.Regex.Posix
import Data.List
import qualified Data.Map as Map
import Data.Unique.Id
import Control.Arrow

-- Regular expressions
regexTime   = "[0-9]{2}:[0-9]{2}:[0-9]{2}"
regexSignal = "(\\-[0-9]{1,3}dB)"
-- SA (source address) and TA (transmitter address)
regexMac    = "SA:([0-9a-f:]{17})"

parseMac    line = line =~ regexMac :: String
parseSignal line = line =~ regexSignal :: String
parseTime   line = line =~ regexTime :: String

-- Unique ID supply
idSupply = unsafePerformIO(initIdSupply 'i')

-- Map of MAC addresses
--MACs = Map.empty

-- Parses a line from a tcpdump file to a list of relevant fields
parseLine :: String -> [String]
parseLine line = 
  case (mac) of
    ""  -> []
    mac -> [parseTime line, parseSignal line, mac]
  where 
    mac = parseMac line

-- 
parse :: [String] -> IO String
parse files = fmap concat $ mapM readFile files
--"3" -- $ hashedId $ idFromSupply idSupply

sanitise :: String -> String
sanitise input = show $ map unwords $ map parseLine $ lines input

main :: IO()
main = getArgs >>= parse >>= putStr . sanitise
--  args <- getArgs
--  content <- readFile $ args !! 0
--  let iterator = lines content
--  let router = args !! 1
--  putStr $ unlines $ filter (not . null) $ map unwords $ map parseLine iterator