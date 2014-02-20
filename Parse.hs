-- A script to parse binary tcpdump files -> text and from text -> json
-- using qlen by Nicolas Pouillard

import System.Environment
import System.IO
import Text.Regex.Posix
import Data.List


--regex = "([0-9:]{8}).+(-[0-9]{1,3}db).+antenna ([0-9]).+SA:([0-9a-f:]{17})"

-- Regular expressions
regexTime   = "[0-9]{2}:[0-9]{2}:[0-9]{2}"
regexSignal = "(\\-[0-9]{1,3}dB)"
regexMac = "SA:([0-9a-f:]{17})"

parseMac    line = line =~ regexMac :: String
parseSignal line = line =~ regexSignal :: String
parseTime   line = line =~ regexTime :: String

parseLine :: String -> [String]
parseLine line = 
  case (mac) of
    ""  -> []
    mac -> [parseTime line, parseSignal line, mac]
  where 
    mac = parseMac line

main :: IO()
main = do 
  args <- getArgs
  content <- readFile $ args !! 0
  let iterator = lines $ content
  print $ filter (not . null) $ map parseLine iterator