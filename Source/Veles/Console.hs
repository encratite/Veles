module Console(
  consoleString
  ) where

-- | This function is used to print erroneous strings provided by users of the server.
-- For one, it is supposed to escape unprintable characters.
-- Furthermore it trims the string if necessary so the console can't easily be flooded by single large malicious requests.
consoleString :: String -> String
consoleString string =
  let lengthLimit = 64 in
  show if length string > lengthLimit
       then take lengthLimit string ++ "..."
       else string