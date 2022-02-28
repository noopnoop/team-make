module Util where

badChar :: [Char]
badChar = [' ', '\"', '\n', '\r', '\\', ',']

removeBadChars :: String -> String
removeBadChars = filter (`notElem` badChar)

safeHead :: e -> [a] -> Either e a
safeHead err []      = Left err
safeHead _   (x : _) = Right x
