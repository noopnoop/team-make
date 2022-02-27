module Util where

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

badChar :: [Char]
badChar = [' ', '\"', '\n', '\r', '\\', ',']

removeBadChars :: String -> String
removeBadChars = filter (`notElem` badChar)

safeHead :: e -> [a] -> Either e a
safeHead err []      = Left err
safeHead _   (x : _) = Right x

-- >>> dropUntil (==3) [1,2,3,4,5]
-- [4,5]
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ []       = []
dropUntil f (x : xs) = 
  if f x 
    then xs 
    else dropUntil f xs
