module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


lookUp :: String -> [(String, a)] -> [a]
lookUp a xs = [c | (b, c) <- xs, b == a ]

split :: [Char] -> String -> (String, [String])
split sep []
  = ("", [""])
split sep (x:xs)
  | elem x sep = (x:word, "":(s:ss))
  | otherwise  = (word, (x:s):ss)
  where
    (word, (s:ss))= split sep xs

combine :: String -> [String] -> [String]
combine "" xs
  = xs
combine (s:ss) (w:ws)
  = w:[s]:combine ss ws

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs []
  = []
getKeywordDefs (x:xs)
  = (y,unwords ys):getKeywordDefs xs
  where
    y:ys = snd (split " " x)

--1. Split original text into components and separators
--2. Find words in components start with $ and replace them with keywords with getKeywordDefs
--3. Combine separators with the new components
expand :: FileContents -> FileContents -> FileContents
expand [] _
  = []
expand text keywords
  = concat (combine sep newText)
   where
     (sep, ss) = split separators text -- ts is the list of strings of Text
     (_, ws)   = split "\n" keywords
     defs      = getKeywordDefs ws
     newText   = replaceWord ss defs

-- helper function 
replaceWord :: [String] -> KeywordDefs -> [String]
replaceWord [] _
  = []
replaceWord (x:xs) defs
  | x == ""       = x : y
  | head x == '$' = head (lookUp x defs): y
  | otherwise     = x : y
  where y = replaceWord xs defs

main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
