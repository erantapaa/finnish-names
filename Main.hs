
{- Program to scrape names from an HTML file - see the file Anna.html.

A typical paragraph containing a name and dates:

A surname entry - indicated by the the <b> tag:

<p class=Default><b><span ...>Halter </span></b><span ...>Frank
J.,708......................................... <o:p></o:p></span></p>

A normal entry - no <b> tag:

<p class=Default ...><span ...>Mervi Katriina, s. 1970,3016...................... <o:p></o:p></span></p>

N.B. The last paragraph has a class of "MsoNormal"

The basic flow of the app is:

  1. read in the html
  2. convert to a list of tags
  3. for each wanted paragraph, create an Entry value
  4. convert each Entry to an Entry' value
  5. format each Entry' value

-}

import Text.HTML.TagSoup
import System.IO
import Data.Char
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (isPrefixOf, intercalate)
import Data.Maybe
import Text.Printf
import Control.Monad.Trans.State.Strict

data Entry = Bold String String | Normal String
  deriving (Show)

data NameType = Bold' String | Normal'
  deriving (Show)

data Entry' = Entry' { surNname_ :: NameType, gname_ :: String, syear_ :: String, pages_ :: [String] }
  deriving (Show)

-- read a text file using the latin1 encoding
readLatin1 path = do
  handle <- openFile path ReadMode
  hSetEncoding handle latin1  -- or whatever applies
  contents <- hGetContents handle
  print ("content length:", length contents)
  return contents

isBoldOrText (TagText _) = True
isBoldOrText (TagOpen name _) = name == "b"
isBoldOrText (TagClose name) = name == "b"
isBoldOrText _ = False

text tags = concat $ [ x | TagText x <- tags ]

closing tname tag = tag == TagClose tname

-- <b>{text}</b>{text}
-- {text}
-- other
processParagraph tags =
  let step1 = filter isBoldOrText tags
      (mbold,step2) = extractBold step1
      text2 = fixupText $ text step2
  in
  case mbold of
    Nothing   -> Normal text2
    Just bold -> Bold (fixupText bold) text2

-- extract the <b>...</b> part of a paragraph
extractBold tags = do
  case tags of
    (TagOpen "b" _: t0) ->
      let (t1, t2) = break (closing "b") t0
      in (Just (text t1), t2)
    _ -> (Nothing, tags)

-- predicate to match the interesting paragraphs
-- can also use `bi` from Control.Concatenative
wantedParagraph t = (t ~== "<p class=Default>") || (t ~== "<p class=MsoNormal>")

processHtml path = do
  tags <- fmap parseTags $ readLatin1 path
  let paragraphs = partitions wantedParagraph tags
      entries = map processParagraph paragraphs
  return $ entries

-- some string munging functions
removeTrailingJunk :: String -> String
removeTrailingJunk s = reverse $ go (reverse s)
  where
    go [] = []
    go (c:cs)
      | isSpace(c) || c == '.' = go cs
      | otherwise              = (c:cs)

replaceSpaces s = go s
  where
    go [] = []
    go (c:cs) | isSpace c = ' ': go cs
              | otherwise = c : go cs

dedupSpaces s = go s
  where
    go [] = []
    go (' ':cs) = ' ' : go (dropWhile isSpace cs)
    go (c:cs)   = c : go cs

fixupText = dedupSpaces . removeTrailingJunk . replaceSpaces

-- Second stage process of entries.

isSYear cs = " s. " `isPrefixOf` cs

processEntry :: Entry -> Maybe Entry'
processEntry e =
  case e of
    Bold a b -> go (Bold' a) b
    Normal b -> go Normal' b
  where
    go n b = case splitOn "," b of
      (f1 : f2 : rest) | isSYear f2 -> Just $ Entry' n f1 f2 rest
      (f1 : rest) -> Just $ Entry' n f1 "" rest
      _           -> Nothing

formatRow :: String -> Entry' -> String
formatRow surName e =
  printf "%-10s | %s | %s | %s" (syear_ e) surName (gname_ e) (intercalate " | " (pages_ e))

formatEntries :: [Entry'] -> [String]
formatEntries entries = evalState doit "---"
  where
    doit = forM entries $ \e -> do
             case surNname_ e of
               Bold' x -> put x
               _       -> return ()
             fn <- get
             return $ formatRow fn e

main = do
  entries0 <- processHtml "Anna.htm"
  let n = length entries0
      entries = drop 0 entries0
  let entries' = catMaybes $ map processEntry entries
  forM_ (formatEntries entries') $ putStrLn

