module Parser (BF (..), parseBF') where

data BF
  = GoLeft
  | GoRight
  | Inc
  | Dec
  | Write
  | Read
  | Loop [BF]
  deriving (Show, Eq)

parseBF' :: String -> Maybe [BF]
parseBF' s = fst <$> parseBF s 0

prepend :: a -> ([a], b) -> ([a], b)
prepend x (c, xs) = (x : c, xs)

parseBF :: String -> Int -> Maybe ([BF], String)
parseBF "" 0 = Just ([], "")
parseBF "" _ = Nothing
parseBF (x : xs) d = case x of
  '>' -> prepend GoRight <$> parseBF xs d
  '<' -> prepend GoLeft <$> parseBF xs d
  '+' -> prepend Inc <$> parseBF xs d
  '-' -> prepend Dec <$> parseBF xs d
  ',' -> prepend Write <$> parseBF xs d
  '.' -> prepend Read <$> parseBF xs d
  '[' -> do
    (contents, xs') <- parseBF xs (d + 1)
    prepend (Loop contents) <$> parseBF xs' d
  ']' -> if d == 0 then Nothing else Just ([], xs)
  _ -> parseBF xs d

