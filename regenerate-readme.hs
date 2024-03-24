import System.IO


data Section
  = TextSection [String]
  | CodeSection [String]
  deriving (Show)

splitIntoSections
  :: [String]
  -> [Section]
splitIntoSections = go Nothing
  where
    go Nothing []
      = []
    go (Just (TextSection xs)) []
      = [TextSection (reverse xs)]
    go (Just (CodeSection xs)) []
      = [CodeSection (reverse xs)]
    go Nothing ("":rest)
      = go Nothing rest
    go Nothing (x@('-':'-':' ':'|':_):rest)
      = go (Just (CodeSection [x])) rest
    go Nothing (('-':'-':' ':x):rest)
      = go (Just (TextSection [x])) rest
    go Nothing (x:rest)
      = go (Just (CodeSection [x])) rest
    go (Just (TextSection xs)) (('-':'-':' ':x):rest)
      = go (Just (TextSection (x:xs))) rest
    go (Just (TextSection xs)) ("--":rest)
      = go (Just (TextSection ("":xs))) rest
    go (Just (TextSection xs)) (x:rest)
      = TextSection (reverse xs)
      : go Nothing (x:rest)
    go (Just (CodeSection xs)) ("":x@('-':'-':' ':'|':_):rest)
      = go (Just (CodeSection (x:"":xs))) rest
    go (Just (CodeSection xs)) ("":x@('-':'-':' ':_):rest)
      = CodeSection (reverse xs)
      : go Nothing (x:rest)
    go (Just (CodeSection xs)) (x:rest)
      = go (Just (CodeSection (x:xs))) rest

hPrintSection
  :: Handle
  ->Section
  -> IO ()
hPrintSection h (TextSection xs) = do
  mapM_ (hPutStrLn h) xs
  hPutStrLn h ""
hPrintSection h (CodeSection xs) = do
  hPutStrLn h "```haskell"
  mapM_ (hPutStrLn h) xs
  hPutStrLn h "```"
  hPutStrLn h ""

main :: IO ()
main = do
  input <- lines <$> readFile "src/Demo.hs"
  let readmePortion
        = drop 1
        $ dropWhile (/= "-- README BEGINS")
        $ input
  let sections
        = splitIntoSections readmePortion
  
  -- for debugging
  --mapM_ (hPrintSection stdout) sections

  withFile "README.md" WriteMode $ \h -> do
    mapM_ (hPrintSection h) sections
