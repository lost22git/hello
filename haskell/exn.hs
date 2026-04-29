import Text.Printf
import System.IO
import Control.Exception
import Control.DeepSeq

main = do
  test_handle
  test_catch
  test_try


test_file_io path = 
  withFile path ReadMode $ \file -> do
  content <- hGetContents file
  return $! force content

-- handle
test_handle = handle
  (\exn -> printf "Error: %s\n" $ show (exn :: IOException))
  $ test_file_io "README2.md" >>= \content -> do
    putStrLn "README.md content:"
    putStrLn content

-- catch
test_catch = catch
  (test_file_io "README2.md" >>= \content -> do
    putStrLn "README.md content:"
    putStrLn content)
  (\exn -> printf "Error: %s\n" $ show (exn :: IOException))
  
-- try
test_try = do
  result <- try (test_file_io "README2.md")
  case result of 
    Left exn -> printf "Error: %s\n" $ show (exn :: IOException)
    Right content -> do
      putStrLn "README.md content:"
      putStrLn content


