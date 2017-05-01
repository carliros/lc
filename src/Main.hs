module Main where

import           IdInt
import           Lambda
--import           Simple
import           Unique

import           System.Console.Haskeline
import           System.Environment
import           System.IO

import           Control.Monad
import           Control.Monad.Trans


main :: IO ()
main = runInputT defaultSettings loop
  where loop = do minput <- getInputLine "lc> "
                  case minput of
                    Nothing -> loop
                    Just str -> case str of
                                  ""   -> loop
                                  ":q" -> outputStrLn "Goodbye."
                                  _    -> (liftIO $ process str) >> loop

        process :: String -> IO ()
        process input = do let expr = (read input) :: LC Id
                           let result = normalForm (toIdInt expr)
                           print result
