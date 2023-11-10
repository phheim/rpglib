module Main where

import RPGS.Writer
import RPGS.Parser

main :: IO ()
main = do
    input <- getContents
    case parseGame input of
        Left err -> fail err
        Right (game, wc)  -> putStrLn (writeGame (game, wc))
