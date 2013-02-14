module Main where

import qualified System.Environment as ENV (getArgs)
import qualified Habolous.Routes as FHR (serve)
import qualified Habolous.Types.Config as FTC (parseConfig)
-- import qualified Paths_habolous as PF (getDataDir)

main :: IO ()
main = do
    args <- ENV.getArgs
    case length args of
        1 -> do cf <- FTC.parseConfig $ head args
                case cf of
                    Right c -> FHR.serve c
                    Left e -> print e
        _ -> putStrLn "Usage: habolous /path/to/configuration.conf"
