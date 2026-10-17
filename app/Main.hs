-- | Copyright 2026 kakkak444
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where


import Data.List             (find)
import Data.Maybe            (fromMaybe)
import System.Console.GetOpt
import System.Environment    (getArgs)
import Streaming.Network.TCP qualified as NS

import SkkServer             (skkserver)


data Flag
    = Port !Int
    | Host !String
    deriving Show

defaultPort :: Flag
defaultPort = Port 1178

defaultHost :: Flag
defaultHost = Host "127.0.0.1"

options :: [OptDescr Flag]
options =
    [ Option ['p'] ["port"] (ReqArg (Port . read) "PORT") "PORT which listen on"
    , Option ['h'] ["host"] (ReqArg  Host         "HOST") "HOST which binds to"
    ]

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv =
    case getOpt Permute options argv of
        (o, n, [])   -> return (o, n)
        (_, _, errs) -> ioError (userError $ concat errs <> usageInfo header options)
  where
    header = "Usage:"

getPort :: [Flag] -> Maybe Flag
getPort = find (\case Port _ -> True ; _ -> False)

getHost :: [Flag] -> Maybe Flag
getHost = find (\case Host _ -> True ; _ -> False)

main :: IO ()
main = do
    argv <- getArgs

    opts <- fst <$> parseOptions argv
    let Port port = fromMaybe defaultPort $ getPort opts
        Host host = fromMaybe defaultHost $ getHost opts

    putStrLn $ "server started on " <> host <> ":" <> show port

    NS.serve (NS.Host host) (show port) (skkserver . fst)
