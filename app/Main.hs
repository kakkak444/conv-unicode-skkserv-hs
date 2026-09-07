{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where


import Control.Concurrent         (forkFinally)
import Control.Exception.Safe                     qualified as E
import Control.Monad              (forever, void)
import Data.ByteString.Char8                      qualified as BS8
import Data.ByteString.Conversion                 qualified as BS8
import Data.ByteString.UTF8       (fromChar)
import Data.Char                  (chr)
import Data.Coerce
import Data.List                  (find)
import Data.List.NonEmpty                         qualified as NE
import Data.Maybe                 (fromMaybe)
import Data.Monoid
import Network.Socket                                              hiding (defaultPort)
import Network.Socket.ByteString
import System.Console.GetOpt
import System.Environment         (getArgs)


data Flag
    = Port !Int
    deriving Show

defaultPort :: Flag
defaultPort = Port 3000

options :: [OptDescr Flag]
options =
    [ Option ['p'] ["port"] (ReqArg (Port . read) "PORT") "PORT which listen on"
    ]

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv =
    case getOpt Permute options argv of
        (o, n, [])   -> return (o, n)
        (_, _, errs) -> ioError (userError $ concat errs <> usageInfo header options)
  where
    header = "Usage:"

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        NE.head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)

getPort :: [Flag] -> Maybe Flag
getPort = find (\case Port _ -> True)

skkserver :: Socket -> IO ()
skkserver sock = do
    bs <- recv sock 1024

    let (opecode, operand) = BS8.splitAt 1 $ BS8.strip bs
        fallback = sendAll sock ("4" <> operand <> " \n")
    res <- E.try $ case opecode of
        "0" -> E.throwString ""
        "1" -> case getAlt $ mconcat $ coerce
                [ BS8.stripPrefix "U+" operand
                , BS8.stripPrefix "u+" operand
                , BS8.stripPrefix "U"  operand
                , BS8.stripPrefix "u"  operand
                ] of
            Nothing -> fallback
            Just codepoint ->
                case BS8.fromByteString @(BS8.Hex Int) codepoint of
                    Just (BS8.Hex codepoint')
                        | isValidUnicodeCodepoint codepoint' ->
                            sendAll sock $ "1/" <> fromChar (chr codepoint') <> "/\n"
                    _ -> fallback
        "2" -> sendAll sock $ versionInfo <> " \n"
        "3" -> sendAll sock $ "novalue: \n"
        _ -> sendAll sock bs

    case res of
        Left (E.StringException _ _) -> return ()
        Right () -> continue
  where
    versionInfo = "conv-unicode-skkserv-0.1.0.0"
    isValidUnicodeCodepoint i = 0x0000 <= i && i <= 0x10FFFF
    continue = skkserver sock

main :: IO ()
main = do
    argv <- getArgs
    opts <- fst <$> parseOptions argv
    let Port port = fromMaybe defaultPort $ getPort opts
    runTCPServer (Just "localhost") (show port) skkserver
