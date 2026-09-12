{- | This server returns utf-8 encoded strings.
     The protocol of SKK server is based on:
     * https://github.com/jj1bdx/dbskkd-cdb/blob/050c9f9af8ba57ff928d3744f1d28f0d89da032e/skk-server-protocol.md
     * https://github.com/tomykaira/mskkserv/blob/59d361e436af65719f15dfff07111b32556f3f6f/README.md
     * https://github.com/wachikun/yaskkserv2/blob/f5bc4590c798c591e9861e02ea2e12d227a047ed/README.md
 -}

{-# LANGUAGE OverloadedStrings #-}

module SkkServer (
        skkserver
    ) where


import Control.Applicative                  (optional)
import Data.Attoparsec.ByteString           qualified as A   hiding (takeTill)
import Data.Attoparsec.ByteString.Char8     qualified as A
import Data.Attoparsec.ByteString.Streaming qualified as S
import Data.ByteString                      qualified as BS
import Data.ByteString.UTF8                 (fromChar)
import Data.Char                            (chr)
import Data.Function                        ((&))
import Data.String                          (IsString, fromString)
import Data.String.UTF8                     (toRep)
import Streaming                            qualified as S
import Streaming.ByteString                 qualified as S
import Streaming.Network.TCP                (Socket, fromSocket, toSocket)
import Streaming.Prelude                    qualified as SP


data Vocabulary
    = End
    | Request BS.ByteString
    | Version
    | Host
    deriving (Show)

parser :: A.Parser Vocabulary
parser = A.choice [ End     <$  (A.char '0' *>                           A.many1 (A.char ' ') <* optional (A.char '\n'))
                  , Request <$> (A.char '1' *> A.takeTill (A.isSpace) <* A.many1 (A.char ' ') <* optional (A.char '\n'))
                  , Version <$  (A.char '2' *>                           A.many1 (A.char ' ') <* optional (A.char '\n'))
                  , Host    <$  (A.char '3' *>                           A.many1 (A.char ' ') <* optional (A.char '\n'))
                  ]

versionInfo :: IsString a => a
versionInfo = fromString "conv-unicode-skkserv-0.1.0.0"

hostName :: IsString a => a
hostName = fromString "novalue:"

-- >>> A.parseOnly codepointParser "u+00ff"
-- Right 255
-- >>> A.parseOnly codepointParser "u00ff"
-- Right 255
codepointParser :: A.Parser Int
codepointParser = A.choice [A.char 'u', A.char 'U'] *> optional (A.char '+') *> A.hexadecimal

-- >>> codepointToChar "u+301c"
-- Right '\12316'
-- >>> codepointToChar "awoief"
-- Left "awoief"
codepointToChar :: BS.ByteString -> Either BS.ByteString Char
codepointToChar bs =
    case A.parseOnly codepointParser bs of
        Right codepoint
            | isValidUnicodeCodepoint codepoint
                -> Right $ chr codepoint
        _ -> Left bs
  where
    isValidUnicodeCodepoint i = 0x0000 <= i && i <= 0x10FFFF

interpret :: Vocabulary -> IO r -> (BS.ByteString -> IO r) -> IO r
interpret End left _ = left
interpret (Request bs) _ right = case codepointToChar bs of
    Left  invalid -> right $ "4"  <> invalid       <> " \n"
    Right char    -> right $ "1/" <> fromChar char <> "/\n"
interpret Version _ right = right $ toRep versionInfo <> " \n"
interpret Host    _ right = right $ toRep hostName    <> " \n"

skkserver :: Socket -> IO ()
{-# INLINABLE skkserver #-}
skkserver sock
    = fromSocket sock 4096
    & S.parsed parser
    & SP.cycle -- for ignoring failures of parsing
    & SP.foldrM ( \x acc ->
        interpret x (return ())
                    (\a -> toSocket sock (S.fromStrict a) >> acc) )
    & S.void
