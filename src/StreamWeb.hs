module StreamWeb where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Streamly.Prelude as SP
import qualified Streamly.Network.Socket as NS
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified StreamWeb.Types as T
import qualified Network.Socket as NS
import AttoParser
import Streamly                  (SerialT, wAsync, serially)
import Streamly.Network.Server   (connectionsOnAllAddrs)
import Network.Socket.ByteString (recv, send)

sockStream :: (NS.Socket -> T.Request -> IO Int) -> NS.PortNumber -> SerialT IO Int
sockStream fn portNumber = SP.concatMapBy wAsync (`NS.withSocketS` (\so -> SP.yieldM (do
                                                                           x <- recv so 4096
                                                                           case AP.parseOnly requestParser x of
                                                                             Right req -> fn so req
                                                                             Left err -> send so (BC.pack err)
                                                                           ))) (serially $ connectionsOnAllAddrs portNumber)

startServer :: (NS.Socket -> T.Request -> IO Int) -> IO ()
startServer = SP.drain . flip sockStream 8081
