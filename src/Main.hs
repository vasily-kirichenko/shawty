{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe             (isJust)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as TL
import qualified Database.Redis         as R
import           Network.URI            (URI, parseURI)
import qualified System.Random          as SR
import           Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return $ xs !! randomDigit

shortyGen :: IO String
shortyGen = replicateM 7 (randomElement alphaNum)

type ShortURI = BC.ByteString
type FullURI = BC.ByteString

saveURI :: Reader R.Connection (ShortURI -> FullURI) IO (Either R.Reply R.Status))
saveURI shortURI uri = R.runRedis $ R.set shortURI uri

getURI :: Reader R.Connection ShortURI (IO (Either R.Reply (Maybe FullURI)))
getURI shortURI = R.runRedis $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat ["<a href=\"", shorty, "\">Copy and paste your short URL</a>"]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty) ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri, " wasn't a url, did you forget http://?"]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs = TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app :: ReaderT R.Connection ScottyM ()
app = do

  get "/" $ do
    uri <- param "uri"
    let parsedUri = parseURI . TL.unpack $ uri
    if isJust parsedUri then do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
        existingUri <- liftIO (getURI shorty)
        case existingUri of
          Left reply -> text . TL.pack . show $ reply
          Right (Just shortURI) ->
            text . TL.pack $
              "Short URI " ++
              BC.unpack shortURI ++
              " has already been used."
          Right _ -> do
            let uri' = encodeUtf8 (TL.toStrict uri)
            resp <- liftIO (saveURI shorty uri')
            html (shortyCreated resp shawty)
    else text (shortyAintUri uri)

  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI short)
    case uri of
      Left reply -> text . TL.pack . show $ reply
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)
          where
            tbs = TL.fromStrict (decodeUtf8 bs)



main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
