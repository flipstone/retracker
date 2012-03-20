{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'app' function is the initializer that combines everything together and
is exported by this module.

-}

module Site
  ( app
  ) where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (toChunks)
import qualified Data.ByteString.Lazy as LB
import           Data.Char (isSpace)
import           Data.Int (Int64)
import           Data.Lens.Lazy
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Network.HTTP.Enumerator
import qualified Network.HTTP.Enumerator as HTTP
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.BackgroundQueue hiding (Handler)
import qualified Snap.Snaplet.BackgroundQueue as BG
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Stats
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)

import           Application


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Adds to the queue
retrack :: Handler App App ()
retrack = ifTop $ do
  body <- (readRequestBody (1024*1042::Int64))
  queueInBackground (B.concat (toChunks body))

  incrementStat "Posts Received"

  writeBS "Got it!\n"

------------------------------------------------------------------------------
-- | Posts request body to a site
forwardRequest :: String -> ByteString -> BG.Handler App App ()
forwardRequest url requestBody = do
  urlRequest <- liftIO $ parseUrl url

  let fullRequest = urlRequest {
                      HTTP.method = "POST"
                    , HTTP.requestBody = (RequestBodyBS requestBody)
                    , HTTP.requestHeaders = [("Content-Type", "text/xml")]
                    }

  response <- liftIO $ withManager (httpLbs fullRequest)

  if (statusCode response) >= 200 && (statusCode response) < 300
    then do
          incrementStat "Posts Forwarded"
          liftIO $ putStrLn ("Successfully forwarded request to " ++ url)
    else do
          incrementStat "Forwarding Errors"
          liftIO $ do
            putStrLn (url ++ " returned code " ++ (show $ statusCode response))
            putStrLn "Response was: "
            LB.putStrLn (HTTP.responseBody response)

  return ()

------------------------------------------------------------------------------
-- | Read the destintations
readDestinations :: Initializer App App [ByteString -> BG.Handler App App ()]
readDestinations = do
    config <- liftIO $ readFile "resources/config/destinations"
    return $ catMaybes (map mkForwarder (lines config))
  where
    mkForwarder line | isBlank line = Nothing
    mkForwarder line = Just (forwardRequest (trim line))

    isBlank = all isSpace
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",            index)
         , ("/retrack",     retrack)
         , ("", with heist heistServe)
         , ("", serveDirectory "resources/static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    destinations <- readDestinations

    liftIO $ putStrLn ("Configuring Retracker with " ++ (show $ length destinations) ++ " destinations")

    h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    bq <- nestSnaplet "backroundQueue" backgroundQueue $ backgroundQueueInit destinations
    stats <- nestSnaplet "stats" stats $ statsInit

    initStatValue "Posts Received" 0
    initStatValue "Posts Forwarded" 0
    initStatValue "Forwarding Errors" 0

    addRoutes routes
    return $ App h bq stats


