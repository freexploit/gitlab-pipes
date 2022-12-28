{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where
import Gitlab
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8  as BC
import           GHC.Generics
import           Network.HTTP.Simple
import qualified Network.HTTP.Types     as HT
import           Options.Applicative
import Data.Text

data Args = Args
    { project     :: String
    , serverl_url :: String
    , pToken      :: String
    }

--
--  {
    --"id": 47,
    --"iid": 12,
    --"project_id": 1,
    --"status": "pending",
    --"source": "push",
    --"ref": "new-pipeline",
    --"sha": "a91957a858320c0e17f3a0eca7cfacbff50ea29a",
    --"web_url": "https://example.com/foo/bar/pipelines/47",
    --"created_at": "2016-08-11T11:28:34.085Z",
    --"updated_at": "2016-08-11T11:32:35.169Z"
  --}
--

argParser :: Parser Args
argParser = Args
    <$> strOption
        (long "project" <> metavar "PROJECT_ID" <> help "Gitlab Project ID")
    <*> strOption
        (long "pToken"
        <> metavar "PERSONAL_TOKEN"
        <> help "Personal gitlab token"
        )
    <*> strOption
        (long "server-url"
        <> showDefault
        <> value "gitlab.com"
        <> metavar "SERVER_URL"
        <> help "Gitlab Server URL"
        )

request :: Args -> Request
request (Args project_id'  token server_url ) =
    buildRequest (BC.pack token) (BC.pack server_url)  "GET" ("/api/v4/projects/" <> (BC.pack project_id') <> "/pipelines")


buildRequest :: BC.ByteString -> BC.ByteString ->  BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path = setRequestMethod method
    $ setRequestHost host
    $ setRequestHeader "PRIVATE-TOKEN" [token]
    $ setRequestPath path
    $ setRequestSecure True
    $ setRequestPort 443
    $ defaultRequest


main :: IO ()
main = execParser opts >>= \args ->
    do
       pipelines <- getPipelines $ request args
       print pipelines
        --let status = getResponseStatus response
       --liftIO $ print $ getResponseBody response

    where
      --checkStatus st@(HT.Status sc _) = sc == 200
      opts = info (argParser <**> helper )
        ( fullDesc
        <> progDesc "Get gitlab stuff"
        <> header "gitlab-pipes a tool for listing gitlab pipelines")
