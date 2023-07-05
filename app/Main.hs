{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified Data.ByteString.Char8    as BC
import           Gitlab
import           Network.HTTP.Simple
import           Options.Applicative

data Args = Args
    { project     :: String
    , serverl_url :: String
    , pToken      :: String
    , reference   :: String
    , latest      :: Bool
    , perPage     :: Int
    }

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
    <*> strOption
        (long "reference"
        <> showDefault
        <> value "main"
        <> metavar "REFERENCE"
        <> help "Git reference"
        )
    <*> switch (long "latest" <> help "Only show latest pipeline")
    <*> option auto
        (long "perPage"
        <> showDefault
        <> value 3
        <> metavar "PERPAGE"
        <> help "How many pipelines want to show"
        )



request :: Args -> Request
request (Args project_id'  token server_url ref' latest' perPage') =
    addToRequestQueryString [("ref", Just $ BC.pack ref')]
    $  addToRequestQueryString [("per_page", Just $ BC.pack $ show perPage')]
    $  buildRequest (BC.pack token) (BC.pack server_url)  "GET" ("/api/v4/projects/" <> (BC.pack project_id') <> "/pipelines" <> isLatest latest' )
        where
            isLatest lts = if lts then "/latest" else "/"

latestPipe :: Args -> Bool
latestPipe (Args _  _ _ _ latest' _) =  latest'

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
           getPipelines  (request args) >>= \pipelines -> print pipelines
    where
      opts = info (argParser <**> helper )
        ( fullDesc
        <> progDesc "Get gitlab stuff"
        <> header "gitlab-pipes a tool for listing gitlab pipelines")
