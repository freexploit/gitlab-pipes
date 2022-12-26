{-# LANGUAGE OverloadedStrings #-}


module Main where
import qualified Data.ByteString.Char8 as BC
import           Options.Applicative

--citadel_project :: BC.ByteString
--citadel_project = "23124553"

data PipelinesRequest = PipelinesRequest
        { url :: BC.ByteString
        }



gitlab_url :: String -> String -> BC.ByteString
gitlab_url server_url gitlab_project = (BC.pack server_url) <> "/api/v4/projects/" <> (BC.pack gitlab_project) <> "/pipelines"


data Args = Args
    { project     :: String
    , serverl_url :: String
    , pToken      :: String
    }

argParser :: Parser Args
argParser = Args
    <$> strOption
        (long "project" <> metavar "PROJECT_ID" <> help "Gitlab Project ID")
    <*> strOption
        (long "serverl_url" 
        <> metavar "SERVER_URL" 
        <> showDefault
        <> value "https://gitlab.com/"
        <> help "Gitlab Server URL"
        )
    <*> strOption
        (long "pToken" 
        <> metavar "PERSONAL_TOKEN" 
        <> help "Personal gitlab token" 
        )

printArgs :: Args -> IO ()
printArgs (Args p s _) = BC.putStrLn $ gitlab_url s p

main :: IO ()
main = execParser opts >>= printArgs
  where
    opts = info (argParser <**> helper )
      ( fullDesc
      <> progDesc "Get gitlab stuff"
      <> header "gitlab-pipes a tool for listing gitlab pipelines")

