{-# LANGUAGE OverloadedStrings #-}


module Main where
import qualified Data.ByteString.Char8 as BC
import Options.Applicative




--citadel_project :: BC.ByteString
--citadel_project = "23124553"

gitlab_url :: BC.ByteString -> BC.ByteString
gitlab_url gitlab_project = "https://gitlab.com/api/v4/projects/" <> gitlab_project <> "/pipelines"


data Args = Args
    { project :: String
    , pToken :: String
    }

argParser :: Parser Args
argParser = Args
    <$> strOption
        (long "project" <> metavar "PROJECT_ID" <> help "Gitlab Project ID")
    <*> strOption
        (long "pToken" <> metavar "PERSONAL_TOKEN" <> help "Personal gitlab token" )


printArgs :: Args -> IO ()
printArgs (Args p t) = putStrLn $ p ++ " " ++ t

main :: IO ()
main = execParser opts >>= printArgs
  where
    opts = info (argParser <**> helper )
      ( fullDesc 
      <> progDesc "Get gitlab stuff"
      <> header "gitlab-pipes a tool for listing gitlab pipelines")



