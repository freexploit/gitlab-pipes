module Gitlab  where

import Data.Aeson

data GitlabPipeline = GitlabPipeline
    { id         :: Int
    , iid        :: Int
    , project_id :: Int
    , status :: Text
    , source:: Text
    , ref:: Text
    , sha :: Text
    , web_url :: Text
    , created_at :: Text
    , updated_at :: Text
    } deriving(Show, Generic)

instance FromJSON GitlabPipeline
instance ToJSON GitlabPipeline


getPipelines :: Request -> IO [GitlabPipeline]
getPipelines request' = do 
    response <- httpJSON  request' 
    return $ getResponseBody response
