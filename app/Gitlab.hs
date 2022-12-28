{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Gitlab  where

import           Data.Aeson
import           Data.Text
import           Data.Time.Clock     (UTCTime)
import           GHC.Generics
import           Network.HTTP.Simple

--
--	The status of pipelines, one of: created, waiting_for_resource, preparing, pending, running, success, failed, canceled, skipped, manual, scheduled
data GitlabPipelineStatus
  = Created
  | WaitingForResource
  | Preparing
  | Pending
  | Running
  | PipelineSuccess
  | Failed
  | Canceled
  | Skipped
  | Manual
  | Scheduled
    deriving(Show, Generic)

instance FromJSON GitlabPipelineStatus where
    parseJSON (String "created")              = pure Created
    parseJSON (String "waiting_for_resource") = pure WaitingForResource
    parseJSON (String "preparing")            = pure Preparing
    parseJSON (String "pending")              = pure Pending
    parseJSON (String "running")              = pure Running
    parseJSON (String "success")              = pure PipelineSuccess
    parseJSON (String "failed")               = pure Failed
    parseJSON (String "canceled")             = pure Canceled
    parseJSON (String "skipped")              = pure Skipped
    parseJSON (String "manual")               = pure Manual
    parseJSON (String "scheduled")            = pure Scheduled
    parseJSON _                               = fail "Unable to parse"

instance ToJSON GitlabPipelineStatus

--
-- push, web, trigger, schedule, api, external, pipeline, chat, webide, merge_request_event, external_pull_request_event, parent_pipeline, ondemand_dast_scan, or ondemand_dast_validation.
--

data GitlabPipelineSource 
    = Push 
    | Web 
    | Trigger
    | Schedule
    | Api
    | External
    | Pipeline
    | Chat
    | Webide
    | MergeRequestEvent
    | ExternalPullRequestEvent
    | ParentPipeline
    | OnDemandDastScan
    | OnDemandDastValidation
    deriving(Show, Generic)


instance FromJSON GitlabPipelineSource where
    parseJSON (String "push") = pure Push 
    parseJSON (String "web") =  pure Web
    parseJSON (String "trigger") =  pure Trigger
    parseJSON (String "schedule") = pure Schedule
    parseJSON (String "api") = pure Api
    parseJSON (String "external") = pure External
    parseJSON (String "pipeline") = pure Pipeline
    parseJSON (String "chat") = pure Chat
    parseJSON (String "webide") = pure Webide
    parseJSON (String "merge_request_event") = pure MergeRequestEvent
    parseJSON (String "external_pull_request_event") = pure ExternalPullRequestEvent
    parseJSON (String "parent_pipeline") = pure ParentPipeline
    parseJSON (String "ondemand_dast_scan") = pure OnDemandDastScan
    parseJSON (String "ondemand_dast_validation") = pure OnDemandDastValidation
    parseJSON _ = fail "Unable to parse"

instance ToJSON GitlabPipelineSource

data GitlabPipeline = GitlabPipeline
    { id         :: Int
    , iid        :: Int
    , project_id :: Int
    , status     :: GitlabPipelineStatus
    , source     :: GitlabPipelineSource
    , ref        :: Text
    , sha        :: Text
    , web_url    :: Text
    , created_at :: UTCTime
    , updated_at :: UTCTime
    } deriving(Show, Generic)

instance FromJSON GitlabPipeline
instance ToJSON GitlabPipeline

-- 	The scope of pipelines, one of: running, pending, finished, branches, tags
data GitlabPipelineScope 
    = ScopeRunning 
    | ScopePending 
    | ScopeFinished 
    | ScopeBranches 
    | ScopeTags 
    deriving(Show, Generic)


instance FromJSON GitlabPipelineScope where
    parseJSON (String "running") = pure ScopeRunning 
    parseJSON (String "pending") = pure ScopePending 
    parseJSON (String "finished" ) = pure ScopeFinished 
    parseJSON (String "branches") = pure ScopeBranches 
    parseJSON (String "tags") = pure ScopeTags 
    parseJSON _ = fail "Unable to parse"

instance ToJSON GitlabPipelineScope


getPipelines :: Request -> IO [GitlabPipeline]
getPipelines request' = do
    response <- httpJSON  request'
    return $ getResponseBody response
