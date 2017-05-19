#!/usr/bin/env stack
{-
  stack script
    --resolver lts-8.5
    --package aeson
    --package bytestring
    --package github
    --package http-types
    --package scotty
    --package text
    --package turtle
    --package wai-extra
    --package data-default
    --
    -Wall -fwarn-tabs
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GitHub.Data.PullRequests
import GitHub.Data.Webhooks.Validate (isValidPayload)
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Turtle hiding (text)
import System.Environment
import Web.Scotty as Scotty

serve :: T.Text -> IO ()
serve secret = do
  requestLogger <- mkRequestLogger def { outputFormat = Apache FromHeader }
  scotty 3000 $ do
    middleware requestLogger

    get "/health" $ text "Ok"

    post "/webhook" $ do
      body' <- body

      -- Check if the request is valid if the secret has been provided.
      optSignature <- Scotty.header "X-Hub-Signature"
      let valid = isValidPayload secret (TL.toStrict <$> optSignature) (BSL.toStrict body')
      if not valid then do
        status status500
        text "Signature does not match"
      else do
        let optPullRequestEvent = decode body' :: Maybe PullRequestEvent
        case optPullRequestEvent of
          Nothing -> do
            status status202
            text "Event is not a PullRequestEvent"
          (Just pullRequestEvent) ->
            case pullRequestEventAction pullRequestEvent of
              PullRequestClosed -> do
                let gitRef = (pullRequestCommitRef . pullRequestHead . pullRequestEventPullRequest) pullRequestEvent
                liftIO . putStrLn $ "Deleting namespace staging-" <> T.unpack gitRef
                (kcExitCode, kcStdout, kcStderr) <- liftIO $ procStrictWithErr "kubectl" ["delete", "namespace", "staging-" <> gitRef] empty
                case kcExitCode of
                  ExitSuccess -> text $ TL.fromStrict kcStdout
                  ExitFailure exitCode -> do
                    status status500
                    text $ ("Unable to delete staging namespace. kubectl exited with code ")
                           <> TL.pack (show exitCode)
                           <> "\nstderr:\n" <> TL.fromStrict kcStderr
              _ -> do
                status status202
                text "Event is not a PullRequestClosed"

main :: IO ()
main = do
  optSecret <- lookupEnv "WEBHOOK_SECRET" :: IO (Maybe String)
  case optSecret of
    Nothing -> putStrLn "Please set WEBHOOK_SECRET"
    Just secret -> serve (T.pack secret)
