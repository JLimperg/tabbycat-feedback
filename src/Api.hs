{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Api where

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           GHC.Generics (Generic)
import           Network.HTTP.Req
import           Text.URI (mkURI)

import           Types (Token(..))

data Answer = Answer
  { question :: Text
  , answer :: Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON Answer

data FeedbackReq = FeedbackReq
  { adjudicator :: Text
  , debate :: Text
  , answers :: [Answer]
  , confirmed :: Bool
  , score :: Float
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON FeedbackReq

newtype RoundReq = RoundReq
  { name :: Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON RoundReq

data AdjudicatorReq = AdjudicatorReq
  { id :: Int
  , name :: Text
  , url_key :: Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON AdjudicatorReq

data AnswerType
  = AnswerTypeBool
  | AnswerTypeInt
  | AnswerTypeText
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON AnswerType where
  parseJSON = withText "AnswerType" $ \case
    "bs" -> pure AnswerTypeBool
    "tl" -> pure AnswerTypeText
    "ss" -> pure AnswerTypeText
    "t" -> pure AnswerTypeText
    "i" -> pure AnswerTypeInt
    "is" -> pure AnswerTypeInt
    x -> fail $ "Unknown answer type: " ++ Text.unpack x

data QuestionReq = QuestionReq
  { text :: Text
  , answer_type :: AnswerType
  , seq :: Int
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON QuestionReq

authHeader :: Token -> Option scheme
authHeader (Token tk) = header "Authorization" $
  "Token " <> Text.encodeUtf8 tk

getOpts :: FromJSON a => Url 'Https -> Option 'Https -> Token -> IO a
getOpts url opts token = runReq defaultHttpConfig $
  responseBody <$> req GET url NoReqBody jsonResponse
    (authHeader token <> opts)

getParseURL :: FromJSON a => Text -> Token -> IO a
getParseURL url token = do
  uri <- mkURI url
  case useHttpsURI uri of
    Just (url, opts) -> getOpts url opts token
    Nothing -> fail $ "Invalid URL: " ++ Text.unpack url

get :: FromJSON a => Url 'Https -> Token -> IO a
get url = getOpts url mempty

getFeedback :: Url 'Https -> Token -> IO [FeedbackReq]
getFeedback baseUrl = get $ baseUrl /: "feedback"

getRound :: Text -> Token -> IO RoundReq
getRound = getParseURL

getAdjudicator :: Text -> Token -> IO AdjudicatorReq
getAdjudicator = getParseURL

getQuestion :: Text -> Token -> IO QuestionReq
getQuestion = getParseURL
