{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Data.Foldable (toList)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust, listToMaybe, catMaybes)
import           Network.HTTP.Req (useHttpsURI)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Read (readMaybe)
import           Text.URI (mkURI)

import           Api
import           CmdArgs (CmdArgs(..), parseCmdArgs)
import           Render (RenderOptions (..), render)
import           Types

data RawFeedback = RawFeedback
  { adjudicatorId :: Int
  , adjudicatorName :: Text
  , urlKey :: Text
  , roundId :: Int
  , roundName :: Text
  , feedback :: Feedback
  }

parseAnswer :: AnswerType -> Text -> Maybe Types.Answer
parseAnswer AnswerTypeBool b
  | b == "true" || b == "True" = pure $ AnswerBool True
  | b == "false" || b == "False" = pure $ AnswerBool False
parseAnswer AnswerTypeInt i = AnswerInt <$> readMaybe (Text.unpack i)
parseAnswer AnswerTypeText t = pure $ AnswerText t
parseAnswer _ _ = Nothing

mungeAnswer :: Token -> [Text] -> Api.Answer -> IO (Maybe QuestionAnswer)
mungeAnswer token hiddenQuestions Answer { question, answer } = do
  QuestionReq { text = questionText, answer_type, seq } <-
    getQuestion question token
  if questionText `elem` hiddenQuestions
    then pure Nothing
    else
      case parseAnswer answer_type answer of
        Nothing -> fail $ concat
          [ "Unexpected answer. Answer type: "
          , show answer_type
          , ". Answer: "
          , Text.unpack answer
          ]
        Just answer' -> pure $ Just $ QuestionAnswer
          { question = questionText
          , questionSequenceNumber = seq
          , answer = answer'
          }

mungeFeedback
  :: Token
  -> [Text]
  -> FeedbackReq
  -> IO (Maybe RawFeedback)
mungeFeedback _ _ FeedbackReq { confirmed = False } = pure Nothing
mungeFeedback
  token
  hiddenQuestions
  FeedbackReq { adjudicator, debate, answers, score }
  = do
    AdjudicatorReq { id = adjId, name = adjName, url_key } <-
      getAdjudicator adjudicator token
    content <-
      sortOn questionSequenceNumber . catMaybes <$>
        traverse (mungeAnswer token hiddenQuestions) answers
    let feedback = Feedback { score, content }
    let roundUrlParts = drop 2 $ reverse (Text.splitOn "/" debate)
    let roundId = fromJust $ readMaybe . Text.unpack =<< listToMaybe roundUrlParts -- TODO error handling
    let roundUrl = Text.intercalate "/" $ reverse roundUrlParts
    RoundReq { name = roundName} <- getRound roundUrl token
    pure $ Just $ RawFeedback
      { adjudicatorId = adjId
      , adjudicatorName = adjName
      , urlKey = url_key
      , roundId = roundId
      , roundName = roundName
      , feedback
      }

index :: (Foldable f) => (a -> Int) -> f a -> IntMap (NonEmpty a)
index f = foldr (\a m -> IntMap.insertWith (<>) (f a) (a :| []) m) IntMap.empty

mungeFeedbacks :: [RawFeedback] -> [AdjudicatorFeedbacks]
mungeFeedbacks feedbacks
  = toList
  $ IntMap.map mungeAdjudicatorFeedbacks
  $ index adjudicatorId feedbacks
  where
    mungeAdjudicatorFeedbacks
      :: NonEmpty RawFeedback
      -> AdjudicatorFeedbacks
    mungeAdjudicatorFeedbacks feedbacks
      = let RawFeedback { adjudicatorName, urlKey } = NonEmpty.head feedbacks
            feedbackByRound
              = NonEmpty.groupAllWith1 roundId feedbacks in
        AdjudicatorFeedbacks
          { adjudicator = adjudicatorName
          , urlKey = urlKey
          , rounds = fmap mungeRoundFeedbacks feedbackByRound
          }

    mungeRoundFeedbacks :: NonEmpty RawFeedback -> RoundFeedbacks
    mungeRoundFeedbacks feedbacks
      = let RawFeedback { roundName } = NonEmpty.head feedbacks in
        RoundFeedbacks
          { round = roundName
          , feedbacks = fmap feedback feedbacks
          }

main :: IO ()
main = do
  cmdArgs <- parseCmdArgs
  let token = cmdArgsToken cmdArgs
  let hiddenQuestions = cmdArgsHiddenQuestions cmdArgs
  let baseDir = cmdArgsBaseDir cmdArgs
  let baseURLT = cmdArgsBaseUrl cmdArgs
  baseURI <- mkURI $ cmdArgsBaseUrl cmdArgs
  baseURL <- case useHttpsURI baseURI of
    Just (url, _) -> pure url
    Nothing -> fail $ "Invalid URL: " ++ Text.unpack baseURLT
  feedbacks <- mungeFeedbacks . catMaybes <$>
    (traverse (mungeFeedback token hiddenQuestions) =<< getFeedback baseURL token)
  let renderOptions = RenderOptions
        { baseDir = baseDir
        , randomizeOrder = cmdArgsRandomizeOrder cmdArgs
        }
  render renderOptions feedbacks
