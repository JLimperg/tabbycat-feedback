{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Render (render) where

import           Prelude hiding (head, div)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import           System.Directory (createDirectoryIfMissing)
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes as Html
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import           Types
import           Static (stylesheet)

render :: FilePath -> [AdjudicatorFeedbacks] -> IO ()
render baseDir feedbacks = do
  createDirectoryIfMissing True baseDir
  mapM_ (writeFeedbacks baseDir) feedbacks
  writeStylesheet baseDir

writeStylesheet :: FilePath -> IO ()
writeStylesheet baseDir = BS.writeFile (baseDir ++ "/style.css") stylesheet

writeFeedbacks :: FilePath -> AdjudicatorFeedbacks -> IO ()
writeFeedbacks baseDir fb@AdjudicatorFeedbacks { urlKey }
  = let filename = baseDir ++ "/" ++ Text.unpack urlKey ++ ".html" in
    BSL.writeFile filename $ renderHtml $ renderFeedbacks fb

renderFeedbacks :: AdjudicatorFeedbacks -> Html
renderFeedbacks AdjudicatorFeedbacks { adjudicator, rounds } = docTypeHtml $ do
  head $ do
    meta ! charset "UTF-8"
    link ! rel "stylesheet" ! href "style.css"
  body $ do
    h1 $ text $ "Feedback for " <> adjudicator
    mapM_ renderRound rounds

renderRound :: RoundFeedbacks -> Html
renderRound RoundFeedbacks { round, feedbacks } = do
  h2 $ text round
  mapM_ renderFeedback feedbacks

renderFeedback :: Feedback -> Html
renderFeedback Feedback { score, content } = do
  div ! class_ "feedback" $ do
    p ! class_ "question" $ "Overall score"
    p ! class_ "answer"  $ string $ show score
    mapM_ renderQuestionAnswer content

renderQuestionAnswer :: QuestionAnswer -> Html
renderQuestionAnswer QuestionAnswer { question, answer } = do
  p ! class_ "question" $ text question
  p ! class_ "answer" $ renderAnswer answer

renderAnswer :: Answer -> Html
renderAnswer = \case
  AnswerBool True -> "yes"
  AnswerBool False -> "no"
  AnswerText t -> mapM_ (p . text) $ Text.lines t
  AnswerInt i -> string $ show i
