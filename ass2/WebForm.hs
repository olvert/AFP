module WebForm (Web, Form, Answers, runWeb) where

import Replay
import Web.Scotty
import Data.Text.Lazy (Text, pack)
import Control.Monad.IO.Class

type Web a = Replay Form Answers a

type ID = String

type Form = [FQuestion]
type FQuestion = (ID, String)

type Answers = [FAnswer]
type FAnswer = (ID, String)

runWeb :: Web () -> ActionM ()
runWeb web = do
  t <- getRawTrace
  r <- liftIO $ run web $ parseTrace t
  case r of
    Left (f, t') -> html $ pack $ unlines $ getFormHtml f

getFormHtml :: Form -> [String]
getFormHtml = concatMap getQuestionHtml

getQuestionHtml :: FQuestion -> [String]
getQuestionHtml (i, q) = [question, input]
  where question = concat ["<p>", q, "</p>"]
        input    = concat ["<input name=", i, ">"]

parseTrace :: [String] -> Trace Answers
parseTrace = map parse
  where parse s = Answer $ read s

getRawTrace :: ActionM [String]
getRawTrace = getRawTrace' 0
  where
    getRawTrace' :: Int -> ActionM [String]
    getRawTrace' i = do
      input <- getInput i
      case input of
        [] -> return []
        s  -> do
          ss <- getRawTrace' (i+1)
          return (s:ss)

    getInput :: Int -> ActionM String
    getInput i = param (packInt i) `rescue` \ _ -> return ""

    packInt :: Int -> Text
    packInt i = pack $ show i
