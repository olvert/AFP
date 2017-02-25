module WebForm (Web, Form, Answers, runWeb) where

import Replay
import Web.Scotty
import Data.Text.Lazy (Text, pack)
import Control.Monad.IO.Class
import Codec.Binary.Base64.String

type Web a = Replay Form Answers a

type ID = String

type Form = [FQuestion]
type FQuestion = (ID, String)

type Answers = [FAnswer]
type FAnswer = (ID, String)

runWeb :: Web () -> ActionM ()
runWeb web = do
  t <- getRawTrace
  liftIO $ print $ map decode t
  liftIO $ print $ parseTrace t
  r <- liftIO $ run web $ parseTrace t
  case r of
    Left (f, t') -> serve f t'

serve :: Form -> Trace Answers -> ActionM ()
serve f t = html $
            pack $
            unlines $
            wrapBodyHtml $
            concat [getFormHtml f, getOKHtml, getTraceHtml t]

wrapBodyHtml :: [String] -> [String]
wrapBodyHtml ss = concat [prepend, ss, append]
  where prepend = ["<html><body>", "<form method=post>"]
        append  = ["</form>", "</body></html>"]

getFormHtml :: Form -> [String]
getFormHtml = concatMap getQuestionHtml

getQuestionHtml :: FQuestion -> [String]
getQuestionHtml (i, q) = [question, input]
  where question = concat ["<p>", q, "</p>"]
        input    = concat ["<input name=", i, ">"]

getOKHtml :: [String]
getOKHtml = ["<input type=submit value=OK>"]

getTraceHtml :: Trace Answers -> [String]
getTraceHtml = zipWith (curry getItemHtml) [0..]

getItemHtml :: (Int, Item Answers) -> String
getItemHtml (i, item) = concat
  [ "<input type=hidden name="
  , show i
  , " value="
  , encode $ show item
  , ">"]

parseTrace :: [String] -> Trace Answers
parseTrace = map parse
  where parse s = Answer $ read $ decode s

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
