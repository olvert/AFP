-- | A web interface using the Replay monad
-- with Form as questions and Answers as trace.
module WebForm (Web, Form, Answers, InputType (String, Num), QInfo, runWeb) where

import Replay
import Web.Scotty
import Data.Text.Lazy (Text, pack)
import Data.Char
import Control.Monad.IO.Class
import Codec.Binary.Base64.String

-- | Type synonym for Replay Form Answer a.
type Web a = Replay Form Answers a

-- | Type synonym for String, used as an identifier.
type ID = String

-- | Type synonym for a list of form questions, FQuestion.
type Form = [FQuestion]

-- | Type synonym for a pair with an ID and a string, where ID is an identifier
-- and String is the question in text.
type FQuestion = (QInfo, String)

-- | Type synonym for a list of form answers, FAnswer.
type Answers = [FAnswer]

type QInfo = (ID, InputType)

-- | Type synonym for a pair with an ID and a string, where ID is an identifier
-- and String is the answer in text.
type FAnswer = (QInfo, String)

data InputType = String | Num
  deriving (Show, Read, Eq)


type EitherWeb = Either (Form, Trace Answers) ()

-- | Takes a 'Web' and returns a 'ActionM' based on that monad.
runWeb :: Web () -> ActionM ()
runWeb web = do
  (prev, curr) <- trace
  r <- liftIO $ run web $ nextTrace prev curr
  html $ nextHtml curr r

  where
    nextHtml :: Maybe (Item Answers) -> EitherWeb -> Text
    nextHtml _ (Right x) = serveResult
    nextHtml Nothing (Left (f, t)) = serveForm f t
    nextHtml (Just as) (Left (f, t)) = serveFormE f t $ validate as

    nextTrace :: Trace Answers -> Maybe (Item Answers) -> Trace Answers
    nextTrace t Nothing = t
    nextTrace t (Just as) | and $ validate as = t ++ [as]
                          | otherwise = t

trace :: ActionM (Trace Answers, Maybe (Item Answers))
trace = do
  rawTrace    <- getRawTrace
  rawFormInfo <- getRawFormInfo
  let prev = parseTrace rawTrace in
    case rawFormInfo of
      [] -> return (prev, Nothing)
      _  -> do
        liftIO $ print $ parseFormInputInfo rawFormInfo
        curr <- getFormInputValues $ parseFormInputInfo rawFormInfo
        return (prev, Just curr)

validate :: Item Answers -> [Bool]
validate (Result s) = [True]
validate (Answer as) = map valid as
  where
    valid :: FAnswer -> Bool
    valid ((_, String), a) = True
    valid ((_, Num), a) = all isNumber a

preProcess :: [String] -> Text
preProcess ss = pack $ unlines $ wrapBodyHtml ss

-- | Serves an ActionM monad based on a form with a corresponding trace.
serveForm :: Form -> Trace Answers -> Text
serveForm f t = serveFormE f t $ replicate (length f) True

serveFormE :: Form -> Trace Answers -> [Bool] -> Text
serveFormE f t bs = preProcess $
                    wrapFormHtml $
                    concat [ getFormHtmlE f bs
                           , getOKHtml
                           , getTraceHtml t
                           , getFormInputHtml f
                           , getErrorHtml $ and bs]

serveResult :: Text
serveResult = undefined

-- | Surround a list of Strings with the html tags: html, body, form with
-- method attribute of type post.
wrapBodyHtml :: [String] -> [String]
wrapBodyHtml ss = concat [prepend, ss, append]
  where prepend = ["<html><body>"]
        append  = ["</body></html>"]

wrapFormHtml :: [String] -> [String]
wrapFormHtml ss = concat [prepend, ss, append]
  where prepend = ["<form method=post>"]
        append  = ["</form>"]

getErrorHtml :: Bool -> [String]
getErrorHtml True  = []
getErrorHtml False =
  [ "<p><i>Your input contained errors.</i></p>"
  , "<p><i>You are therefor being scolded accordingly.</i></p>"]

getFormHtmlE :: Form -> [Bool] -> [String]
getFormHtmlE f bs = concatMap getQuestionHtmlE $ zip f bs

-- | Convert a FQuestion to its html representaion.
getQuestionHtmlE :: (FQuestion, Bool) -> [String]
getQuestionHtmlE ((i, q), v) = if v then [question, input] else [questionE, input]
  where question  = concat ["<p>", q, "</p>"]
        questionE = concat ["<p style=\"color:red;\">", q, "</p>"]
        input     = concat ["<input name=", show $ fst i, ">"]

getQuestionHtml :: FQuestion -> [String]
getQuestionHtml q = getQuestionHtmlE (q, True)

-- | A html submit button.
getOKHtml :: [String]
getOKHtml = ["<input type=submit value=OK>"]

-- | Convert a Trace Answers to hidden html elements.
getTraceHtml :: Trace Answers -> [String]
getTraceHtml = zipWith (curry getItemHtml) [0..]

-- | Converts Form to list of html strings.
getFormInputHtml :: Form -> [String]
getFormInputHtml f = [getInputHtml formInputID (show $ map fst f)]

-- | Generate hidden input html element with a number as name attribute.
getItemHtml :: (Int, Item Answers) -> String
getItemHtml (i, item) = getInputHtml (show i) (show item)

-- | Generates a input form element with name and value.
getInputHtml :: String -> String -> String
getInputHtml name value = concat
  [ "<input type=hidden name="
  , name
  , " value="
  , encode value
  , ">"]

-- | Converts strings to Trace Answers.
parseTrace :: [String] -> Trace Answers
parseTrace = map parse
  where parse s = read $ decode s

-- | Gather all hidden elements in a html form (byteString) to be used
-- to generate traces.
getRawTrace :: ActionM [String]
getRawTrace = getRawTrace' 0
  where
    getRawTrace' :: Int -> ActionM [String]
    getRawTrace' i = do
      input <- getInput $ packInt i
      case input of
        [] -> return []
        s  -> do
          ss <- getRawTrace' (i+1)
          return (s:ss)

    packInt :: Int -> Text
    packInt i = pack $ show i

-- | Parses html form input names to list of names.
parseFormInputInfo :: String -> [(ID, InputType)]
parseFormInputInfo info = read $ decode info

-- | Parse the values from the named elements,
-- returning them as a ActionM monad.
getFormInputValues :: [QInfo] -> ActionM (Item Answers)
getFormInputValues info = do
  values <- mapM f info
  liftIO $ print values
  return $ Answer $ zip info values
  where f i = getInput $ pack $ fst i

-- | Gets input from byteString
getRawFormInfo :: ActionM String
getRawFormInfo = getInput $ pack formInputID

-- | Form input string.
formInputID :: String
formInputID = "form-input"

-- | Retrieves value from input field.
getInput :: Text -> ActionM String
getInput t = param t `rescue` \ _ -> return ""
