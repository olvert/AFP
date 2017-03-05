-- | A web interface using the Replay monad
-- with Form as questions and Answers as trace.
module WebForm (Web, Form, Answers, InputType (String, Num), QInfo, runWeb) where

import Replay
import Web.Scotty
import Data.Text.Lazy (Text, pack)
import Data.Char
import Control.Monad.IO.Class
import Codec.Binary.Base64.String


-- * Types

-- | Type synonym for Replay Form Answer a.
type Web a = Replay Form Answers a

-- | Type synonym for String, used as an identifier.
type ID = String

-- | Data type defining possible input types in form.
data InputType = String | Num
  deriving (Show, Read, Eq)

-- | Type synonym for tuple consisting of a question ID and InputType
type QInfo = (ID, InputType)

-- | Type synonym for a list of form questions, FQuestion.
type Form = [FQuestion]

-- | Type synonym for a question item consisting of ID, InputType and
-- the question itself.
type FQuestion = (QInfo, String)

-- | Type synonym for a list of form answers, FAnswer.
type Answers = [FAnswer]

-- | Type synonym for a answer item consisting of ID, InputType and
-- the answer.
type FAnswer = (QInfo, String)

-- | Type synonym used to make function type signatures slightly shorter.
type EitherWeb = Either (Form, Trace Answers) String


-- * Server logic

-- | Main run function. Takes a web replay application and serves html page.
runWeb :: Web String -> ActionM ()
runWeb web = do
  (prev, curr) <- trace
  r <- liftIO $ run web $ nextTrace prev curr
  html $ nextHtml curr r

  where
    -- Returns appropiate html page based on input
    nextHtml :: Maybe (Item Answers) -> EitherWeb -> Text
    nextHtml _ (Right s) = serveResult s
    nextHtml Nothing (Left (f, t)) = serveForm f t
    nextHtml (Just as) (Left (f, t)) = serveFormE f t $ validate as

    -- Makes sure that trace is not advanced if input is invalid
    nextTrace :: Trace Answers -> Maybe (Item Answers) -> Trace Answers
    nextTrace t Nothing = t
    nextTrace t (Just as) | and $ validate as = t ++ [as]
                          | otherwise = t

-- | Retrieves and parses all trace and form info from html page.
trace :: ActionM (Trace Answers, Maybe (Item Answers))
trace = do
  rawTrace    <- getRawTrace
  rawFormInfo <- getRawFormInfo
  let prev = parseTrace rawTrace in
    case rawFormInfo of
      [] -> return (prev, Nothing)
      _  -> do
        curr <- getFormInputValues $ parseFormInputInfo rawFormInfo
        return (prev, Just curr)


-- * HTML Generators

-- | Serves an ActionM monad based on a form with a corresponding trace.
serveForm :: Form -> Trace Answers -> Text
serveForm f t = serveFormE f t $ replicate (length f) True

serveFormE :: Form -> Trace Answers -> [Bool] -> Text
serveFormE f t bs = preProcess $
                    wrapFormHtml $
                    concat [ genFormHtmlE f bs
                           , genOKHtml
                           , genTraceHtml t
                           , genFormInputHtml f
                           , genErrorHtml $ and bs]

serveResult :: String -> Text
serveResult s = preProcess ["<p>", s, "</p>"]

-- | Takes a list of html strings and converts them to a text
-- element ready to be served by Scotty.
preProcess :: [String] -> Text
preProcess ss = pack $ unlines $ wrapBodyHtml ss

-- | Surround a list of html strings with the html tags: html, body.
wrapBodyHtml :: [String] -> [String]
wrapBodyHtml ss = concat [prepend, ss, append]
  where prepend = ["<html><body>"]
        append  = ["</body></html>"]

-- | Surround a list of html strings with the html tags: form.
wrapFormHtml :: [String] -> [String]
wrapFormHtml ss = concat [prepend, ss, append]
  where prepend = ["<form method=post>"]
        append  = ["</form>"]

-- | Generates html strings for error message if bool argument is true.
genErrorHtml :: Bool -> [String]
genErrorHtml True  = []
genErrorHtml False =
  [ "<p><i>Your input contained errors.</i></p>"
  , "<p><i>You are therefor being scolded accordingly.</i></p>"]

-- | Convert a Form to its html representaion.
genFormHtmlE :: Form -> [Bool] -> [String]
genFormHtmlE f bs = concatMap genQuestionHtmlE $ zip f bs

-- | Convert a FQuestion to its html representaion.
-- Bool argument denotes whether input was invalid.
genQuestionHtmlE :: (FQuestion, Bool) -> [String]
genQuestionHtmlE ((i, q), v) = if v then [question, input] else [questionE, input]
  where question  = concat ["<p>", q, "</p>"]
        questionE = concat ["<p style=\"color:red;\">", q, "</p>"]
        input     = concat ["<input name=", show $ fst i, ">"]

-- | A html submit button.
genOKHtml :: [String]
genOKHtml = ["<input type=submit value=OK>"]

-- | Convert a Trace Answers to hidden html elements.
genTraceHtml :: Trace Answers -> [String]
genTraceHtml = zipWith (curry genItemHtml) [0..]

-- | Converts Form to list of html strings.
genFormInputHtml :: Form -> [String]
genFormInputHtml f = [genInputHtml formInputID (show $ map fst f)]

-- | Generate hidden input html element with a number as name attribute.
genItemHtml :: (Int, Item Answers) -> String
genItemHtml (i, item) = genInputHtml (show i) (show item)

-- | Generates a input form element with name and value.
genInputHtml :: String -> String -> String
genInputHtml name value = concat
  [ "<input type=hidden name="
  , name
  , " value="
  , encode value
  , ">"]


-- * Parsers

-- | Converts strings to Trace Answers.
parseTrace :: [String] -> Trace Answers
parseTrace = map parse
  where parse s = read $ decode s

-- | Parses html form input names to list of names.
parseFormInputInfo :: String -> [(ID, InputType)]
parseFormInputInfo info = read $ decode info


-- * HTML Getters

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

-- | Parse the values from the named elements,
-- returning them as a ActionM monad.
getFormInputValues :: [QInfo] -> ActionM (Item Answers)
getFormInputValues info = do
  values <- mapM f info
  return $ Answer $ zip info values
  where f i = getInput $ pack $ fst i

-- | Gets input from byteString
getRawFormInfo :: ActionM String
getRawFormInfo = getInput $ pack formInputID

-- | Retrieves value from input field.
getInput :: Text -> ActionM String
getInput t = param t `rescue` \ _ -> return ""

-- | Form input string.
formInputID :: String
formInputID = "form-input"


-- * Misc

-- | Validates form answers.
validate :: Item Answers -> [Bool]
validate (Result s) = [True]
validate (Answer as) = map valid as
  where
    valid :: FAnswer -> Bool
    valid ((_, String), a) = True
    valid ((_, Num), a) = all isNumber a
