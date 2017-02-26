-- | A web interface using the Replay monad 
-- with Form as questions and Answers as trace.
module WebForm (Web, Form, Answers, runWeb) where

import Replay
import Web.Scotty
import Data.Text.Lazy (Text, pack)
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
type FQuestion = (ID, String)

-- | Type synonym for a list of form answers, FAnswer.
type Answers = [FAnswer]

-- | Type synonym for a pair with an ID and a string, where ID is an identifier
-- and String is the answer in text.
type FAnswer = (ID, String)

-- | Takes a 'Web' and returns a 'ActionM' based on that monad. 
runWeb :: Web () -> ActionM ()
runWeb web = do
  t <- trace
  liftIO $ print t
  --  liftIO $ print $ parseTrace t
  r <- liftIO $ run web t
  case r of
    Left (f, t') -> serve f t'
    Right x      -> liftIO $ putStrLn "Done!"

trace :: ActionM (Trace Answers)
trace = do
  rawTrace        <- getRawTrace
  formInput       <- getRawFormInput
  liftIO $ putStrLn $ "Form input: " ++ formInput
  let parsedTs = parseTrace rawTrace in
    case formInput of
      [] -> return parsedTs
      _  -> do
        parsedT <- parseFormInputValues $ parseFormInputNames formInput
        return $ parsedTs ++ [parsedT]

-- | Serves a ActionM monad based on a form with a corresponding trace.
serve :: Form -> Trace Answers -> ActionM ()
serve f t = html $
            pack $
            unlines $
            wrapBodyHtml $
            concat [ getFormHtml f
                   , getOKHtml
                   , getTraceHtml t
                   , getFormInputHtml f]

-- | Surround a list of Strings with the html tags: html, body, form with 
-- method attribute of type post.
wrapBodyHtml :: [String] -> [String]
wrapBodyHtml ss = concat [prepend, ss, append]
  where prepend = ["<html><body>", "<form method=post>"]
        append  = ["</form>", "</body></html>"]

-- | Convert a Form to its html representaion.
getFormHtml :: Form -> [String]
getFormHtml = concatMap getQuestionHtml

-- | Convert a FQuestion to its html representaion.
getQuestionHtml :: FQuestion -> [String]
getQuestionHtml (i, q) = [question, input]
  where question = concat ["<p>", q, "</p>"]
        input    = concat ["<input name=", i, ">"]

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
parseFormInputNames :: String -> [String]
parseFormInputNames s = read $ decode s

-- | Parse the values from the named elements,
-- returning them as a ActionM monad.
parseFormInputValues :: [String] -> ActionM (Item Answers)
parseFormInputValues names = do
  values <- mapM f names
  return $ Answer $ zip names values
  where f n = getInput $ pack n

-- | Gets input from byteString
getRawFormInput :: ActionM String
getRawFormInput = getInput $ pack formInputID

-- | Form input string.
formInputID :: String
formInputID = "form-input"

-- | Retrieves value from input field.
getInput :: Text -> ActionM String
getInput t = param t `rescue` \ _ -> return ""
