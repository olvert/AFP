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

serve :: Form -> Trace Answers -> ActionM ()
serve f t = html $
            pack $
            unlines $
            wrapBodyHtml $
            concat [ getFormHtml f
                   , getOKHtml
                   , getTraceHtml t
                   , getFormInputHtml f]

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

getFormInputHtml :: Form -> [String]
getFormInputHtml f = [getInputHtml formInputID (show $ map fst f)]

getItemHtml :: (Int, Item Answers) -> String
getItemHtml (i, item) = getInputHtml (show i) (show item)

getInputHtml :: String -> String -> String
getInputHtml name value = concat
  [ "<input type=hidden name="
  , name
  , " value="
  , encode value
  , ">"]

parseTrace :: [String] -> Trace Answers
parseTrace = map parse
  where parse s = read $ decode s

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

parseFormInputNames :: String -> [String]
parseFormInputNames s = read $ decode s

parseFormInputValues :: [String] -> ActionM (Item Answers)
parseFormInputValues names = do
  values <- mapM f names
  return $ Answer $ zip names values
  where f n = getInput $ pack n

getRawFormInput :: ActionM String
getRawFormInput = getInput $ pack formInputID

formInputID :: String
formInputID = "form-input"

getInput :: Text -> ActionM String
getInput t = param t `rescue` \ _ -> return ""
