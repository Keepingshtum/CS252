import Text.ParserCombinators.Parsec
import System.Environment
import Data.List (intercalate)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
  deriving (Eq, Ord, Show)


jsonFile :: GenParser Char st JValue
jsonFile = do
  result <- jsonArr <|> jsonObject
  spaces
  eof
  return result

jsonElem :: GenParser Char st JValue
jsonElem = do
  spaces
  result <- jsonElem'
  spaces
  return result

jsonElem' = jsonArr
        <|> jsonString
        <|> jsonBool
        <|> jsonNull
        <|> jsonNumber
        <|> jsonObject
        <?> "json element"

jsonString :: GenParser Char st JValue
jsonString = jsonStringDQ <|> jsonStringSQ

jsonStringDQ = do
  char '"'
  s <- many $ noneOf "\"" -- crude.  does not allow double quotes within strings
  char '"'
  return $ JString s

jsonStringSQ = do
  char '\''
  s <- many $ noneOf "'" -- crude, same as above
  char '\''
  return $ JString s

jsonBool = do
  bStr <- string "true" <|> string "false"
  return $ case bStr of
    "true" -> JBool True
    "false" -> JBool False

jsonNull = do
  string "null"
  return JNull

jsonArr = do
  char '['
  arr <- jsonElem `sepBy` (char ',')
  char ']'
  return $ JArray arr


jsonNumber :: GenParser Char st JValue
jsonNumber = do
  str <- many1 digit
  return $ JNumber (read str)  

jsonObject = do 
  char '{'
  fields <- jsonField `sepBy` (char ',')
  char '}'
  return $ JObject fields

jsonField = do
  spaces
  name <- many letter
  spaces
  char ':'
  spaces
  value <- jsonElem
  return (name, value)



parseJSON :: String -> Either ParseError JValue
parseJSON input = parse jsonFile "(unknown)" input


prettyPrint :: JValue -> String
prettyPrint (JString s) = "\"" ++ s ++ "\""
prettyPrint (JNumber n) = show n
prettyPrint (JBool True) = "true"
prettyPrint (JBool False) = "false"
prettyPrint JNull = "null"
prettyPrint (JArray arr) = "[" ++ intercalate ", " (map prettyPrint arr) ++ "]"
prettyPrint (JObject obj) = "{" ++ intercalate ",\n"  (map formatPair obj) ++ "}"
  where
    formatPair (key, value) = "\"" ++ key ++ "\": " ++ prettyPrint value 


main = do
  args <- getArgs
  p <- parseFromFile jsonFile (head args)
  case p of
    Left err  -> print err
    Right json -> putStrLn (prettyPrint json)


