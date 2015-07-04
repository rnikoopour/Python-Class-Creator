import System.Environment
import Data.List


indentDepth n = concat $ replicate (n*4) " "

createFunctionHeader depth functionName parameters =  concat [(indentDepth depth), "def ", functionName,  "(", (prepareParams parameters), "):"]

createConstructorHeader members = createFunctionHeader 1 "__init__" ("self " ++ members)

prepareParams = intercalate ", " . words

setBasicMembers members = intercalate "\n" $ map (\paramName -> concat [indentDepth 2, "self.", paramName, " = ", paramName]) (words members)

createConstructor members = intercalate "\n" [createConstructorHeader members, setBasicMembers members]

createClassHeader className = concat ["class ", className, ":"]

createClass className members = intercalate "\n" [createClassHeader className, createConstructor members]

main = do
  args <- getArgs
  putStrLn $ createClass (head args) (last args)
