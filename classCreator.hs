import System.Environment
import Data.List
import Data.String.Utils

indentDepth n = concat $ replicate (n*4) " "

createFunctionHeader depth functionName parameters =  concat [(indentDepth depth), "def ", functionName,  "(", (prepareParams parameters), "):\n"]

createConstructorHeader members = createFunctionHeader 1 "__init__" ("self " ++ members)

prepareParams = intercalate ", " . words

setBasicMembers members = concat $ map (\paramName -> concat [indentDepth 2, "self.", paramName, " = ", paramName, "\n"]) (words members)

createConstructor members = concat [createConstructorHeader members, setBasicMembers members]

createClassHeader className = concat ["class ", className, ":\n"]

createClass className members = concat [createClassHeader className, createConstructor members]

main = do
  args <- getArgs
  putStrLn $ createClass (head args) (last args)
