import System.Environment
import Data.List
import Data.String.Utils

createClassHeader className = concat ["class ", className, ":\n"]

createFunctionHeader depth functionName parameters =  concat [(indentDepth depth), "def ", functionName,  "(", (prepareParams parameters), "):\n"]

createConstructorHeader members = createFunctionHeader 1 "__init__" ("self " ++ members)

setBasicMembers members = concat $ map (\paramName -> concat [indentDepth 2, "self.", paramName, " = ", paramName, "\n"]) (words members)

createConstructor members = concat [createConstructorHeader members, setBasicMembers members]

prepareParams = intercalate ", " . words

indentDepth n = concat $ replicate (n*4) " "

createClass className members = concat [createClassHeader className, createConstructor members]

main = do
  args <- getArgs
  putStrLn $ createClass (head args) (last args)
