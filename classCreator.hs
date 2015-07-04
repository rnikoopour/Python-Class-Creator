import System.Environment
import Data.List

indentDepth n = concat $ replicate (n*4) " "

addLineBreaks = intercalate "\n"

createFunctionHeader depth functionName parameters = concat [(indentDepth depth), "def ", functionName,  "(", (prepareParams parameters), "):"]

createConstructorHeader members = createFunctionHeader 1 "__init__" ("self " ++ members)

prepareParams = intercalate ", " . words

setBasicMember member = concat [indentDepth 2, "self.", member, " = ", member]

setBasicMembers members = addLineBreaks $ map setBasicMember (words members)

createConstructor members = addLineBreaks $ [createConstructorHeader members, setBasicMembers members]

createClassHeader className = concat ["class ", className, ":"]

createClass className members = addLineBreaks $ [createClassHeader className, createConstructor members]

main = do
  args <- getArgs
  putStrLn $ createClass (head args) (last args)
