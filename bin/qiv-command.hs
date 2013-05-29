import System.Environment.UTF8

main = do
    [argN, argF] <- getArgs
    let oFile = "qiv-notes-" ++ argN
    appendFile oFile $ argF ++ "\n"

