import System.Environment
import System.IO.Error
import Control.Exception

data IncorrectNumberOfArguments = MkMyException deriving Show
instance Exception IncorrectNumberOfArguments

countLines file = do
  contents <- readFile file
  return (length $ lines contents)

wordCounter file = do
    contents <- readFile file
    return (length . words $ contents)

signCounter file = do 
    contents <- readFile file
    return (length contents)

linesOver80Counter file = do
    contents <- readFile file
    let arr = lines contents
    return (length [x | x<-arr , length x >80])

    


riskyAction :: IO ()
riskyAction = do (fileName:a) <- getArgs
                 numOfLines <- countLines fileName
                 putStrLn ("Nr of lines = " ++ show numOfLines)
                 numOfWords <- wordCounter fileName
                 putStrLn ("Nr of words = " ++ show numOfWords)
                 numOfSings <- signCounter fileName
                 putStrLn ("Nr of sings = " ++ show numOfSings)
                 numOver80 <- linesOver80Counter fileName
                 putStrLn ("Nr of lines over 80 = " ++ show numOver80)
                 
                 
                 



                 

exHdlr :: IOError -> IO ()
exHdlr = \ex -> if isDoesNotExistError ex
                then putStrLn "The file doesn't exist!"
                else ioError ex

main :: IO ()
main = riskyAction `catch` exHdlr