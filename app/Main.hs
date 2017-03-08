module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import qualified System.Process as SP --for system function
import System.Exit -- for the IO ExitCode

{-  1. Create DB with sqlite3
    2. Model tables in this file
    3. Create instances of show
    4. Create way to add resources with addResource
    5. Create way to print users 
-}

--model task and user after db fields
data Todo = Todo
    { taskId :: Int
    , taskName :: String
    , description :: String
    , dateAdded :: String
    , status :: String
    }

--get info from db
instance FromRow Todo where 
    fromRow = Todo <$> field 
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                  
--instance of both as strings
instance Show Todo where
    show todo = mconcat [ show $ taskId todo
                        , ".) "
                        , taskName todo
                        , "\n description: "
                        , description todo
                        , "\n date added: "
                        , dateAdded todo
                        , "\n status: "
                        , status todo
                        , "\n"
                        ]

--automatically handles opening and closing database
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn

--execute runs a SQL command, and the (?)'s match the String values
addTodo :: String -> String -> String -> String -> IO ()
addTodo taskName description dateAdded status = withConn "rc.db" $
                   \conn -> do
                     execute conn "INSERT INTO todo (taskName,description,dateAdded, status) VALUES (?,?,?,?)"
                       (taskName, description, dateAdded, status)
                     print "task added"

printTodoQuery :: Query -> IO ()
printTodoQuery q =  withConn "rc.db" $
                         \conn ->  do
                           resp <- query_ conn q :: IO [Todo]
                           mapM_ print resp

printTodos :: IO ()
printTodos =  printTodoQuery "SELECT * FROM todo;" 

--what task is available
printCompleted :: IO ()
printCompleted = printTodoQuery $
                    mconcat ["select * from todo "
                            ,"where status='Done';"]

--what task is checked out
printInProgress :: IO ()
printInProgress = printTodoQuery $
                 mconcat ["select * from todo "
                         ,"where status='In Progress';"]

--DB operations are I/O and Maybe catches incorrect ids
selectTodo :: Connection -> Int -> IO (Maybe Todo)
selectTodo conn taskId = do
    resp <- query conn
            "SELECT * FROM todo where id = (?)"
            (Only taskId) :: IO [Todo]
    return $ firstOrNothing resp

--returns the id of task from above
firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

updateTodo :: Todo -> String -> Todo
updateTodo todo status = Todo
    { taskId = taskId todo
    , taskName = taskName todo
    , description = description todo
    , dateAdded = dateAdded todo
    , status = status
    }

--update task or show Nothing if invalid
updateOrWarn :: Maybe Todo -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just todo) = withConn "rc.db" $
                            \conn -> do
                                let q = mconcat ["UPDATE TODO "
                                                ,"SET STATUS = ? "
                                                ,"WHERE ID = ?;"]
                                execute conn q ( status todo
                                               , taskId todo) 
                                print "todo updated"

promptAddTodo :: IO () 
promptAddTodo = do
    print "Enter new task" 
    taskName <- getLine
    print "Enter description" 
    description <- getLine
    print "Enter date added" 
    dateAdded <- getLine
    print "Enter status" 
    status <- getLine
    print "Done! Added a task"
    addTodo taskName description dateAdded status

performCommand :: String -> IO () 
performCommand command
    | command == "todo" = printTodos >> main
    | command == "add" = promptAddTodo >> main
    | command == "doing" = printInProgress >> main
    | command == "done" = printCompleted >> main
    | command == "quit" = print "bye!"
    | otherwise = print "Sorry command not found" >> main

--Rebuild database!!!!!!!!
build :: IO ExitCode
build = do
    print "This will reset the DB, type yes if you're sure"
    answer <- getLine
    if answer == "yes"
    then SP.system "sqlite3 rc.db < build_db.sql"
    else SP.system "Goodbye"

main :: IO ()
main = do
    print "Enter a command: todo, add, doing, done, quit" 
    command <- getLine 
    performCommand command
