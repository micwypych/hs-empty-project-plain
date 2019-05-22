module App where

import Student
import StudentRepository
import Database.HDBC.Sqlite3
import qualified Data.Aeson as Aeson
import Web.Scotty

main :: IO ()
main = runApp

exampleOldMain :: IO ()
exampleOldMain = do
    connection <- connectSqlite3 "test1.db"
    migrateStudents connection
    repository <- mkStudentRepository connection

    let Just student = mkStudent "Abc" "Efg" "00000000"
    print $ Aeson.encode student
    result <- createStudent repository student
    print result

    let Just student' = mkStudent "Abc2" "Efg2" "11111111"
    print $ Aeson.encode student'
    result' <- createStudent repository student'
    print result'


runApp :: IO ()
runApp = do
    connection <- connectSqlite3 "test1.db"
    migrateStudents connection
    repository <- mkStudentRepository connection

    scotty 3000 $
        get "/:word" $ do
            beam <- param "word"::ActionM Int
            let Just student = mkStudent "Abc" "Efg" "00000000"
            json student