module App where

import Student
import StudentRepository(mkStudentRepository, migrateStudents)
import qualified StudentRepository as R
import StudentController
import Database.HDBC.Sqlite3
import qualified Data.Aeson as Aeson
import Web.Scotty
import Network.Wai

import Control.Monad.IO.Class

main :: IO ()
main = runApp

exampleOldMain :: IO ()
exampleOldMain = do
    connection <- connectSqlite3 "test1.db"
    migrateStudents connection
    repository <- mkStudentRepository connection

    let Just student = mkStudent "Abc" "Efg" "00000000"
    print $ Aeson.encode student
    result <- R.createStudent repository student
    print result

    let Just student' = mkStudent "Abc2" "Efg2" "11111111"
    print $ Aeson.encode student'
    result' <- R.createStudent repository student'
    print result'


runApp :: IO ()
runApp = do
    connection <- connectSqlite3 "test1.db"
    migrateStudents connection
    repository <- mkStudentRepository connection
    controller <- mkStudentController repository

    scotty 3000 $ do
        get "/:word" $ do
            _ <- param "word"::ActionM Int
            let Just student = mkStudent "Abc" "Efg" "00000000"
            json student

        post "/student" $ do
            req <- request
            b <- body
            h <- headers
            liftIO $ putStr "request from: "
            liftIO $ print $ remoteHost req
            liftIO $ putStr "\tto:"
            liftIO $ print $ rawPathInfo req
            liftIO $ putStr "\trequest body: "
            liftIO $ print b
            liftIO $ putStr "\trequest headers: "
            liftIO $ print h
            --studentMaybe <- Aeson.decode <$> body
            --student <- maybe failedJsonParsing return studentMaybe
            student <- jsonData
            createStudent controller student