{-# LANGUAGE OverloadedStrings #-}
module Main where

import Student
import StudentRepository
import Database.HDBC.Sqlite3
import Data.Aeson

main :: IO ()
main = do
    connection <- connectSqlite3 "test1.db"
    migrateStudents connection
    repository <- mkStudentRepository connection

    let Just student = mkStudent "Abc" "Efg" "00000000"
    print $ encode student
    result <- createStudent repository student
    print result

    let Just student' = mkStudent "Abc2" "Efg2" "11111111"
    print $ encode student'
    result' <- createStudent repository student'
    print result'