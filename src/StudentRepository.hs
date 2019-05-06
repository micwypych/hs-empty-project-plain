{-# LANGUAGE MultiParamTypeClasses #-}
module StudentRepository where

import Database.HDBC
import Data.Convertible

import Student

data StudentRepository conn = StudentRepository {connection'::conn, insertStudentStmt'::Statement, selectInsertedIdStmt'::Statement}

migrateStudents :: IConnection conn => conn -> IO ()
migrateStudents connection = do
    withTransaction connection $ \conn ->
      run conn "CREATE TABLE students (id INTEGER PRIMARY KEY AUTOINCREMENT, first_name TEXT NOT NULL, last_name TEXT NOT NULL, student_id TEXT NOT NULL, UNIQUE(first_name,last_name,student_id))" []
    return ()


mkStudentRepository :: IConnection conn => conn -> IO (StudentRepository conn)
mkStudentRepository connection = do
    
    insertStudentStmt <- prepare connection insertStudentQuery
    selectInsertedIdStmt <- prepare connection selectInsertedIdQuery
    return $ StudentRepository connection insertStudentStmt selectInsertedIdStmt



insertStudentQuery :: String
insertStudentQuery = "INSERT INTO students(first_name, last_name, student_id) VALUES (?, ?, ?)"

selectInsertedIdQuery :: String
selectInsertedIdQuery = "SELECT last_insert_rowid()"

instance Convertible FirstName SqlValue where
    safeConvert (FirstName fn) = safeConvert fn

instance Convertible LastName SqlValue where
    safeConvert (LastName ln) = safeConvert ln

instance Convertible StudentId SqlValue where
    safeConvert (StudentId id) = safeConvert id

createStudent :: IConnection conn => StudentRepository conn -> Student -> IO (Maybe Int)
createStudent (StudentRepository connection insertStudentStmt selectInsertedIdStmt) (Student firstName lastName studentId) = do
    withTransaction connection $ \_ -> do
        nInserted <- execute insertStudentStmt [toSql firstName, toSql lastName, toSql studentId]
        execute selectInsertedIdStmt []
        mId <- fetchRow selectInsertedIdStmt
        return $ fmap (\list -> fromSql $ list !! 0) mId