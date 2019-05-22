module StudentController where

import Data.Maybe 

import Control.Monad.IO.Class

import Web.Scotty
import Network.HTTP.Types.Status

import Student
import qualified StudentRepository as R


import Database.HDBC(IConnection(..))

data StudentController conn = StudentController {repository'::R.StudentRepository conn}

mkStudentController:: IConnection conn => R.StudentRepository conn -> IO (StudentController conn)
mkStudentController repository = do 
    return $ StudentController repository


createStudent :: IConnection conn => StudentController conn -> Student -> ActionM ()
createStudent  controller (Student (FirstName firstName) (LastName lastName) (StudentId studentId)) = do
    let studentMaybe = mkStudent firstName lastName studentId
    student <- maybe failedValidation return studentMaybe
    --when (notValid studentMaybe) $ do
    --    status badRequest400
    --    finish

    let repository = repository' controller
    idMaybe <- liftIO $ R.createStudent repository student

    maybe failedInsert (\identifier -> json (identifier, student)) idMaybe
    
failedValidation = do
    status badRequest400
    finish

failedInsert = do
    status conflict409
    finish