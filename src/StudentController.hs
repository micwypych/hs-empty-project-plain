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
    --the line is equivallent to the 3 lines below
    student <- maybe failedValidation return studentMaybe
    --when (notValid studentMaybe) $ do
    --    failedValidation
    --let Just student = studentMaybe

    let repository = repository' controller
    idMaybe <- liftIO $ R.createStudent repository student -- this probably does not work properly due to the uncaught exception, see StudentRepository test.

    maybe failedInsert (\identifier -> json (identifier, student)) idMaybe
    
failedValidation = do
    status badRequest400
    finish

failedInsert = do
    status conflict409
    finish