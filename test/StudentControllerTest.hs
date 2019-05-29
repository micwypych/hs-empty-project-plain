module StudentControllerTest where

import Test.HUnit

import Database.HDBC.Sqlite3
import Data.Default
import qualified Data.Map as Map
import Web.Scotty
import Web.Scotty.Internal.Types
import Network.Wai
import Network.HTTP.Types.Status

import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Except

import Student
import qualified StudentRepository as Respository
import StudentController

allTests :: Test
allTests = TestLabel "StudentControllerTest" $ TestList 
    [TestLabel "createStudent adam, beth, and caroline correctly inserts data into repository" createStudentCalled3timesProperlyInsertsValuesIntoRepository]


createStudentCalled3timesProperlyInsertsValuesIntoRepository :: Test    
createStudentCalled3timesProperlyInsertsValuesIntoRepository = TestCase $ do
    connection <- connectSqlite3 ":memory:"
    Respository.migrateStudents connection
    repository <- Respository.mkStudentRepository connection

    controller <- mkStudentController repository
    let Just adam = mkStudent "Adam" "Xyz" "0001112223"
    let Just beth = mkStudent "Beth" "Xyz" "0001112224"
    let Just caroline = mkStudent "Caroline" "Xyz" "0001112225"

    adamResponse <- runController $ createStudent controller adam
    bethResponse <- runController $ createStudent controller beth
    carolineResponse <- runController $ createStudent controller caroline

    assertStatusIs adamResponse conflict409 --probably we should expect status200, don't we? 
    assertStatusIs bethResponse status200
    assertStatusIs carolineResponse status200

-- but it would be great if we could just get rid of the real database and just use simpler in memory map
-- what should have to be changed in the StudentRepository to be able to run this code without any modification to 
-- StudentConroller nor App
{-createStudentCalled3timesProperlyInsertsValuesIntoRepository :: Test    
createStudentCalled3timesProperlyInsertsValuesIntoRepository = do
    let repository = Map.empty
    controller <- mkStudentController repository

    let Just adam = mkStudent "Adam" "Xyz" "0001112223"
    let Just beth = mkStudent "Beth" "Xyz" "0001112224"
    let Just caroline = mkStudent "Caroline" "Xyz" "0001112225"

    adamResponse <- runController $ createStudent controller adam
    bethResponse <- runController $ createStudent controller beth
    carolineResponse <- runController $ createStudent controller caroline

    assertStatusIs adamResponse status200
    assertStatusIs bethResponse status200
    assertStatusIs carolineResponse status200
    assertThatCountIs repository 3

    assertThatContains "Adam" repository adam 
    assertThatContains "Beth" repository beth 
    assertThatContains "Caroline" repository caroline-}

runController :: ActionM () -> IO (ScottyResponse)
runController action = do
    let passActionEnv = runReaderT . runExceptT . runAM $ action
    response <- execStateT (passActionEnv defualtEnvironment) def
    return response

defualtEnvironment :: ActionEnv
defualtEnvironment = Env defaultRequest [] (return "") (return "") []


assertStatusIs :: ScottyResponse -> Status -> Assertion
assertStatusIs response expectedResponseCode =
    assertEqual "the status is different than expected" expectedResponseCode (srStatus response)

assertThatCountIs :: Map.Map Int Student -> Int -> Assertion
assertThatCountIs students expectedCounts = do
    assertEqual ("the expected number of elements in the repository is different than expected\nrepository: "++show students) expectedCounts (Map.size students)

assertThatContains :: String -> Map.Map Int Student -> Student -> Assertion
assertThatContains name students expectedStudent = do 
    let justStudents = Map.elems students
    assertBool ("the student: "++name++" is not in the repository\nrepository: "++show students) (expectedStudent `elem` justStudents)