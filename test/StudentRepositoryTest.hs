module StudentRepositoryTest where

import Test.HUnit

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe

import Control.Exception

import Student
import StudentRepository

allTests :: Test
allTests = TestLabel "StudentRepositoryTest" $ TestList 
    [TestLabel "createStudent adam, beth, and caroline correctly inserts data into repository" createStudentCalled3timesProperlyInsertsValuesIntoRepository,
    TestLabel "createStudent david, david fails because of duplicated data" createStudentCalled2TimesWithTheSameStudenShouldFail]


createStudentCalled3timesProperlyInsertsValuesIntoRepository :: Test
createStudentCalled3timesProperlyInsertsValuesIntoRepository = TestCase $ do
    repository <- setupInMemoryDb
    let Just adam = mkStudent "Adam" "Xyz" "0001112223"
    let Just beth = mkStudent "Beth" "Xyz" "0001112224"
    let Just caroline = mkStudent "Caroline" "Xyz" "0001112225"

    adamId <- createStudent repository adam
    bethId <- createStudent repository beth
    carolineId <- createStudent repository caroline

    assertNotEqual "Adam and Beth identifiers should be different" adamId bethId
    assertNotEqual "Beth and Caroline identifiers should be different" bethId carolineId
    assertNotEqual "Caroline and Adam identifiers should be different" carolineId adamId

    assertJustInRange adamId 1 10
    assertJustInRange bethId 1 1  --maybe this line should be adjusted?
    assertJustInRange carolineId 6 10 --so does this line...

createStudentCalled2TimesWithTheSameStudenShouldFail :: Test
createStudentCalled2TimesWithTheSameStudenShouldFail = TestCase $ do
    repository <- setupInMemoryDb
    let Just david = mkStudent "David" "Xyz" "0001112223"

    davidId <- createStudent repository david
    assertJustInRange davidId 1 10

    davidId2 <- createStudent repository david -- whoopsy ... SqlError {seState = "", seNativeError = 19, seErrorMsg = "step: UNIQUE constraint failed: students.first_name, students.last_name, students.student_id"}
    -- so check the code below:
    {-davidId2 <- createStudent repository david `catch` \(SqlError state nativeErrorCode errorMsg) -> do
        assertEqual "expected UNIQUE constriant failure" "step: UNIQUE constraint failed: students.first_name, students.last_name, students.student_id" errorMsg
        return Nothing-}

    assertNotEqual "Adam and Beth identifiers should be different" davidId davidId2
    assertBool "David identifier should be Nothing" $ isNothing davidId2

-- some helper methods to make tests more clear and readable    

setupInMemoryDb :: IO (StudentRepository Connection)
setupInMemoryDb = do
    connection <- connectSqlite3 ":memory:"
    migrateStudents connection
    repository <- mkStudentRepository connection
    return repository


-- HUnit is pretty poorly equipped with assertions, just mere assertBool, and assertEqual it is definitely not the AsserJ :/
-- but we can add some assertions to make the tests clearer, probably they should be moved to the separate module in the future...

assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual msg v1 v2 = do
    assertBool msg $ v1 /= v2

assertJustInRange :: (Eq a, Show a, Ord a) => Maybe a -> a -> a -> Assertion
assertJustInRange Nothing _ _ = do
    assertFailure "value is nothing"
assertJustInRange (Just v) expectedMinInclusive expectedMaxInclusive = do
    assertBool ("value is too small\nexpected : " ++ show v ++ " to be in range ["++show expectedMinInclusive ++ ", "++show expectedMaxInclusive ++"]") $ v >= expectedMinInclusive
    assertBool ("value is too large\nexpected : " ++ show v ++ " to be in range ["++show expectedMinInclusive ++ ", "++show expectedMaxInclusive ++"]") $ v <= expectedMaxInclusive