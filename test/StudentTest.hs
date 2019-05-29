module StudentTest where

import Test.HUnit

import Data.Maybe

import Student

allTests = TestList 
    [TestLabel "it is possible to create valid student" mkStudentValidTest, 
    TestLabel "it is impossible to create invalid student with empty first name" mkStudentInvalidEmptyNameTest,
    TestLabel "it is impossible to create invalid student with empty surname" mkStudentInvalidEmptySurnameTest,
    TestLabel "it is impossible to create invalid student with student id containing non digit characters" mkStudentInvalidNonDigitCharactersInStudentIdTest,
    TestLabel "it is impossible to create invalid student with too short student id" mkStudentInvalidStudentIdTooShortTest,
    TestLabel "it is impossible to create invalid student with too long student id" mkStudentInvalidStudentIdTooLongTest]

mkStudentValidTest = TestCase $ do
    let studentMaybe = mkStudent "Maks" "Maksowski" "0000000000"
    assertBool "validation did not return Just ..." $ isJust studentMaybe
    let expectedStudent = Student (FirstName "Mak") (LastName "Maksowski") (StudentId "0000000000")
    assertEqual "students are different" expectedStudent (fromJust studentMaybe)

mkStudentInvalidEmptyNameTest = TestCase $ do
    let studentMaybe = mkStudent "" "Maksowski" "0000000000"
    assertBool "validation passed but it should not" $ isNothing studentMaybe

mkStudentInvalidEmptySurnameTest = TestCase $ do
    let studentMaybe = mkStudent "Maks" "" "0000000000"
    assertBool "validation passed but it should not" $ isNothing studentMaybe

mkStudentInvalidNonDigitCharactersInStudentIdTest = TestCase $ do
    let studentMaybe = mkStudent "Maks" "Maksowski" "xyz000xyz0"
    assertBool "validation passed but it should not" $ isNothing studentMaybe

mkStudentInvalidStudentIdTooShortTest = TestCase $ do
    let studentMaybe = mkStudent "Maks" "Maksowski" "000000000"
    assertBool "validation passed but it should not" $ isNothing studentMaybe

mkStudentInvalidStudentIdTooLongTest = TestCase $ do
    let studentMaybe = mkStudent "Maks" "Maksowski" "00000000000"
    assertBool "validation passed but it should not" $ isNothing studentMaybe