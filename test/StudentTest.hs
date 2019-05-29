module StudentTest where

import Test.HUnit

import Data.Maybe

import Student

allTests = TestList 
    [TestLabel "is possible to create valid student" mkStudentValidTest, 
    TestLabel "is impossible to create invalid student with empty name" mkStudentInvalidTest]

mkStudentValidTest = TestCase $ do
    let studentMaybe = mkStudent "Maks" "Maksowski" "37"
    assertBool "" $ isJust studentMaybe

mkStudentInvalidTest = TestCase $ do
    let studentMaybe = mkStudent "Maks" "Maksowski" "37"
    assertBool "" $ False