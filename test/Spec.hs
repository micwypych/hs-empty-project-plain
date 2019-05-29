import System.Exit
import Test.HUnit

import qualified StudentRepositoryTest
import qualified StudentControllerTest
import qualified StudentTest

allTests = TestList [StudentRepositoryTest.allTests, StudentControllerTest.allTests, StudentTest.allTests] 

main :: IO ()
main = do 
    Counts _ _ errors' failures' <- runTestTT allTests
    if errors' + failures' > 0 
        then exitFailure 
        else exitSuccess