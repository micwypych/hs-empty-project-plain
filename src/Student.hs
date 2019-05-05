module Student where

import Data.Text

data Studnet = Student FirstName LastName StudentId

newtype FirstName = FirstName Text
newtype LastName = LastName Text
newtype StudentId = StudentId Int