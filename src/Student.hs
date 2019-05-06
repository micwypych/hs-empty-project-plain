{-# LANGUAGE MultiParamTypeClasses #-}
module Student where

import Data.Text

data Student = Student FirstName LastName StudentId

newtype FirstName = FirstName Text
newtype LastName = LastName Text
newtype StudentId = StudentId Text

mkStudent :: Text -> Text -> Text -> Maybe Student
mkStudent firstName lastName studentId = 
    Student <$> validate firstName
            <*> validate lastName
            <*> validate studentId
 
class Validate fromType toType where
    validate :: fromType -> Maybe toType

instance Validate Text FirstName where
    validate firstName = Just $ FirstName firstName


instance Validate Text LastName where
    validate lastName = Just $ LastName lastName

instance Validate Text StudentId where
    validate studentId = Just $ StudentId studentId