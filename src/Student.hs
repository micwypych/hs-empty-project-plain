{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Student where

import Data.Text
import Data.Aeson
import Data.Attoparsec.Internal.Types

data Student = Student FirstName LastName StudentId
    deriving (Eq, Show, Read)

newtype FirstName = FirstName Text
    deriving (Eq, Show, Read)
newtype LastName = LastName Text
    deriving (Eq, Show, Read)
newtype StudentId = StudentId Text
    deriving (Eq, Show, Read)

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

instance ToJSON Student where
    -- this generates a Value
    toJSON (Student firstName lastName studentId) =
        object ["first_name" .= firstName, "last_name" .= lastName, "student_id" .= studentId]

    -- this encodes directly to a bytestring Builder
    toEncoding (Student firstName lastName studentId) =
        pairs ("first_name" .= firstName <> "last_name" .= lastName <> "student_id" .= studentId)

instance ToJSON FirstName where
    toJSON (FirstName txt) = toJSON txt

instance ToJSON LastName where
    toJSON (LastName txt) = toJSON txt

instance ToJSON StudentId where
    toJSON (StudentId txt) = toJSON txt
    

instance FromJSON Student where
    parseJSON = withObject "Student" $ \v -> Student
                       <$> v .: "first_name"
                       <*> v .: "last_name"
                       <*> v .: "student_id"

instance FromJSON FirstName where
    parseJSON = withText "FirstName" $ pure . FirstName

instance FromJSON LastName where
    parseJSON = withText "LastName" $ pure . LastName

instance FromJSON StudentId where
    parseJSON = withText "StudentId" $ pure . StudentId