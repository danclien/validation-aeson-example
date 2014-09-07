{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo
  ( List
  , list    
  , Item
  , item
  , Person
  , person
  ) where

import Control.Applicative
import Data.Aeson

import Types

-- Todo list of items to finish
data List = List { title :: Text32 
                 , owner :: Person
                 , items :: [Item]
                 } deriving (Eq, Show)

-- Item on the todo list
data Item = Item { description :: Text32 
                 , completed :: Bool
                 } deriving (Eq, Show)

-- Person
data Person = Person { name :: Text64 
                     } deriving (Eq, Show)

-- Smart constructors
list :: AppValidation Text32 -> AppValidation Person -> AppValidation [Item] -> AppValidation List
list lTitle lOwner lItems = List <$> lTitle <*> lOwner <*> lItems

item :: AppValidation Text32 -> AppValidation Bool -> AppValidation Item
item iTitle iCompleted = Item <$> iTitle <*> iCompleted

person :: AppValidation Text64 -> AppValidation Person
person pName = Person <$> pName

-- aeson instances
instance FromJSON (AppValidation List) where
  parseJSON = withObject "List" $ \o ->
    list <$> 
    o .: "title" <*>
    o .: "owner" <*>
    o .: "items"

instance FromJSON (AppValidation Person) where
  parseJSON = withObject "Person" $ \o ->
    person <$> o .: "name"

instance FromJSON (AppValidation Item) where
  parseJSON = withObject "Item" $ \o ->
    item <$>
    o .: "description" <*>
    o .: "completed"

instance FromJSON (AppValidation [Item]) where
  parseJSON = withArraySequenceA "[Item]"
